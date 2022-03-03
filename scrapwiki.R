library(tidyverse)
library(rvest)
library(jsonlite)
library(aws.s3)

AWSDF<-data.frame(
  stringsAsFactors = FALSE,
  Access.key.ID =  Sys.getenv("AKI"),
  Secret.access.key = Sys.getenv("SAK")
)
Sys.setenv("AWS_ACCESS_KEY_ID" = AWSDF$Access.key.ID[1],
           "AWS_SECRET_ACCESS_KEY" = AWSDF$Secret.access.key[1],
           "AWS_DEFAULT_REGION" = "eu-west-1")

date<-gsub("-","",Sys.Date())
page<-"https://en.wikipedia.org/w/index.php?title=Module:Russo-Ukrainian_War_detailed_map&action=history"
pagehtml<-read_html(page)
listedeshref<-pagehtml%>%html_nodes("a")%>%html_attr("href")
justebonhref<-listedeshref[grepl("detailed_map&oldid=",listedeshref)][1]
idversionchoisie<-substr(justebonhref,66,nchar(justebonhref))


sortLeJson<-function(date,idversionchoisie){
  urllast<-paste0("https://en.wikipedia.org/w/index.php?title=Module:Russo-Ukrainian_War_detailed_map&oldid=",idversionchoisie)
  pagehtml<-read_html(urllast)
  pagehtmltxt<-pagehtml%>%html_text()
  debutcode<-str_locate(string=pagehtmltxt,pattern = "secondaryModules")
  fincode<-str_locate(string=pagehtmltxt,pattern = "containerArgs")
  texteselectionne<-substr(pagehtmltxt,debutcode[2],fincode[1])
  premierelat<-str_locate(string=texteselectionne,pattern = "\\{ lat =")
  toutesleslignes<-str_locate_all(texteselectionne,"\\},\\\n\\}")
  textesansbackslach<-substr(texteselectionne,premierelat[1],toutesleslignes[[1]][1])
  textesansbackslach<-gsub("position = ","\'position\' :",(
    gsub("link = ","\'link\' :",(gsub("label_size = ","\'label_size\' :",(
      gsub("marksize = ","\'marksize\' :",(
        gsub("label = ","\'label\' :",(
          gsub("mark = ","\'mark\' :",(
            gsub("long = ","\'long\' :",(
              gsub("lat = ","\'lat\' :",textesansbackslach)))))))))))))))
  jsoncars<-textesansbackslach%>%jsonlite::toJSON(pretty = TRUE)
  fileConn<-file(paste0(date,"_",idversionchoisie,".json"))
  writeLines(jsoncars,fileConn)
  close(fileConn)
  
  aws.s3::put_object(file=paste0(date,"_",idversionchoisie,".json"), bucket = "dataviz-r-files/ukrus")
}


sortLeJson(date,idversionchoisie)
