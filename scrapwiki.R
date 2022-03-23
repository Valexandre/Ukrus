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
    gsub("link = ","\'link\' :",(gsub("label_size =","\'label_size\' :",(
      gsub("marksize = ","\'marksize\' :",(
        gsub("label = ","\'label\' :",(
          gsub("mark = ","\'mark\' :",(
            gsub("long = ","\'long\' :",(
              gsub("lat = ","\'lat\' :",textesansbackslach)))))))))))))))
  textesansbackslach<-gsub("(--[A-z\\s]+)","",textesansbackslach)
  textesansbackslach<-gsub("lable_size","label_size",textesansbackslach)
  textesansbackslach<-gsub("Oblast:","",textesansbackslach)
  textesansbackslach<-gsub("Oblast","",textesansbackslach)
  textesansbackslach<-gsub("\\[\\[Port of Mariupol\\]\\]","'[[Port of Mariupol]]'",textesansbackslach)
  textesansbackslach<-gsub("\\[\\[Port Krym\\]\\]","'[[Port Krym]]'",textesansbackslach)
  textesansbackslach<-gsub("city","",gsub("City","",gsub("-Frankivsk","",textesansbackslach)))
  textesansbackslach<-gsub('\\["','\\[',textesansbackslach)
  textesansbackslach<-gsub('"\\]','\\]',textesansbackslach)
  textesansbackslach<-gsub("\",\\s\\},","\"\\},",textesansbackslach)
  textesansbackslach<-gsub("label_size=",'"label_size" :',textesansbackslach)
  textesansbackslach<-gsub("label_size =",'"label_size" :',textesansbackslach)
  textesansbackslach<-gsub('label_size :','"label_size" :',textesansbackslach)
  textesansbackslach<-gsub(", :",'',textesansbackslach)
  textesansbackslach<-gsub("\\n",'',textesansbackslach)
  textesansbackslach<-gsub("\\t",'',textesansbackslach)
  textesansbackslach<-gsub('\\"','"',textesansbackslach, fixed = TRUE)
  textesansbackslach<-gsub("'",'"',textesansbackslach)
  textesansbackslach<-gsub('\\["\\{','\\[\\{',textesansbackslach)
  jsoncars<-gsub('\\}"\\]','\\}\\]',gsub('\\["\\{','\\[\\{',textesansbackslach%>%jsonlite::toJSON(pretty = TRUE)))
  #On écrit la base des lieux
  RAWDATA<-jsonlite::fromJSON(gsub('\\\\"','"',as.character(jsoncars))) %>% 
    tibble::as_tibble()%>%
    mutate(lat=format(round(as.numeric(lat), 3), nsmall = 3),
           lon=format(round(as.numeric(long), 3), nsmall = 3))%>%
    mutate(visible="TRUE",z=99,
           lat=str_pad(lat,width = 6,side = "right",pad = 0),
           lon=str_pad(lon,width = 6,side = "right",pad = 0),
           id=paste0(lon,"_",lat))
    
  #Quels Points sont déjà présents dans la feuille ?
  DejaPresents<-read_csv("csv_ukr/locations.csv", col_types = cols(lat = col_character(), 
                                                  lon = col_character()))
  #DejaPresents<-read_sheet(ss = sheetid,sheet=touslieux)
  
  PointsAEcrire<-RAWDATA%>%group_by(id)%>%
    filter(marksize==min(marksize))%>%
    filter(id%!in%DejaPresents$id)
  
  Tout<-rbind(DejaPresents%>%select(id,label,marksize,visible,z,lat,lon),PointsAEcrire%>%select(id,label,marksize,visible,z,lat,lon))
  
  write.csv(Tout,"csv_ukr/locations.csv",row.names=F)
  
  # googlesheets4::sheet_append(PointsAEcrire%>%select(id,label,marksize,visible,z,lat,lon),
  #                            ss = sheetid,sheet=touslieux)
  
  ###########
  #Pour savoir quelles couleurs indiquer aux points
  #Si deux points ont la même id, on met en jaune et on garde le marksize le plus petit.
  ListeDesDoublePoints<-RAWDATA%>%group_by(id)%>%mutate(Nombre=n(),row=row_number())%>%
    filter(Nombre>1)%>%
    filter(marksize==min(marksize) & row==min(row))%>%
    mutate(color="#FACC34",
           date=date)%>%dplyr::select(id,color,date)
  ListeConflits<-RAWDATA%>%
    filter(mark=="80x80-red-blue-anim.gif")%>%
    filter(id%!in%ListeDesDoublePoints$id)%>%
    mutate(color="#FACC34",
           date=date)%>%dplyr::select(id,color,date)
  Kiev<-RAWDATA%>%
    filter(mark=="Battle of Kyiv (2022) template.svg")%>%
    mutate(color="#FACC34",
           date=date)%>%dplyr::select(id,color,date)
  #S'il y a blue, on met en bleu,
  ListeBleu<-RAWDATA%>%filter(grepl("blue",mark))%>%
    filter(mark%!in%c("80x80-red-blue-anim.gif"))%>%
    filter(id%!in%ListeDesDoublePoints$id)%>%
    mutate(color="#1ea0e6",
           date=date)%>%dplyr::select(id,color,date)
  #S'il y a red, on met en rouge.
  ListeRouge<-RAWDATA%>%filter(grepl("red",mark))%>%
    filter(mark%!in%c("80x80-red-blue-anim.gif"))%>%
    filter(id%!in%ListeDesDoublePoints$id)%>%
    mutate(color="#F03333",
           date=date)%>%dplyr::select(id,color,date)
  #S'il y a trève, on met en violet
  ListeTreve<-RAWDATA%>%filter(grepl("Purple",mark))%>%
    mutate(color="#8A51D2",
           date=date)%>%dplyr::select(id,color,date)
  AjoutDuJour<-rbind(ListeBleu,ListeConflits,ListeDesDoublePoints,ListeRouge,ListeTreve,Kiev)
  AjoutDuJour<-AjoutDuJour%>%left_join(Tout%>%select(id,label))
  # googlesheets4::sheet_append(AjoutDuJour%>%select(id,color,date,label),
  #                             ss = sheetid,sheet=couleurs)
  #ToutesJournees<-read_sheet(ss = sheetid,sheet=couleurs)
  #googlesheets4::write_sheet(ToutesJournees%>%select(id,color,date,label)%>%
  #                             arrange(label,date),
  #                            ss = sheetid,sheet=couleurs)
  ToutesJournees<-read_csv("csv_ukr/points.csv", col_types = cols(date = col_character()))
  ToutesJournees<-rbind(ToutesJournees%>%select(id,color,date,label),
                        AjoutDuJour%>%select(id,color,date,label))%>%
    arrange(label,date)
  write.csv(Tout,"csv_ukr/points.csv",row.names=F) 
  
  jsoncars<-textesansbackslach%>%jsonlite::toJSON(pretty = TRUE)
  fileConn<-file(paste0("data/",date,"_",idversionchoisie,".json"))
  writeLines(jsoncars,fileConn)
  close(fileConn)
  aws.s3::put_object(file=paste0("data/",date,"_",idversionchoisie,".json"), bucket = "dataviz-r-files/ukrus")
}


sortLeJson(date,idversionchoisie)
