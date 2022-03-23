library(rvest)
library(jsonlite)
library(googlesheets4)
sheetid<-"1S3_Y1-jti8ofVnPumryFjnZr5wo9GrSMCheYj-2IH68"
touslieux<-"locations"
couleurs<-"points"
# designate project-specific cache
options(gargle_oauth_cache = ".secrets")
# check the value of the option, if you like
gargle::gargle_oauth_cache()
# trigger auth on purpose to store a token in the specified cache
# a broswer will be opened
googlesheets4::sheets_auth()
