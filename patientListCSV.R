## ---------------------------
##
## Script name: patientListCSV.R
##
## Purpose of script: generate shortlinks from patient codes contained in data/idListIn.csv 
##
## Author: Dr. Roberts Klotins
##
## Date Created: 2020-10-11
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


### Libraries
library(tidyverse)
library(lubridate)
library(qrencoder)
library(stringi)
library(urlshorteneR)
library(purrr)

### Variables:

# User changeable variables:
# survey 1 number (every-session)
surveyId01="638xxx" # This is Halliwick Weekly Coping
# same for survey 2 (3-Monthly)
surveyId02="141xxx" # This is Borderline Symptom List
# survey 1 ID question code (the survey field accepting the patient code)
survey01QID="QXXXID"
# survey 2 ID question code
survey02QID="SXX36"

# Variables that do not need changing:
# survey link without the patient number paramter
surveyLinkBase="https://lime.psychinfo.org/"
dataDir <- "./data/"
# If Y then only those without treatment end date are processed. Default is Y
restrictToActive <- "Y"
# construct beginnings (bases) of both long links
link01common <- URLencode(paste0(surveyLinkBase,surveyId01,"?newtest=Y&",survey01QID,"="))
link02common <- URLencode(paste0(surveyLinkBase,surveyId02,"?newtest=Y&",survey02QID,"="))

### Functions:

getlongLink <- function(x,y){
   # combine long link base with the (patient) code 
   z <- URLencode(paste0(y,x))
   return(z)
}

getShortLink <- function(x){
   z <- isgd_LinksShorten(x)
   # if you want link without https:// part then use next line
   # z <- str_split(isgd_LinksShorten(x), '//',simplify=TRUE)[2]
   return(z)
}


### Reading in and pre-processing data

initData <- read.csv(paste0(dataDir,"idListIn.csv"))

initData$StartDate <- as.Date(initData$StartDate, origin = "1899-12-30")

#### Filtering variables
if (restrictToActive == "Y") {
  initData <- initData %>% filter(is.na(EndDate))
}


### Main point where links are inserted in data
initData <- initData %>% 
   mutate(longLink1 = map_chr(SHASUM,getlongLink,y=link01common))  %>% 
   mutate(longLink2 = map_chr(SHASUM,getlongLink,y=link02common))  %>% 
   mutate(ShortLink1 = map_chr(longLink1,getShortLink)) %>% 
   mutate(ShortLink2 = map_chr(longLink2,getShortLink))


### Write out data as CSV file
write.csv(initData,file=paste0("./output/","shortLinks_HPD_",format(Sys.time(),"%Y-%b-%d_%H-%M"),".csv"),na="")
