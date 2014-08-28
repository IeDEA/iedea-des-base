#############################################################
#
# Program: read_data.R
#
# Project: IeDEA
#
# Biostatistician/Programmer: Meridith Blevins, MS
#
# Purpose: This R program is intended to load data from 
#        IeDEA-DES into R.
#
# Notes: rootdir must be specified in all programs calling
#        read_data.R
# Created: 28 August 2014
# Revisions:
#
#############################################################

if(!exists("rootdir")) stop("the variable 'rootdir' must be specified in all programs calling read_data.R")


## IDENTIFY WHICH TABLES TO EXPECT FROM DES
expectedtables <- c("center","program","basic","ltfu","cd4","rna","art","dis","visit")
expecteddestables <- c("tblCENTER","tblPROGRAM","tblBAS","tblLTFU","tblLAB_CD4","tblLAB_RNA","tblART","tblDIS","tblVIS")
## CHOOSE FIRST SELECTS THE TEXT STRING OCCURING BEFORE THE SPECIFIED SEPARATER
choosefirst <- function(var,sep=".") unlist(lapply(strsplit(var,sep,fixed=TRUE),function(x) x[1]))
## DETERMINE WHICH TABLES EXIST IN '/input'
existingtables <- choosefirst(list.files(paste0(rootdir,"/input")))
readtables <- expectedtables[match(existingtables,expecteddestables)]
## READ IN ALL EXISTING TABLES
for(i in 1:length(readtables)){
  if(!is.na(readtables[i])){
     readcsv <- read.csv(paste0(rootdir,"/input/",existingtables[i],".csv"),header=TRUE,stringsAsFactors = FALSE,na.strings=c(NA,""))
     names(readcsv) <- tolower(names(readcsv))
     assign(readtables[i],readcsv)
   }
}
