#############################################################
#
#   Program: computed_variables.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, MD, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Write some standard functions for computing
#            variables routinely needed for medical research.
#
#   INPUT: 
#   OUTPUT: VARIABLE(_TYPE)_CMP attached to tblXXX_CMP.csv
#
#   Notes: 
#
#   Created: 29 August 2014
#   Revisions: 
#     
#############################################################
# set working directory to R code location using setwd() or RStudio->Session->Set Working Directory->To Source File Location

## Determine the root directory for iedea-des-base
rootdir <- gsub("/code","",getwd(),fixed=TRUE)

## READ UTILITY_FUNCTIONS.R
source("code/utility_functions.R")
## READ IeDEA-DES FORMATTED DATA
source("code/read_data.R")


## DUPLICATE VISIT DATES ARE A PROBLEM, SO REMOVE THEM IF THEY EXIST
## NOTE: These will be listed as data exceptions in the QA checks
dups <- unsplit(lapply(split(visit$vis_d, visit$patient), FUN=duplicated), visit$patient)
visit <- visit[!dups,]

## DETERMINE WHICH CENTER THE PATIENT VISITED FIRST, LAST, AND MOST OFTEN 
center_first <- getselectvar(id=patient,var=center,date=vis_d,type="first",data=visit,returndate=FALSE,dateformat="%Y-%m-%d")
center_last <- getselectvar(id=patient,var=center,date=vis_d,type="last",data=visit,returndate=FALSE,dateformat="%Y-%m-%d")
center_most <- getselectvar(date=vis_d,id=patient,var=center,type="most",data=visit,returndate=FALSE,dateformat="%Y-%m-%d")

## DETERMINE THE FIRST DATE OF ART RECEIPT FROM TBLART
art_first <- getselectdate(art_sd,patient,data=art,dateformat="%Y-%m-%d") # default type="first"

## MERGE COMPUTED VARIABLES WITH tblBAS
## NOTE: THIS IS NOT CBIND() IN CASE A PATIENT IN BASIC DOES NOT HAVE A RECORD IN VISIT, AND IN CASE THEY ARE NOT IDENTICALLY SORTED
basic_cmp <- merge(merge(merge(merge(basic,center_first,all.x=TRUE),center_last,all.x=TRUE),center_most,all.x=TRUE),art_first,all.x=TRUE)

## WRITE TABLE FILES THAT HAVE COMPUTED VARIABLES -- CREATE OUTPUT DIRECTORY (IF NEEDED)
if(!("output" %in% list.files(rootdir))){dir.create(file.path(rootdir,"output"))}
write.csv(basic_cmp,paste0(rootdir,"/output/tblBAS_CMP.csv"),row.names=FALSE)

