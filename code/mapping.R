#############################################################
#
#   Program: mapping.R
#   Project: IeDEA
# 
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Read in IeDEA standard and create maps
#
#   INPUT: "tblCENTER.csv"
#   OUTPUT: "tbl_query_yyyymmdd.csv"
#
#   Notes: As long as the working directory structure 
#          matches README.md, such that the tblCENTER,
#          R-code, and resources may be sourced, 
#          then this code should run smoothly, generating
#          a listing of data queries in /output.
#
#   Dependency: latticeExtra and cshapes (which loads addtl
#               dependencies)
#   Created: 15 May 2013
#   Revisions: 15 July 2013  -- format for GitHub
#              29 August 2014  -- re-format for GitHub 
#     
#############################################################
rm(list=ls()) # clear namespace

# set working directory to R code location using setwd() or RStudio->Session->Set Working Directory->To Source File Location

## Determine the root directory for iedea-des-base
rootdir <- gsub("/code","",getwd(),fixed=TRUE)


library(latticeExtra)
library(cshapes)

## DEFINE COLOR PALETTE
# library(RColorBrewer)
# colvec <- brewer.pal(7, "Set1")
colvec <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628")

## USER -- PLEASE REVISE or CHANGE THE APPROPRIATE WORKING DIRECTORY
# setwd("/home/blevinml/Projects/IeDEAS/qa-checks-r")


## READ IN tblCENTER 
center <- read.csv(paste0(rootdir,"/input/tblCENTER.csv"),header=TRUE,na.strings=c(""))
names(center) <- tolower(names(center))


## LATER MADE USE OF CSHAPES PACKAGE BECAUSE IT IS MOST CURRENT (2012)
cmap <- cshp(date=as.Date("2012-6-30"))

## SET OUR REGION DATA TOGETHER WITH THE SHP FILE
## THIS WOULD BE THE SECTION OF CODE TO INCLUDE COUNTRY 
## SPECIFIC INFORMATION IF AVAILABLE (e.g, HIV prevalence)
uniquecountry <- center[c("region","country")]
o <- match(cmap$ISO1AL3,uniquecountry$country)
uniquecountry <- uniquecountry[o,]
row.names(uniquecountry) <- uniquecountry$FEATUREID
countriesSPDF <- cmap
countriesSPDF@data <- cbind(uniquecountry,cmap@data)
colnum <- length(levels(center$region))

## SET SEED SO JITTER IS CONSISTENT 
set.seed(1)
centerpoints <- center[!is.na(center$geocode_lon),]
## I AM ADDING A .5 DEGREE JITTER BECAUSE SOME SITES ARE ON TOP OF EACH OTHER
centerpoints$geocode_latj <- jitter(centerpoints$geocode_lat,amount=0.5)
centerpoints$geocode_lonj <- jitter(centerpoints$geocode_lon,amount=0.5)
## SET THE LONG, LAT AS COORDINATES FOR PLOTTING
coordinates(centerpoints) = ~geocode_lonj+geocode_latj

## WRITE MAP FILES -- CREATE OUTPUT DIRECTORY (IF NEEDED)
if(!("output" %in% list.files(rootdir))){dir.create(file.path(rootdir,"output"))}

## MAP 1
png(paste0(rootdir,"/output/map1.png"),res=125,width=1200,height=600, bg="transparent") 
region_codes <- c("NA","CN","SA","EA","WA","CA","AP")
region_labels <- c("North America","CCASAnet","Southern Africa","East Africa","West Africa","Central Africa","Asia-Pacific") 
region_labels1 <- region_labels[match(levels(countriesSPDF$region),region_codes)]
spplot_sites = list("sp.points", centerpoints, pch=19, col="black",alpha=0.75)
p1 <- spplot(countriesSPDF,"region", col.regions=colvec[1:colnum], main="",sp.layout=list(spplot_sites),colorkey=FALSE)
p1 <- p1 + layer(panel.key(region_labels1, corner = c(.02,0.15), padding = 2,rectangles = TRUE, space="left",size=2, height=c(1,1), points = FALSE, lines = FALSE, packets = 1,cex=0.8))
print(update(p1, par.settings =  custom.theme(symbol = colvec[1:colnum], fill = colvec[1:colnum], lwd=1)))
dev.off()

## MAP 2
png(paste0(rootdir,"/output/map2.png"),res=125,width=1200,height=600, bg="transparent") 
spplot_adultped1 = list("sp.points", centerpoints[centerpoints$adultped=="ADULT" & !is.na(centerpoints$adultped),], pch=19, col=colvec[1], cex=0.7,alpha=0.75)
spplot_adultped2 = list("sp.points", centerpoints[centerpoints$adultped=="PED" & !is.na(centerpoints$adultped),], pch=19, col=colvec[2], cex=0.7,alpha=0.75)
spplot_adultped3 = list("sp.points", centerpoints[centerpoints$adultped=="BOTH" & !is.na(centerpoints$adultped),], pch=19, col=colvec[3], cex=0.7,alpha=0.75)
spplot_adultped4 = list("sp.points", centerpoints[is.na(centerpoints$adultped),], pch=19, col=colvec[6], cex=0.7,alpha=0.75)
keymap2 <- c("Adult only","Pediatrics only","Both")
colmap2 <- colvec[1:3]
## ONLY PRINT MISSING TO THE LEGEND IF THERE ARE SOME MISSING DATA
if(any(is.na(centerpoints$adultped))){
  colmap2 <- c(colmap2,colvec[6])
  keymap2 <- c(keymap2,"Missing data")
}
p2 <- spplot(countriesSPDF,"region", col.regions=rep(gray(.8),colnum), main="",sp.layout=list(spplot_adultped1,spplot_adultped2,spplot_adultped3,spplot_adultped4),colorkey=FALSE)
p2 <- p2 + layer(panel.key(keymap2, corner = c(.02,0.15), padding = 1.2,points = TRUE, space="right",size=2, lines = FALSE, packets = 1,cex=0.8))
print(update(p2, par.settings =  custom.theme(symbol = colmap2, pch=19)))
dev.off()

## MAP 3
png(paste0(rootdir,"/output/map3.png"),res=125,width=1200,height=600, bg="transparent") 
spplot_rural1 = list("sp.points", centerpoints[centerpoints$rural==1 & !is.na(centerpoints$rural),], pch=19, col=colvec[1],alpha=0.75)
spplot_rural2 = list("sp.points", centerpoints[centerpoints$rural==2 & !is.na(centerpoints$rural),], pch=19, col=colvec[2],alpha=0.75)
spplot_rural3 = list("sp.points", centerpoints[centerpoints$rural==3 & !is.na(centerpoints$rural),], pch=19, col=colvec[3],alpha=0.75)
spplot_rural4 = list("sp.points", centerpoints[centerpoints$rural==4 & !is.na(centerpoints$rural),], pch=19, col=colvec[4],alpha=0.75)
spplot_rural5 = list("sp.points", centerpoints[is.na(centerpoints$rural),], pch=19, col=colvec[6],alpha=0.75)
keymap3 <- c("Urban","Mostly urban","Mostly rural","Rural")
colmap3 <- colvec[1:4]
## ONLY PRINT MISSING TO THE LEGEND IF THERE ARE SOME MISSING DATA
if(any(is.na(centerpoints$rural))){
  colmap3 <- c(colmap3,colvec[6])
  keymap3 <- c(keymap3,"Missing data")
}
p3 <- spplot(countriesSPDF,"region", col.regions=rep(gray(.8),colnum), main="",sp.layout=list(spplot_rural1,spplot_rural2,spplot_rural3,spplot_rural4,spplot_rural5),colorkey=FALSE)
p3 <- p3 + layer(panel.key(keymap3, corner = c(.02,0.15), padding = 1.2,points = TRUE, space="right",size=2, lines = FALSE, packets = 1,cex=0.8))
print(update(p3, par.settings =  custom.theme(symbol = colmap3, pch=19)))
dev.off()

## MAP 4
png(paste0(rootdir,"/output/map4.png"),res=125,width=1200,height=600, bg="transparent") 
spplot_level1 = list("sp.points", centerpoints[centerpoints$level==1 & !is.na(centerpoints$level),], pch=19, col=colvec[1],alpha=0.75)
spplot_level2 = list("sp.points", centerpoints[centerpoints$level==2 & !is.na(centerpoints$level),], pch=19, col=colvec[2],alpha=0.75)
spplot_level3 = list("sp.points", centerpoints[centerpoints$level==3 & !is.na(centerpoints$level),], pch=19, col=colvec[3],alpha=0.75)
spplot_level4 = list("sp.points", centerpoints[is.na(centerpoints$level),], pch=19, col=colvec[6],alpha=0.75)
keymap4 <- c("Health center","District hospital","Regional, provincial or \n university hospital")
colmap4 <- colvec[1:3]
## ONLY PRINT MISSING TO THE LEGEND IF THERE ARE SOME MISSING DATA
if(any(is.na(centerpoints$rural))){
  colmap4 <- c(colmap4,colvec[6])
  keymap4 <- c(keymap4,"Missing data")
}
p4 <- spplot(countriesSPDF,"region", col.regions=rep(gray(.8),colnum), main="",sp.layout=list(spplot_level1,spplot_level2,spplot_level3,spplot_level4),colorkey=FALSE)
p4 <- p4 + layer(panel.key(keymap4, corner = c(.02,0.15), padding = 1.2,points = TRUE, space="right",size=2, lines = FALSE, packets = 1,cex=0.8))
print(update(p4, par.settings =  custom.theme(symbol = colmap4, pch=19)))
dev.off()


## COLLATE MAPS IN HTML FILE
library(brew)
brew(file='mapping.brew',output=paste0(paste0(rootdir,"/output/mapping_report.html")))

