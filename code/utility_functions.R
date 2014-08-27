#############################################################
#
#   Program: utility_functions.R
#   Project: IeDEA
# 
#   PI: Firas Wehbe, MD, PhD
#   Biostatistician/Programmer: Meridith Blevins, MS
#   Purpose: Write some standard functions for deriving
#            variables routinely needed for medical research.
#            Designed specifically to reduce dependencies
#            on existing R packages.
#
#   INPUT: 
#   OUTPUT: standard R functions
#
#     
#############################################################

#' convertdate
#'
#' summary takes IeDEA-DES character string(s) and outputs date objects
#'
#' details
#'
#' @param date is an object or variable name of character string(s) that needs to be converted to an date object.  Must follow IeDEA-DES (yyyy-mm-dd).
#' @param table (optional) specifies which table to read date from if date is not itself an object.  
#' @export

convertdate <- function(date,table=parent.frame()){
  if(exists(deparse(substitute(date)),table)){
    var <- as.Date(get(deparse(substitute(date)),table),"%Y-%m-%d")
  }
  if(!exists(deparse(substitute(date)),table)){
    var <- NULL
  }
  return(var)
}

#' getbaseline
#'
#' summary This utility function provides a means for indetifying one date and optionally value among a series of dates (and values) occuring closest/latest/earliest within a window of another date for a group of subjects.  A popular usage would be to determine the date of CD4 collection and CD4 count closest to HAART initiation.  
#'
#' details
#'
#' @param baselinedate date of interest occurring only once per subject on which to determine the closest/latest/earliest date/value.
#' @param visitdate one or more dates per subject that will be distilled into relevant date occurring within the specified window and type.  Duplicate visitdate within ids will cause the function to stop.  
#' @param id contains or indicates the unique id per subject
#' @param value one or more values per subject with corresponding visitdate that will be distilled into relevant date/value occurring within the specified window and type.
#' @param before beginning of window specifying the number of days allowable after the baseline date of interest.  Defaults to 30 days.
#' @param after end of window specifying the number of days allowable after the baseline date of interest.  Defaults to 30 days.
#' @param type determines where in the window to calculate the date.  Options include: "closest" (default), "earliest", "latest".
#' @param data data frame to use. If omitted, the parent environment is assumed to contain the variables.
#' @param returndate optional bolean to indicate whether or not to return the dates corresponding to the relevant value. Default to TRUE.
#' @param dateformat optonal string to indicate format of date variables if they are not already date objects (e.g. "%Y-%m-%d") 
#' @param subset logical expression defining a subset of observations to analyze
#' @export

getbaseline <- function(baselinedate,visitdate,id,value=value,before=30,after=30,type="closest",data=parent.frame(),returndate=TRUE,dateformat=dateformat,subset=subset){
   ## get appropriate variables from data frame if provided
    if(!missing(data)){
        bdate <- get(deparse(substitute(baselinedate)),data)
        vdate <- get(deparse(substitute(visitdate)),data)
        ids <- get(deparse(substitute(id)),data)
        if(!missing(value)) values <- get(deparse(substitute(value)),data)
    }
    if(!missing(dateformat)){
        vdate <- as.Date(vdate,dateformat)
	bdate <- as.Date(bdate,dateformat)
    }
  ## check variable lengths
   if(!missing(value) & length(values)!=length(ids))  stop("values is not the appropriate length")
   if(length(vdate)!=length(ids))  stop("vdate is not the appropriate length")
   if(length(bdate)!=length(ids))  stop("bdate is not the appropriate length")
   if(!missing(subset)){
       if(length(subset) != length(ids)) stop("subset is not the appropriate length")
       if(!is.logical(subset)) stop("subset should be a logical argument")
       vdate <- vdate[subset]
       bdate <- bdate[subset]
       ids <- ids[subset]
       if(!missing(value)) values <- values[subset]
   }
  ## check for duplicates by visitdate
    dups <- unsplit(lapply(split(vdate, ids), FUN=anyDuplicated), ids)
    dupids <- sort(unique(ids[dups>0]))
    if(length(dupids)>0){
         warning(paste("visitdate is duplicated for",length(dupids),"id's; returning list of duplicate id."))
         return(duplicateids = dupids)
    }
  ## now extract the date and value (optional) associated with the window (before/after) and type (closest/earliest/latest)
    originalid <- ids
    diff <- vdate - bdate
    window <- diff >= before*(-1) & diff <= after
    ids <- ids[window]; vdate <- vdate[window]
    diff <- diff[window]
    if(!missing(value)) values <- values[window]
    if(type=="closest") keep1 <- unsplit(lapply(split(diff, ids), FUN=function(x) c(min(abs(x)))), ids)
    if(type=="earliest") keep1 <- unsplit(lapply(split(diff, ids), FUN=function(x) c(min(x))), ids)
    if(type=="latest") keep1 <- unsplit(lapply(split(diff, ids), FUN=function(x) c(max(x))), ids)
    if(!missing(value)) {
        if(returndate){
            baselinevalues <- data.frame(ids,vdate,values)[keep1==diff & !is.na(values),]
            names(baselinevalues) <- c(deparse(substitute(id)),paste(deparse(substitute(value)),"_d",sep=""),deparse(substitute(value)))
        }
        if(!returndate){
            baselinevalues <- data.frame(ids,values)[keep1==diff & !is.na(values),]
            names(baselinevalues) <- c(deparse(substitute(id)),deparse(substitute(value)))
        }
    }
    if(missing(value)) {
        baselinevalues <- data.frame(ids,vdate)[keep1==diff,]
        names(baselinevalues) <- c(deparse(substitute(id)),deparse(substitute(visitdate)))
    }
    return(baselinevalues)
}

#' getselectdate
#'
#' summary
#'
#' details
#'
#' @param date one or more dates per subject that will be distilled into relevant date according to type. 
#' @param id contains or indicates the unique id per subject
#' @param type determines which date to return.  Options include: "first" (default) and "last".
#' @param data data frame to use. If omitted, the parent environment is assumed to contain the variables.
#' @param dateformat optonal string to indicate format of date variables if they are not already date objects (e.g. "%Y-%m-%d") 
#' @export

getselectdate <- function(date,id,type="first",data=data,dateformat=dateformat){
    ## get appropriate variables from data frame if provided
    if(!missing(data)){
        dates <- get(deparse(substitute(date)),data)
        ids <- get(deparse(substitute(id)),data)
    }
    if(!missing(dateformat)){
        dates <- as.Date(dates,dateformat)
    }
    if(type=="first") keep1 <- unsplit(lapply(split(dates, ids), FUN=function(x) min(x)), ids)
    if(type=="last") keep1 <- unsplit(lapply(split(dates, ids), FUN=function(x) max(x)), ids)
    selectdate <- unique(data.frame(ids,keep1))
    names(selectdate) <- c(deparse(substitute(id)),deparse(substitute(date)))
    return(selectdate)
}

#' getnadirvalue
#'
#' summary This utility function determines the lowest occurring value (nadir value) of a series of values within a subject returning that value and optionally the corresponding date for each subject. 
#'
#' details
#'
#' @param value one or more values per subject with optional corresponding visitdate that will be distilled into nadir value/date.
#' @param id contains or indicates the unique id per subject
#' @param date optionally specify one or more dates per subject corresponding to value that will be returned (if prompted) corresponding to the nadir value. 
#' @param data data frame to use. If omitted, the parent environment is assumed to contain the variables.
#' @param returndate optional bolean to indicate whether or not to return the dates corresponding to the nadir value. Default to TRUE.
#' @param dateformat optonal string to indicate format of date variables if they are not already date objects (e.g. "%Y-%m-%d") 
#' @export

getnadirvalue <- function(value,id,date=date,data=data,returndate=TRUE,dateformat=dateformat){
    ## get appropriate variables from data frame if provided
    if(!missing(data)){
        ids <- get(deparse(substitute(id)),data)
        values <- get(deparse(substitute(value)),data)
        if(!missing(date)) dates <- get(deparse(substitute(date)),data)
    }
    if(!missing(dateformat) & !missing(date)){
        dates <- as.Date(dates,dateformat)
    }
    keep1 <- unsplit(lapply(split(values, ids), FUN=function(x) min(x)), ids)
    if(missing(date)){
        nadirvalue <- unique(data.frame(ids,keep1))
        names(nadirvalue) <- c(deparse(substitute(id)),deparse(substitute(value)))
    }
    if(!missing(date)){
      if(returndate){
          nadirvalue <- data.frame(ids,keep1,dates)[keep1==values,]
	        nadirvalue <- nadirvalue[!duplicated(nadirvalue$ids),]
          names(nadirvalue) <- c(deparse(substitute(id)),deparse(substitute(value)),deparse(substitute(date)))
      }
      if(returndate){
        nadirvalue <- data.frame(ids,keep1)[keep1==values,]
        nadirvalue <- nadirvalue[!duplicated(nadirvalue$ids),]
        names(nadirvalue) <- c(deparse(substitute(id)),deparse(substitute(value)))
      }      
    }
    return(nadirvalue)
}


