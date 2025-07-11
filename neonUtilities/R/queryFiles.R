##############################################################################################
#' @title Get a list of data files from the query endpoint of the NEON API

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description Uses the query endpoint of the NEON API to find the full list of files for a given data product, release, site(s), and date range.
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param package Either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param release The data release to be downloaded; either 'current' or the name of a release, e.g. 'RELEASE-2021'. 'current' returns the most recent release, as well as provisional data if include.provisional is set to TRUE. To download only provisional data, use release='PROVISIONAL'. Defaults to 'current'.
#' @param timeIndex Either the string 'all', or the time index of data to download, in minutes. Only applicable to sensor (IS) data. Defaults to 'all'.
#' @param tabl Either the string 'all', or the name of a single data table to download. Defaults to 'all'.
#' @param metadata T or F, should urls for metadata files (variables, sensor positions, etc) be included. Defaults to F, can only be set to T if tabl is not 'all'.
#' @param include.provisional T or F, should provisional data be included in downloaded files? Defaults to F. See https://www.neonscience.org/data-samples/data-management/data-revisions-releases for details on the difference between provisional and released data.
#' @param token User specific API token (generated within data.neonscience.org user accounts). Optional.
#' 
#' @return A list of two elements: (1) the set of urls matching the query; (2) the most recent variables file for the set of urls

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2025-03-04 Claire Lunch: Original creation
#   
##############################################################################################

queryFiles <- function(dpID, site="all", startdate=NA, enddate=NA,
                       package="basic", release="current",
                       timeIndex="all", tabl="all", metadata=TRUE,
                       include.provisional=FALSE, token=NA_character_) {
  
  # if token is an empty string, set to NA
  if(identical(token, "")) {
    token <- NA_character_
  }
  
  # check for expiration
  token <- tokenCheck(token)
  
  # check for GCS and S3 enabled
  if(!arrow::arrow_with_gcs()) {
    if(!arrow::arrow_with_s3()) {
      stop("Package arrow is installed with S3 and GCS disabled and cannot access NEON data. Consult documentation at https://arrow.apache.org/docs/r/articles/fs.html , update installation and re-try.")
    } else {
      message("Package arrow is installed with GCS disabled. S3 will be used to access data; performance may be reduced. To enable GCS consult documentation at https://arrow.apache.org/docs/r/articles/fs.html ")
    }
  }
  
  # first query products endpoint to get availability info
  if(release=="current" | release=="PROVISIONAL") {
    prod.req <- getAPI(apiURL = paste("https://data.neonscience.org/api/v0/products/", 
                                      dpID, sep=""), token = token)
  } else {
    prod.req <- getAPI(apiURL = paste("https://data.neonscience.org/api/v0/products/", 
                                      dpID, "?release=", release, sep=""), token = token)
  }

  if(is.null(prod.req)) {
    message(paste("No data found for data product ", dpID, " and release ", release, sep=""))
    return(invisible())
  }
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)

  # set up url for available sites
  siteset <- avail$data$siteCodes$siteCode
  if(!identical(site, "all")) {
    siteset <- base::intersect(site, siteset)
  }
  if(length(siteset)==0) {
    message("No data found at the selected site(s).")
    return(invisible())
  }
  siteurl <- paste("&siteCode=", siteset, sep="", collapse="")
  
  # set up dates for date range
  minDate <- min(unique(unlist(avail$data$siteCodes$availableMonths)))
  maxDate <- max(unique(unlist(avail$data$siteCodes$availableMonths)))
  
  if(is.na(startdate)) {
    startdate <- minDate
  } else {
    if(minDate>startdate) {
      startdate <- minDate
    }
  }
  if(is.na(enddate)) {
    enddate <- maxDate
  } else {
    if(maxDate<enddate) {
      enddate <- maxDate
    }
  }
  dateurl <- paste("&startDateMonth=", startdate, "&endDateMonth=", enddate, sep="")
  
  # url for releases and include provisional
  if(include.provisional) {
    ipurl <- "&includeProvisional=true"
  } else {
    ipurl <- "&includeProvisional=false"
  }
  
  if(release=="current" | release=="PROVISIONAL") {
    relurl <- ""
  } else {
    relurl <- paste("&release=", release, sep="")
  }
  
  # check package value
  if(!package %in% c("basic", "expanded")) {
    message("Data package must be basic or expanded.")
    return(invisible())
  }
  if(package=="expanded" & isFALSE(avail$data$productHasExpanded)) {
    message("No expanded package available. Basic package downloaded instead.")
    package <- "basic"
  }
  
  # construct full query url and run query
  qurl <- paste("https://data.neonscience.org/api/v0/data/query?productCode=",
                dpID, siteurl, dateurl, ipurl, "&package=", package, relurl, sep="")
  qreq <- getAPI(apiURL=qurl, token=token)
  
  if(is.null(qreq)) {
    message("No API response for selected query. Check inputs.")
    return(invisible())
  }
  if(qreq$status_code!=200) {
    message(paste("Query failed with status code ", qreq$status_code, 
                  ". Check inputs.", sep=""))
    return(invisible())
  }
  
  # get file list from response
  qall <- jsonlite::fromJSON(httr::content(qreq, as='text', encoding='UTF-8'), 
                             simplifyDataFrame=TRUE, flatten=TRUE)
  relall <- qall$data$releases
  
  # if only PROVISIONAL requested, check for provisional data and subset
  if(release=="PROVISIONAL") {
    if(!"PROVISIONAL" %in% relall$release) {
      message("PROVISIONAL data requested but no provisional data found in query parameters.")
      return(invisible())
    } else {
      relall <- relall[which(relall$release=="PROVISIONAL"),]
    }
  }
  
  # get list of files for each site-month
  names(relall$packages) <- relall$release
  packall <- relall$packages
  fllst <- list()
  for(i in 1:length(packall)) {
    fltmp <- packall[[i]]$files
    names(fltmp) <- paste(names(packall)[i], packall[[i]]$siteCode, 
                          packall[[i]]$month, sep=".")
    fllst <- c(fllst, fltmp)
  }
  
  # subset by time index or table, if relevant
  # make data frame of urls, checksums for each file, and checksum of corresponding variables file
  flset <- character()
  relnames <- strsplit(names(fllst), split='.', fixed=TRUE)
  if(timeIndex=="all" & tabl=="all") {
    for(j in 1:length(fllst)) {
      flsetk <- cbind(fllst[[j]]$url, 
                      fllst[[j]]$md5,
                      rep(ifelse(length(grep(pattern="variables", 
                                             x=fllst[[j]]$url))==1,
                                 fllst[[j]]$md5[grep(pattern="variables", 
                                                     x=fllst[[j]]$url)], 
                                 NA_character_),
                          length(fllst[[j]]$url)),
                      rep(ifelse(length(grep(pattern="variables", 
                                             x=fllst[[j]]$url))==1,
                                 fllst[[j]]$url[grep(pattern="variables", 
                                                     x=fllst[[j]]$url)], 
                                 NA_character_),
                          length(fllst[[j]]$url)),
                      rep(relnames[[j]][1], length(fllst[[j]]$url)))
      flset <- rbind(flset, flsetk)
    }
    
  } else {
    if(timeIndex!="all") {
      tiInd <- lapply(fllst, FUN=function(x) {
        base::grep(pattern=paste(timeIndex, "min|", timeIndex, 
                                 "_min|science_review_flags|variables|readme|sensor_positions|categoricalCodes", 
                                 sep=""),
                   x=x$url)
      })
      for(k in 1:length(tiInd)) {
        kInd <- tiInd[[k]]
        flsetk <- cbind(fllst[[k]]$url[kInd], 
                        fllst[[k]]$md5[kInd],
                        rep(ifelse(length(grep(pattern="variables", 
                                               x=fllst[[k]]$url))==1,
                                   fllst[[k]]$md5[grep(pattern="variables", 
                                                       x=fllst[[k]]$url)], 
                                   NA_character_),
                            length(fllst[[k]]$url[kInd])),
                        rep(ifelse(length(grep(pattern="variables", 
                                               x=fllst[[k]]$url))==1,
                                   fllst[[k]]$url[grep(pattern="variables", 
                                                       x=fllst[[k]]$url)], 
                                   NA_character_),
                            length(fllst[[k]]$url[kInd])),
                        rep(relnames[[k]][1], length(fllst[[k]]$url[kInd])))
        flset <- rbind(flset, flsetk)
      }
    }
    
    if(tabl!="all") {
      tabInd <- lapply(fllst, FUN=function(x) {
        base::grep(pattern=paste("[.]", tabl, "[.]|variables|readme|sensor_positions|categoricalCodes", 
                                 sep=""),
                   x=x$url)
      })
      for(k in 1:length(tabInd)) {
        kInd <- tabInd[[k]]
        flsetk <- cbind(fllst[[k]]$url[kInd], 
                        fllst[[k]]$md5[kInd],
                        rep(ifelse(length(grep(pattern="variables", 
                                               x=fllst[[k]]$url))==1,
                                   fllst[[k]]$md5[grep(pattern="variables", 
                                                       x=fllst[[k]]$url)], 
                                   NA_character_),
                            length(fllst[[k]]$url[kInd])),
                        rep(ifelse(length(grep(pattern="variables", 
                                               x=fllst[[k]]$url))==1,
                                   fllst[[k]]$url[grep(pattern="variables", 
                                                       x=fllst[[k]]$url)], 
                                   NA_character_),
                            length(fllst[[k]]$url[kInd])),
                        rep(relnames[[k]][1], length(fllst[[k]]$url[kInd])))
        flset <- rbind(flset, flsetk)
      }
    }
  }
  
  flset <- data.frame(flset)
  colnames(flset) <- c("url","md5","md5var","urlvar","release")
  
  # drop default base url, but keep a copy of original
  flset$urlbase <- flset$url
  flset$url <- base::gsub(pattern="https://storage.googleapis.com/", 
                       replacement="", flset$url)
  flset$urlvar <- base::gsub(pattern="https://storage.googleapis.com/", 
                          replacement="", flset$urlvar)
  
  # add GCS base url, if GCS is enabled
  if(arrow::arrow_with_gcs()) {
    flset$url <- paste("gs://anonymous@", flset$url, sep="")
    flset$urlvar <- paste("gs://anonymous@", flset$urlvar, sep="")
  } else {
    # S3 base url and suffix if GCS is not enabled
    flset$url <- paste("s3://", flset$url, 
                    "/?endpoint_override=https%3A%2F%2Fstorage.googleapis.com", sep="")
    flset$urlvar <- paste("s3://", flset$urlvar, 
                       "/?endpoint_override=https%3A%2F%2Fstorage.googleapis.com", sep="")
  }
  
  # get most recent variables file
  # if multiple checksums for variables files, raise a flag
  varset <- list()
  varfls <- base::grep(pattern="variables", x=flset$url, value=TRUE)
  if(length(unique(flset$md5var))>1) {
    varu <- unique(flset$md5var)
    for(k in varu) {
      varset[[k]] <- getRecentPublication(flset$url[which(flset$md5==k)])[[1]]
    }
    vardiff <- TRUE
  } else {
    vardiff <- FALSE
  }
  varfl <- getRecentPublication(varfls)[[1]]
  
  if(isFALSE(metadata)) {
    # subset to only the table, if requested
    # keep variables files
    nmInd <- base::grep(pattern=paste("[.]", tabl, "[.]", sep=""), 
                        x=flset$url)
    flset <- flset[nmInd,]
    urllst <- flset$url
  } else {
    urllst <- flset$url
  }
  # check for no data
  if(length(urllst)==0) {
    message(paste("No files found for table ", tabl, " in ", 
                  package, " package", sep=""))
    return(invisible())
  } else {
    # if only metadata, print message but still return the metadata files
    if(tabl!="all" & length(base::grep(pattern=paste("[.]", tabl, "[.]", sep=""), x=urllst))==0) {
      message(paste("No files found for table ", tabl, ", only metadata.", sep=""))
    }
  }
  
  # if vardiff is FALSE, only need urllst (set of urls to stack) and varfl (variables file to use)
  # if vardiff is TRUE, need the set of all urls, including variables files
  return(list(files=urllst, variables=varfl, varcheck=vardiff, 
              filesall=flset, varset=varset))
}
