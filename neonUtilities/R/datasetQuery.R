##############################################################################################
#' @title Query the query endpoint of the NEON API and create an arrow dataset from the results

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description Uses the query endpoint of the NEON API to find the full list of files for a given data product, release, site(s), and date range, then turns them into an arrow dataset.
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param tabl The name of a single data table to download.
#' @param hor The horizontal index of data to download. Only applicable to sensor (IS) data.
#' @param ver The vertical index of data to download. Only applicable to sensor (IS) data.
#' @param package Either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param release The data release to be downloaded; either 'current' or the name of a release, e.g. 'RELEASE-2021'. 'current' returns the most recent release, as well as provisional data if include.provisional is set to TRUE. To download only provisional data, use release='PROVISIONAL'. Defaults to 'current'.
#' @param include.provisional T or F, should provisional data be included in downloaded files? Defaults to F. See https://www.neonscience.org/data-samples/data-management/data-revisions-releases for details on the difference between provisional and released data.
#' @param token User specific API token (generated within data.neonscience.org user accounts). Optional.
#' 
#' @return An arrow dataset for the data requested.

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2025-04-22 Claire Lunch: Original creation
#   
##############################################################################################

datasetQuery <- function(dpID, site="all", 
                         startdate=NA, enddate=NA, 
                         tabl=NA_character_, hor=NA, ver=NA,
                         package="basic", release="current", 
                         include.provisional=FALSE, token=NA_character_) {
  
  # if token is an empty string, set to NA
  if(identical(token, "")) {
    token <- NA_character_
  }
  
  # check for expiration
  token <- tokenCheck(token)
  
  # check inputs: this only works for OS and tabular IS data
  # and for IS products, HOR and VER are required, and only one site can be queried
  prod.req <- getAPI(apiURL = paste("https://data.neonscience.org/api/v0/products/", 
                                    dpID, sep=""), token = token)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  pubType <- avail$data$productPublicationFormatType
  
  if(pubType=="AOP Data Product Type") {
    stop(paste(dpID, " is a remote sensing data product. Remote sensing data can't be queried using this function.", sep=""))
  }

  # table is a required input
  if(is.na(tabl)) {
    stop("Table name (tabl=) is a required input to this function.")
  }
  
  if(pubType %in% c("TIS Data Product Type","AIS Data Product Type")) {
    if(dpID %in% c("DP4.00200.001",
                   "DP1.00007.001","DP1.00010.001","DP1.00034.001","DP1.00035.001",
                   "DP1.00036.001","DP1.00037.001","DP1.00099.001","DP1.00100.001",
                   "DP2.00008.001","DP2.00009.001","DP2.00024.001","DP3.00008.001",
                   "DP3.00009.001","DP3.00010.001","DP4.00002.001","DP4.00007.001",
                   "DP4.00067.001","DP4.00137.001","DP4.00201.001","DP1.00030.001")) {
      stop(paste(dpID, " is an eddy covariance data product and can't be queried using this function.", sep=""))
    }
    if(grepl(pattern="^ais[_]", x=tabl)) {
      tabl <- tabl
      if(any(!is.na(c(hor, ver)))) {
        hor <- NA
        ver <- NA
        message("AIS maintenance tables can't be queried by location index; hor and ver will be ignored.")
      }
    } else {
      if(length(site)>1 | identical(site, "all")) {
        stop(paste(dpID, " is a sensor data product and can only be queried at a single site using this function. If you need data at multiple sites, run multiple queries or use loadByProduct().", sep=""))
      }
      if(any(is.na(c(hor, ver)))) {
        stop(paste(dpID, " is a sensor data product. hor and ver indices must be provided to use this query function.", sep=""))
      }
      if(any(nchar(c(hor,ver))!=3)) {
        stop("hor and ver must be 3-digit codes in character format.")
      }
      if(any(c(length(hor), length(ver))!=1)) {
        stop("Only a single sensor location can be queried; hor and ver must each be a single code.")
      }
    }
  } else {
    if(any(!is.na(c(hor, ver)))) {
      hor <- NA
      ver <- NA
      message(paste(dpID, "is an observational data product; hor and ver will be ignored."))
    }
  }
  
  # get files
  urlset <- queryFiles(dpID=dpID, site=site, 
                       startdate=startdate, enddate=enddate,
                       package=package, release=release,
                       timeIndex="all", tabl=tabl, metadata=FALSE,
                       include.provisional=include.provisional, 
                       token=token)
  
  # exit if no files (no message here because queryFiles() prints messages)
  if(is.null(urlset)) {
    return(invisible())
  }
  
  # subset by hor and ver
  if(!is.na(ver)) {
    # urls to files only
    urlset[["files"]] <- base::grep(pattern=paste(
      "[.]00[0-9]{1}[.]", hor, "[.]", ver, "[.][0-9]{2}[A-Z0-9]{1}[.]", sep=""), 
      x=urlset[["files"]], value=TRUE)
    # data frame with urls, checksums, variables files, etc
    hvind <- base::grep(pattern=paste(
      "[.]00[0-9]{1}[.]", hor, "[.]", ver, "[.][0-9]{2}[A-Z0-9]{1}[.]", sep=""), 
      x=urlset[["filesall"]]$url)
    urlsub <- urlset[["filesall"]][hvind,]
  } else {
    urlsub <- urlset[["filesall"]]
  }
  
  # check table type and subset if not site-date
  ttypes <- findTablesByFormat(urlsub$urlbase)
  if(nrow(ttypes)!=1) {
    stop("Table type could not be identified, or more than one table was found. Check inputs.")
  }
  if(ttypes$tableName!=tabl) {
    message("Table name is inconsistent with inputs. Function will proceed, but check for inconsistencies in data outputs.")
  }
  # most recent for each site for site-all
  if(ttypes$tableType=="site-all") {
    sites <- as.list(unique(substring(basename(urlsub$urlbase), 10, 13)))
    # get indices of most recent files for each site
    siteind <- unique(unlist(lapply(sites, function(j, urlsub) {
      url_recent <- getRecentPublication(urlsub$urlbase[grep(j, urlsub$urlbase)])[[1]]
      url_ind <- which(urlsub$urlbase==url_recent)[1]
      return(url_ind)
    }, urlsub=urlsub)))
    urlsub <- urlsub[siteind,]
  }
  # most recent for each lab for lab files
  if(ttypes$tableType == "lab") {
    labs <- unique(unlist(lapply(strsplit(basename(urlsub$urlbase), split="[.]"), 
                                 FUN="[[", 2)))
    
    labind <- unique(unlist(lapply(labs, function(j, urlsub) {
      url_lab <- getRecentPublication(urlsub$urlbase[grep(j, urlsub$urlbase)])[[1]]
      url_ind <- which(urlsub$urlbase==url_lab)[1]
      return(url_ind)
    }, urlsub=urlsub)))
    urlsub <- urlsub[labind,]
  }
  
  # start with variables file returned by queryFiles
  varend <- urlset[["variables"]]
  trystring <- FALSE
  
  # check for inconsistencies in variables files
  if(isTRUE(urlset[["varcheck"]])) {
    # check for differences in fieldNames and dataTypes for the relevant table
    varset <- urlset[["varset"]]
    varFieldDiff <- checkVarFields(variableSet=varset, tableName=tabl)
    if(isTRUE(varFieldDiff)) {
      # if there are inconsistencies, read each separately, then unify
      mdlist <- urlsub$md5var
      tablist <- list()
      piecewise <- TRUE
      for(i in unique(mdlist)) {
        
        vari <- getRecentPublication(urlsub$urlvar[which(mdlist==i)])[[1]]
        flsi <- urlsub$url[which(mdlist==i)]
        
        ds <- try(arrow::open_csv_dataset(sources=flsi, 
                                          schema=schemaFromVar(vari,
                                                               tab=tabl,
                                                               package=package),
                                          skip=1), silent=TRUE)
        if(inherits(ds, "try-error")) {
          piecewise <- FALSE
          next
        } else {
          tablist[[i]] <- ds
        }
        
      }
      
      # if any chunks failed, try for a string schema
      if(isFALSE(piecewise)) {
        trystring <- TRUE
      } else {
        # if all chunks succeeded, merge them
        ds <- try(arrow::open_csv_dataset(sources=tablist, 
                                          unify_schemas=TRUE,
                                          skip=0), silent=TRUE)
        # if merge fails, try for a string schema
        if(inherits(ds, "try-error")) {
          trystring <- TRUE
        }
      }
      
    } else {
      # if fieldNames and dataTypes match across files, use first variables file
      varend <- arrow::read_csv_arrow(varset[[1]], col_names=TRUE, skip=0)
      urlset[["varcheck"]] <- FALSE
    }
  }
  
  if(isFALSE(urlset[["varcheck"]])) {
    tableschema <- schemaFromVar(varend,
                                 tab=tabl,
                                 package=package)
    ds <- try(arrow::open_csv_dataset(sources=urlset[["files"]], 
                                      schema=tableschema,
                                      skip=1), silent=TRUE)
    if(inherits(ds, "try-error")) {
      trystring <- TRUE
    }
  }
  
  # if making dataset via any path above failed, try a string schema
  if(isTRUE(trystring)) {
    message("Data retrieval using variables file to generate schema failed. All fields will be read as strings. This can be slow, and will reduce the possible types of queries you can make. This can usually be avoided by excluding provisional data, and if that does not resolve the problem, consider downloading data using loadByProduct().")
    stringtablist <- list()
    stringpiecewise <- TRUE
    for(i in unique(urlsub$md5var)) {
      
      urlsubi <- urlsub[which(urlsub$md5var==i),]
      flsi <- urlsubi$url
      stringschema <- schemaAllStringsFromSet(flsi)
      
      ds <- try(arrow::open_csv_dataset(sources=flsi, 
                                        schema=stringschema,
                                        skip=1), silent=TRUE)
      if(inherits(ds, "try-error")) {
        stringpiecewise <- FALSE
        next
      } else {
        stringtablist[[i]] <- ds
      }
      
    }
    
    if(isFALSE(stringpiecewise)) {
      message("Reading data as strings failed. Try excluding provisional data, and contact NEON if unable to resolve.")
      return(invisible())
    } else {
      # if all chunks succeeded, merge them
      ds <- try(arrow::open_csv_dataset(sources=stringtablist, 
                                        unify_schemas=TRUE,
                                        skip=0), silent=TRUE)
      if(inherits(ds, "try-error")) {
        message("Reading data as strings failed. Try excluding provisional data, and contact NEON if unable to resolve.")
        return(invisible())
      }
    }
  }
  
  return(ds)
  
}
