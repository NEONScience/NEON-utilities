##############################################################################################
#' @title Select files from a stored set of NEON data, created by neonstore package methods or another method

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Select files from a stored set based on input criteria and pass to stackByTable() or stackEddy()
#'
#' @param filepaths Either a vector of filepaths pointing to files to be stacked, or a single directory containing files that can be stacked, with selection criteria detmined by the other inputs. In both cases files to be stacked must be either site-month zip files or unzipped folders corresponding to site-month zips. [character]
#' @param dpID The NEON data product ID of the data to be stacked [character]
#' @param site Either "all" or a vector of NEON site codes to be stacked [character]
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA. [character]
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA. [character]
#' @param pubdate The maximum publication date of data to include in stacking, in the form YYYY-MM-DD. If NA, the most recently published data for each product-site-month combination will be selected. Otherwise, the most recent publication date that is older than pubdate will be selected. Thus the data stacked will be the data that would have been accessed on the NEON Data Portal, if it had been downloaded on pubdate. [character]
#' @param zipped Should stacking use data from zipped files or unzipped folders? This option allows zips and their equivalent unzipped folders to be stored in the same directory; stacking will extract whichever is specified. Defaults to FALSE, i.e. stacking using unzipped folders. [logical]
#' @param package Either "basic" or "expanded", indicating which data package to stack. Defaults to basic. [character]
#' @param load If TRUE, stacked data are read into the current R environment. If FALSE, stacked data are written to the directory where data files are stored. Defaults to TRUE. [logical]
#' @param nCores Number of cores to use for optional parallel processing. Defaults to 1. [integer]

#' @return If load=TRUE, returns a named list of stacked data tables. If load=FALSE, return is empty and stacked files are written to data directory.

#' @export
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2020-09-15)
##############################################################################################
stackFromStore <- function(filepaths, dpID, site="all", 
                           startdate=NA, enddate=NA, 
                           pubdate=NA, zipped=FALSE,
                           package="basic", load=TRUE, 
                           nCores=1) {
  
  if(any(!file.exists(filepaths))) {
    stop("Files not found in specified file paths.")
  }
  
  # standard error checks
  
  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.00[0-9]{1}",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.00#.", sep=" "))
  }
  
  # error message for AOP data
  if(substr(dpID, 5, 5) == "3" & dpID!="DP1.30012.001"){
    stop("This is an AOP data product, files are not tabular and cannot be stacked.")
  }
  
  if(dpID == "DP1.10017.001" & package != 'basic'){
    saveUnzippedFiles = TRUE
    writeLines("Note: Digital hemispheric photos (in NEF format) cannot be stacked; only the CSV metadata files will be stacked.\n")
  }
  
  # error message if dates aren't formatted correctly
  # separate logic for each, to easily allow only one to be NA
  if(!is.na(startdate)) {
    if(regexpr("[0-9]{4}-[0-9]{2}", startdate)!=1) {
      stop("startdate and enddate must be either NA or valid dates in the form YYYY-MM")
    }
  }
  
  if(!is.na(enddate)) {
    if(regexpr("[0-9]{4}-[0-9]{2}", enddate)!=1) {
      stop("startdate and enddate must be either NA or valid dates in the form YYYY-MM")
    }
  }
  
  if(!is.na(pubdate)) {
    if(regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", pubdate)!=1) {
      stop("pubdate must be either NA or a valid date in the form YYYY-MM-DD")
    }
  }
  
  # package check
  if(!package %in% c("basic","expanded")) {
    stop("package must be either basic or expanded.")
  }
  
  # if a list of files is input, pass directly to stacking functions
  # need to add some checking here - don't allow nonsensical file combinations to pass through
  # should this really be an option?
  if(length(filepaths)>1) {
    if(dpID=="DP4.00200.001") {
      if(load==FALSE) {
        stop("Writing to local directory is not available for DP4.00200.001. Use load=TRUE and assign to a variable name.")
      } else {
        return(stackEddy(filepaths))
      }
    } else {
      if(load==TRUE) {
        savepath <- "envt"
      } else {
        savepath <- NA
      }
      return(stackByTable(filepaths, savepath=savepath, dpID=dpID, package=package))
    }
  }
  
  # if parent directory is input, select files based on criteria
  if(length(filepaths)==1) {
    
    files <- list.files(filepaths, full.names=T, recursive=T)
    files <- files[grep(dpID, files)]
    
    if(zipped==T) {
      files <- files[grep(".zip$", files)]
      stop("Files must be unzipped to use this function. Zip file handling will be added in a future version.")
    } else {
      files <- files[grep(".zip$", files, invert=T)]
    }

    if(!identical(site, "all")) {
      files <- files[grep(paste(site, collapse="|"), files)]
    }
    
    # basic vs expanded files are simple for SAE, not for everything else
    if(dpID=="DP4.00200.001") {
      files <- files[grep(package, files)]
      tabs1 <- "DP4.00200.001.nsae"
      
    } else {
      
      # expanded package can contain basic files. find variables file published 
      # most recently, or most recently before pubdate, to check expected contents
      varFiles <- files[grep("[.]variables[.]", files)]
      varDates <- regmatches(basename(varFiles), 
                             regexpr("[0-9]{8}T[0-9]{6}Z", basename(varFiles)))
      if(is.na(pubdate)) {
        varFile <- varFiles[grep(max(varDates, na.rm=T), varFiles)][1]
      } else {
        pubdateP <- as.POSIXct(pubdate, format="%Y-%m-%d", tz="GMT")
        varDatesP <- as.POSIXct(varDates, format="%Y%m%dT%H%M%SZ", tz="GMT")
        varInd <- which(varDatesP==max(varDatesP[which(varDatesP < pubdateP)], na.rm=T))[1]
        varFile <- varFiles[varInd]
      }
      
      # inspect variables file for basic/expanded package status of each table
      v <- utils::read.csv(varFile, header=T, stringsAsFactors=F)
      vTabs <- unique(v$table)
      vTypes <- unlist(lapply(vTabs, function(x) {
        vx <- v$downloadPkg[which(v$table==x)]
        if(all(vx=="none")) {"none"} else {
          vx <- vx[which(vx!="none")]
        }
        if(all(vx=="basic")) {"basic"} else {
          if(all(vx=="expanded")) {"expanded"} else {"both"}
        }
      }))
      
      # assemble list of tables included in the package
      # expanded package includes the basic tables
      if(package=="expanded") {
        tabs1 <- vTabs
      } else {
        if(package=="basic") {
          tabs1 <- vTabs[which(vTypes %in% c("basic","both"))]
        } else {
          stop("Package must be basic or expanded.")
        }
      }
      
      # include metadata tables
      tabs <- c(tabs1, "validation", "variables", "readme", 
                "categoricalCodes", "sensor_positions")
      
      # select files matching table names and metadata
      files <- files[grep(paste(paste("[.]", tabs, "[.]", sep=""), collapse="|"), files)]
      
      # for tables with both versions, need to pick the correct one
      if(any(vTypes=="both")) {
        bothTabs <- vTabs[which(vTypes=="both")]
        bothFiles <- files[grep(paste(paste("[.]", bothTabs, "[.]", sep=""), 
                                      collapse="|"), files)]
        if(package=="expanded") {
          remFiles <- bothFiles[grep("basic", bothFiles)]
        } else {
          remFiles <- bothFiles[grep("expanded", bothFiles)]
        }
        files <- files[which(!files %in% remFiles)]
      }
      
      # check whether expected files are in archive
      tabCheck <- unlist(lapply(tabs1, function(x) {
        if(length(grep(paste("[.]", x, "[.]", sep=""), files))>0) {
          TRUE
        } else {
          FALSE
        }
      }))
      if(any(!tabCheck)) {
        warning(paste("Some expected data tables are not present in the files to be stacked. Stacking will proceed with available tables, but check for mismatched input criteria, e.g. attempting to stack expanded package from an archive containing only the basic package. The missing tables are", paste(tabs1[!tabCheck], collapse=" ")))
      }
      
    }
    
    # for all tables, discard anything more recent than pub date.
    # for anything that isn't site-month-specific, can pass on to stackByTable - it will find most recent
    # for site-date tables, find the most recent.
    
    # get pub dates
    pubmat <- regexpr("[0-9]{8}T[0-9]{6}Z", basename(files))
    pubdates <- sapply(regmatches(basename(files), pubmat, invert=NA), "[", 2)
    pubdates <- as.POSIXct(pubdates, format="%Y%m%dT%H%M%SZ", tz="GMT")
    
    # keep only dates older than pub date
    if(!is.na(pubdate)) {
      pubdate <- as.POSIXct(pubdate, format="%Y-%m-%d", tz="GMT")
      files <- files[union(which(pubdates <= pubdate), which(is.na(pubdates)))]
    } else {
      pubdate <- max(pubdates, na.rm=T)
    }
    
    for(i in tabs1) {
      filesub <- files[grep(paste("[.]", i, "[.]", sep=""), files)]
      filesuborig <- filesub
      
      # extract sites from filenames (don't use site input in case some sites requested aren't available)
      sitemat <- regexpr("[.][A-Z]{4}[.]", basename(filesub))
      sitesactual <- regmatches(basename(filesub), sitemat)
      sitesactual <- gsub(".", "", sitesactual, fixed=T)
      
      # if files aren't specific to a site, pass on all of them to stackByTable() - it will choose
      if(length(sitesactual)==0) {
        files <- files
      } else {
        # extract dates from filenames for subsetting
        datemat <- regexpr("[0-9]{4}-[0-9]{2}", basename(filesub))
        datadates <- regmatches(basename(filesub), datemat)
        
        # if files aren't specific to a month, pass on all of them to stackByTable() - it will choose
        if(length(datadates)==0) {
          files <- files
        } else {
          
          # subset by input start and end date
          if(!is.na(startdate) & !is.na(enddate)) {
            filesub <- filesub[which(datadates >= startdate & datadates <= enddate)]
          } else if(!is.na(startdate)) {
            filesub <- filesub[which(datadates >= startdate)]
          } else if(!is.na(enddate)) {
            filesub <- filesub[which(datadates <= enddate)]
          }
          
          # extract dates again to match data actually available
          datemat <- regexpr("[0-9]{4}-[0-9]{2}", basename(filesub))
          monthsactual <- regmatches(basename(filesub), datemat)
          
          # extract pub dates again for remaining files
          pubmatsub <- regexpr("[0-9]{8}T[0-9]{6}Z", basename(filesub))
          pubdatesub <- sapply(regmatches(basename(filesub), pubmatsub, invert=NA), "[", 2)
          pubdatesub <- as.POSIXct(pubdatesub, format="%Y%m%dT%H%M%SZ", tz="GMT")
          
          # and extract sites again
          sitemat <- regexpr("[.][A-Z]{4}[.]", basename(filesub))
          sitesactual <- regmatches(basename(filesub), sitemat)
          sitesactual <- gsub(".", "", sitesactual, fixed=T)
          
          # get most recent publication date *before* pubdate for each site and month
          # this mimics behavior as if data had been downloaded from portal or API on pubdate
          # but not precisely - pub packages are created and then have to sync to portal, so there is a small delay
          sitedates <- numeric()
          for(j in unique(sitesactual)) {
            sitemonths <- monthsactual[which(sitesactual==j)]
            for(k in unique(sitemonths)) {
              sitemonthfiles <- filesub[intersect(grep(j, filesub), grep(k, filesub))]
              sitemonthpubs <- pubdatesub[intersect(grep(j, filesub), grep(k, filesub))]
              maxdate <- max(sitemonthpubs[which(sitemonthpubs <= pubdate)])
              if(length(maxdate)==0) {
                sitedates <- sitedates
              } else {
                maxdateindex <- which(pubdatesub==maxdate)
                sitedates <- c(sitedates, maxdateindex)
              }
            }
          }
          filesubsub <- filesub[sitedates]
          files <- files[union(which(files %in% filesubsub),
                               which(!files %in% filesuborig))]
          
        }
        
      }
      
    }
    
    # check for no files
    if(length(files)==0) {
      stop("No files found meeting all input criteria.")
    }
    
    if(dpID=="DP4.00200.001") {
      if(load==FALSE) {
        stop("Writing to local directory is not available for DP4.00200.001. Use load=TRUE and assign to a variable name.")
      } else {
        return(stackEddy(files))
      }
    } else {
      if(load==TRUE) {
        savepath <- "envt"
      } else {
        savepath <- NA
      }
      return(stackByTable(files, savepath=savepath, saveUnzippedFiles=T, nCores=nCores,
                          dpID=dpID, package=package))
    }
    
  }
  
}
