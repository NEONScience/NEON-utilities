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
#' @param pubdate The maximum publication date of data to include in stacking, in the form YYYY-MM. If NA, the most recently published data for each product-site-month combination will be selected. Otherwise, the most recent publication date that is older than pubdate will be selected. Thus the data stacked will be the data that would have been accessed on the NEON Data Portal, if it had been downloaded on pubdate. [character]
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
  
  # if a list of files is input, pass directly to stacking functions
  # need to add some checking here - don't allow nonsensical file combinations to pass through
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
      return(stackByTable(filepaths, savepath=savepath))
    }
  }
  
  # if parent directory is input, select files based on criteria
  if(length(filepaths)==1) {
    
    files <- list.files(filepaths, full.names=T)
    files <- files[grep(dpID, files)]
    files <- files[grep(package, files)]
    
    if(zipped==T) {
      files <- files[grep(".zip", files)]
    } else {
      files <- files[grep(".zip", files, invert=T)]
    }
    
    if(!identical(site, "all")) {
      files <- files[grep(paste(site, collapse="|"), files)]
    }
    
    splfiles <- strsplit(files, split=".", fixed=T)
    datadates <- unlist(lapply(splfiles, "[", 7))
    
    if(!is.na(startdate) & !is.na(enddate)) {
      files <- files[which(datadates >= startdate & datadates <= enddate)]
    } else if(!is.na(startdate)) {
      files <- files[which(datadates >= startdate)]
    } else if(!is.na(enddate)) {
      files <- files[which(datadates <= enddate)]
    }
    
    splfiles.sites <- strsplit(files, split=".", fixed=T)
    sitesactual <- unlist(lapply(splfiles.sites, "[", 3))
    monthsactual <- unlist(lapply(splfiles.sites, "[", 7))
    pubdates <- unlist(lapply(splfiles.sites, "[", 9))
    
    if(is.na(pubdate)) {
      pubdate <- max(pubdates, na.rm=T)
    }
    
    # get most recent publication date *before* pubdate for each site and month
    # this mimics behavior as if data had been downloaded from portal or API on pubdate
    # not precisely - pub packages are created and then have to sync to portal, so there is a small delay
    sitedates <- numeric()
    for(i in unique(sitesactual)) {
      sitemonths <- monthsactual[which(sitesactual==i)]
      for(j in unique(sitemonths)) {
        sitemonthfiles <- files[intersect(grep(i, files), grep(j, files))]
        sitemonthpubs <- pubdates[intersect(grep(i, files), grep(j, files))]
        maxdate <- max(sitemonthpubs[which(sitemonthpubs <= pubdate)])
        if(length(maxdate)==0) {
          sitedates <- sitedates
        } else {
          maxdateindex <- grep(maxdate, files)
          sitedates <- c(sitedates, maxdateindex)
        }
      }
    }
    files <- files[sitedates]
    
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
      return(stackByTable(files, savepath=savepath, saveUnzippedFiles=T, nCores=nCores))
    }
    
  }
  
}
