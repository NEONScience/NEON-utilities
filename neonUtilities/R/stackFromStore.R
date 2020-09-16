##############################################################################################
#' @title Select files from a stored set of NEON data, created by neonstore package methods or another method

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Select files from a stored set based on input criteria and pass to stackByTable() or stackEddy()
#'
#' @param folder The file path to the folder that needs to be cleaned up (the root directory of the data package)
#' @param orig The list of files that were present in the folder before unzipping and stacking
#' @return Only the folders created during unzip will be deleted. All custom folders/files and the stackedFiles output folder will be retained.

#' @export
#' 
#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2020-09-15)
##############################################################################################
stackFromStore <- function(filepaths, dpID, site="all", 
                           startdate=NA, enddate=NA, 
                           pubdate=NA, zipped=FALSE) {
  
  if(any(!file.exists(filepaths))) {
    stop("Files not found in specified file paths.")
  }
  
  # if a list of files is input, pass directly to stacking functions
  # need to add some checking here - don't allow nonsensical file combinations to pass through
  if(length(filepaths)>1) {
    if(dpID=="DP4.00200.001") {
      return(stackEddy(filepaths))
    } else {
      return(stackByTable(filepaths))
    }
  }
  
  # if parent directory is input, select files based on criteria
  if(length(filepaths)==1) {
    
    files <- list.files(filepaths, full.names=T)
    files <- files[grep(dpID, files)]
    
    if(zipped=T) {
      files <- files[grep(".zip", files)]
    } else {
      files <- files[grep(".zip", files, invert=T)]
    }
    
    if(site!="all") {
      files <- files[grep(paste(site, collapse="|"), files)]
    }
    
    splfiles <- strsplit(files, split=".", fixed=T)
    datadates <- lapply(splfiles, "[", 6)
    
    if(!is.na(startdate) & !is.na(enddate)) {
      files <- files[which(datadates >= startdate & datadates <= enddate)]
    } else if(!is.na(startdate)) {
      files <- files[which(datadates >= startdate)]
    } else if(!is.na(enddate)) {
      files <- files[which(datadates <= enddate)]
    }
    
    splfiles.sites <- strsplit(files, split=".", fixed=T)
    sitesactual <- lapply(splfiles.sites, "[", 3)
    pubdates <- lapply(splfiles.sites, "[", 8)
    
    if(is.na(pubdate)) {
      pubdate <- max(pubdates, na.rm=T)
    }
    
    # subset by pubdate logic:
    # get most recent publication date *before* pubdate for each site and month
    # this mimics behavior as if data had been downloaded from portal or API on pubdate
    # not precisely - pub packages are created and then have to sync to portal, so there is a small delay
    # code below gets most recent by site, need to include month
    sitedates <- character()
    for(i in sitesactual) {
      sitefiles <- files[grep(i, files)]
      sitepubs <- pubdates[grep(i, files)]
      maxdate <- max(sitepubs[which(sitepubs <= pubdate)], na.rm=T)
      if(length(maxdate)==0) {
        sitedates <- sitedates
      } else {
        sitedates <- c(sitedates, maxdate)
      }
    }
    files <- files[sitedates]
    
  }
  
}
