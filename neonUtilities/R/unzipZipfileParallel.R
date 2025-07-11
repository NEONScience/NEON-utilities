##############################################################################################
#' @title Unzip a zip file either at just the top level or recursively through the file

#' @author
#' Christine Laney \email{claney@battelleecology.org}
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Unzip a zip file either at just the top level or recursively through the file
#'
#' @param zippath The filepath of the input file
#' @param outpath The name of the folder to save unpacked files to
#' @param level Whether the unzipping should occur only for the 'top' zip file, or unzip 'all' recursively, or only files 'in' the folder specified
#' @param nCores Number of cores to use for parallelization
#' @param progress T or F, should progress bars be printed?
#' 
#' @keywords internal

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2017-07-02 (Christine Laney): Original creation of function
#   2017-09-28 (Claire Lunch): Addition of option for files downloaded using API (via zipsByProduct())
#   2018-04-03 (Christine Laney): Replacement of line-by-line message of unzipping files with a progress bar
##############################################################################################

unzipZipfileParallel <- function(zippath, outpath = substr(zippath, 1, nchar(zippath)-4), 
                                 level="all", nCores=1, progress=TRUE){

  if(isFALSE(progress)) {
    pborig <- pbapply::pboptions(type="none")
  }
  
  if(level == "all") {
    tl <- utils::unzip(zipfile = zippath, list=TRUE)
    if(any(nchar(tl)>260) & Sys.info()[["sysname"]]=="Windows") {
      stop(paste("Longest filepath is", max(nchar(tl)), "characters long. Filepaths on Windows are limited to 260 characters. Move files closer to the root directory."))
    }
    
    utils::unzip(zipfile = zippath, exdir=outpath)
    zps <- listZipfiles(zippath)
    if(isTRUE(progress)) {
      message(paste0("Unpacking zip files using ", nCores, " cores."))
    }

    if(length(zps) >= 1){

      cl <- parallel::makeCluster(getOption("cl.cores", nCores), 
                                  setup_strategy='sequential')
      suppressWarnings(on.exit(parallel::stopCluster(cl)))

      pbapply::pblapply(zps, function(z, outpath) {
        o <- paste0(outpath, "/", basename(unlist(z)))
        
        l <- utils::unzip(o, list=TRUE)
        nl <- nchar(l$Name) + nchar(o) - 3
        if(any(nl>260) & Sys.info()[["sysname"]]=="Windows") {
          nll <- paste(substr(o, 1, nchar(o)-4), 
                       l$Name[which(nchar(l$Name)==max(nchar(l$Name)))][1], sep="/")
          stop(paste("Filepath", nll, "is", nchar(nll), "characters long. Filepaths on Windows are limited to 260 characters. Move files closer to the root directory, or, if you are using loadByProduct(), switch to using zipsByProduct() followed by stackByTable()."))
        }
        
        utils::unzip(o, exdir=substr(o, 1, nchar(o)-4))
        if (file.exists(o)) {
          file.remove(o) }
      },
      outpath=outpath, cl=cl)

    } else {
      message("This zip file doesn't contain monthly data packages") }
    
    if(isFALSE(progress)) {
      pbapply::pboptions(pborig)
    }
    
    return(zps)
  }

  if(level == "in") {
    zps <- as.list(grep(list.files(zippath, full.names=TRUE), pattern = '*.zip', value=TRUE))
    if(isTRUE(progress)) {
      message(paste0("Unpacking zip files using ", nCores, " cores."))
    }

    if(length(zps) >= 1) {

      cl <- parallel::makeCluster(getOption("cl.cores", nCores),
                                  setup_strategy='sequential')
      suppressWarnings(on.exit(parallel::stopCluster(cl)))

      pbapply::pblapply(zps, function(z, outpath) {
        o <- paste0(outpath, "/", basename(unlist(z)))
        if(!file.exists(substr(o, 1, nchar(o)-4))) {
          
          l <- utils::unzip(z, list=TRUE)
          nl <- nchar(l$Name) + nchar(o) - 3
          if(any(nl>260) & Sys.info()[["sysname"]]=="Windows") {
            nll <- paste(substr(o, 1, nchar(o)-4), 
                         l$Name[which(nchar(l$Name)==max(nchar(l$Name)))][1], sep="/")
            stop(paste("Filepath", nll, "is", nchar(nll), "characters long. Filepaths on Windows are limited to 260 characters. Move files closer to the root directory, or, if you are using loadByProduct(), switch to using zipsByProduct() followed by stackByTable()."))
          }
          
          utils::unzip(z, exdir=substr(o, 1, nchar(o)-4))
        } else {
          message(paste0("Skipping ",  z, " because these files have already been unpacked."))
        }
        if (file.exists(z)) {
            file.remove(z)
          }
        },
        outpath=outpath, cl=cl)
    } else {
        message("This zip file doesn't contain monthly data packages")
    }
    
    if(isFALSE(progress)) {
      pbapply::pboptions(pborig)
    }
    
    return(zps)
  }
}
