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
#' @param level Whether the unzipping should occur only for the 'top' zip file, or unzip 'all' recursively,
#' or only files 'in' the folder specified

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2017-09-28)
#   Claire Lunch (2017-09-28)
##############################################################################################

unzipZipfile <- function(zippath, outpath = substr(zippath, 1, nchar(zippath)-4), level="all"){
  if(level %in% c("top", "all")){
    unzip(zipfile = zippath, exdir=outpath)
  }

  if(level == "all"){
    unzip(zipfile = zippath, exdir=outpath)
    zps <- listZipfiles(zippath)
    writeLines("Unpacking zip files")
    pb <- txtProgressBar(style=3)
    setTxtProgressBar(pb, 0)
    if(length(zps) >= 1){
      for(i in 1:length(zps)){
        p <- paste0(outpath, "/", zps[i])
        unzip(p, exdir=substr(p, 1, nchar(p)-4), overwrite = T)
        if (file.exists(p)) file.remove(p)
        setTxtProgressBar(pb, 1/length(zps))
#        writeLines(paste("Unpacked ", zps[i]))
      }
      setTxtProgressBar(pb, 1)
      close(pb)
    } else writeLines("This zip file doesn't contain monthly data packages")
  }

  if(level == "in"){
    zps <- list.files(zippath)
    writeLines("Unpacking zip files")
    pb <- txtProgressBar(style=3)
    setTxtProgressBar(pb, 0)
    if(length(zps) >= 1){
      for(i in 1:length(zps)){
        p <- paste0(zippath, "/", zps[i])
        unzip(p, exdir=substr(p, 1, nchar(p)-4), overwrite = T)
        if (file.exists(p)) file.remove(p)
        setTxtProgressBar(pb, 1/length(zps))
#        writeLines(paste("Unpacked ", zps[i]))
      }
      setTxtProgressBar(pb, 1)
      close(pb)
    } else writeLines("This zip file doesn't contain monthly data packages")
  }
}
