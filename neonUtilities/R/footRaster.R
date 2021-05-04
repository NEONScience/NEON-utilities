##############################################################################################
#' @title Extract eddy covariance footprint data from HDF5 format

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Create a raster of flux footprint data. Specific to expanded package of eddy covariance data product: DP4.00200.001
#' For definition of a footprint, see Glossary of Meteorology: https://glossary.ametsoc.org/wiki/Footprint
#' For background information about flux footprints and considerations around the time scale of footprint calculations, see Amiro 1998: https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.922.4124&rep=rep1&type=pdf
#'
#' @param filepath One of: a folder containing NEON EC H5 files, a zip file of DP4.00200.001 data downloaded from the NEON data portal, a folder of DP4.00200.001 data downloaded by the neonUtilities::zipsByProduct() function, or a single NEON EC H5 file. Filepath can only contain files for a single site. [character]

#' @details Given a filepath containing H5 files of expanded package DP4.00200.001 data, extracts flux footprint data and creates a raster.

#' @return A rasterStack object containing all the footprints in the input files, plus one layer (the first in the stack) containing the mean footprint.

#' @examples
#' \dontrun{
#' # To run the function on a zip file downloaded from the NEON data portal:
#' ftprnt <- footRaster(filepath="~/NEON_eddy-flux.zip")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   2019-07-04 (Claire Lunch): created
#   2020-03-06 (Claire Lunch and Chris Florian): updated to apply coordinate system to output raster
#   2021-05-03 (Claire Lunch): correction; matrix transposed before creating raster
##############################################################################################

footRaster <- function(filepath) {
  
  # first check for rhdf5 package
  if(!requireNamespace("rhdf5", quietly=T)) {
    stop("Package rhdf5 is required for this function to work.
         \nrhdf5 is a Bioconductor package. To install, use:\ninstall.packages('BiocManager')\nBiocManager::install('rhdf5')\n")
  }
  
  # also check for raster package
  if(!requireNamespace("raster", quietly=T)) {
    stop("Package raster is required for this function to work. Install and re-try.")
  }
  
  # check for vector of filepaths: not currently supported
  if(length(filepath)>1) {
    stop("This function requires a single filepath as input; vector of filepaths is not currently supported.")
  }
  
  # get list of files, unzipping if necessary
  if(substring(filepath, nchar(filepath)-3, nchar(filepath))==".zip") {
    outpath <- gsub(".zip", "", filepath)
    if(!dir.exists(outpath)) {
      dir.create(outpath)
    }
    if(length(grep(".zip", utils::unzip(filepath, list=T)$Name, fixed=T))>0) {
      utils::unzip(filepath, exdir=outpath)
    } else {
      utils::unzip(filepath, exdir=outpath, junkpaths=T)
    }
    filepath <- outpath
  }
  
  # allow for a single H5 file
  if(substring(filepath, nchar(filepath)-2, nchar(filepath))==".h5") {
    files <- filepath
  } else {
    files <- list.files(filepath, recursive=F, full.names=T)
  }
  
  # unzip files if necessary
  if(length(grep(".zip", files))==length(files)) {
    lapply(files, function(x) {
      utils::unzip(x, exdir=filepath)
    })
    files <- list.files(filepath, recursive=F, full.names=T)
  }
  
  # after unzipping, check for .gz
  if(length(grep(".h5.gz", files))>0) {
    lapply(files[grep(".h5.gz", files)], function(x) {
      R.utils::gunzip(x)
    })
    files <- list.files(filepath, recursive=F, full.names=T)
  }
  
  # only need the H5 files for data extraction
  files <- files[grep(".h5$", files)]
  
  # check for duplicate files and use the most recent
  fileDups <- gsub("[0-9]{8}T[0-9]{6}Z.h5", "", files)
  if(any(base::duplicated(fileDups))) {
    maxFiles <- character()
    for(i in unique(fileDups)) {
      maxFiles <- c(maxFiles, 
                    max(files[grep(i, files)]))
    }
    files <- maxFiles
  }
  
  # make empty, named list for the footprint grids
  gridList <- vector("list", length(files))
  names(gridList) <- substring(files, 1, nchar(files)-3)
  
  # make empty list for location/dimension data
  locAttr <- list()
  
  # make empty messages
  messages <- character()
  
  # set up progress bar
  writeLines(paste0("Extracting data"))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 0)

  # extract footprint data from each file
  for(i in 1:length(files)) {
    
    listObj <- base::try(rhdf5::h5ls(files[i]), silent=T)
    
    if(class(listObj)=="try-error") {
      stop(paste("\n", files[i], " could not be read.", sep=""))
    }
    
    listDataObj <- listObj[listObj$otype == "H5I_DATASET",]
    listDataName <- base::paste(listDataObj$group, listDataObj$name, sep = "/")
    
    # filter by variable/level selections
    levelInd <- grep("dp04", listDataName)
    
    # get only the footprint grid data
    if(length(grep("foot/grid", listDataName))==0) {
      stop("No footprint data available.")
    } else {
      gridInd <- grep("foot/grid", listDataName)
    }
    
    ind <- intersect(levelInd, gridInd)
    
    # check that you haven't filtered to nothing
    if(length(ind)==0) {
      stop("No footprint data available.")
    }
    
    listDataName <- listDataName[ind]
    
    # get footprints for each half hour
    gridList[[i]] <- base::lapply(listDataName, rhdf5::h5read, 
                                 file=files[i], read.attributes=T)
    base::names(gridList[[i]]) <- substring(listDataName, 2, nchar(listDataName))
    
    # transpose: eddy4R transposes the data to make them compatible with Python and other systems; need to be transposed back in R
    gridList[[i]] <- base::t(gridList[[i]])
    
    # get location data on first pass
    if(i==1) {
      locAttr <- rhdf5::h5readAttributes(file=files[i], name=listObj$group[2])
      
      # get grid cell dimensions
      oriAttr <- rhdf5::h5read(file=files[i],
                               name=paste(listDataObj$group[intersect(grep('/dp04/data/foot', 
                                                                           listDataObj$group),
                                                                      grep('stat',
                                                                           listDataObj$name))],
                                          'stat', sep='/'))
      # check for internal consistency
      if(length(unique(oriAttr$distReso))!=1) {
        messages <- c(messages,'Resolution attribute is inconsistent. Rasters are unscaled.\n')
        locAttr$distReso <- 0
      } else {
        locAttr$distReso <- unique(oriAttr$distReso)
      }
      
    }
    
    # after first pass, check for consistency
    if(i!=1) {
      newAttr <- rhdf5::h5readAttributes(file=files[i], name=listObj$group[2])
      newOri <- rhdf5::h5read(file=files[i],
                               name=paste(listDataObj$group[intersect(grep('/dp04/data/foot', 
                                                                           listDataObj$group),
                                                                      grep('stat',
                                                                           listDataObj$name))],
                                          'stat', sep='/'))
      newAttr$distReso <- newOri$distReso
      
      if(unique(newOri$distReso)!=locAttr$distReso) {
        messages <- c(messages, 'Resolution attribute is inconsistent. Rasters are unscaled.\nCheck input data, inputs may have included multiple sites.\n')
        locAttr$distReso <- 0
      } else {
        if(!all(c(newAttr$LatTow, newAttr$LonTow, 
                  newAttr$ZoneUtm)==c(locAttr$LatTow,
                                      locAttr$LonTow,
                                      locAttr$ZoneUtm))) {
          messages <- c(messages, 'Resolution attribute is inconsistent. Rasters are unscaled.\nCheck input data, inputs may have included multiple sites.\n')
          locAttr$distReso <- 0
        }
      }
      
    }
    
    utils::setTxtProgressBar(pb, i/length(files))
    
  }
  close(pb)
 
  allGrids <- unlist(gridList, recursive=F)
  
  # check that data come from only one site
  site <- unique(base::gsub( pattern = ".*([A-Z]{4})[.DP4].*", "\\1", names(allGrids)))
  if(length(site)>1) {
    stop(paste(filepath, " contains files from more than one site.", sep=""))
  }
  
  # make raster stack of everything
  rasterList <- lapply(allGrids, raster::raster)
  masterRaster <- raster::stack(rasterList)
  
  # if location data were consistent, apply scaling to stack
  if(locAttr$distReso!=0) {
    
    # set up location data to scale rasters
    LatLong <- data.frame(X = locAttr$LonTow, Y = locAttr$LatTow)
    sp::coordinates(LatLong) <- ~ X + Y # longitude first
    epsg.z <- relevant_EPSG$code[grep(paste("+proj=utm +zone=", 
                                            locAttr$ZoneUtm, " ", sep=""), 
                                      relevant_EPSG$prj4, fixed=T)]
    if(utils::packageVersion("sp")<"1.4.2") {
      sp::proj4string(LatLong) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      utmTow <- sp::spTransform(LatLong, sp::CRS(paste0("+proj=utm +zone=", 
                                                        locAttr$ZoneUtm, "N +ellps=WGS84")))
    } else {
      raster::crs(LatLong) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
      utmTow <- sp::spTransform(LatLong, sp::CRS(paste("+init=epsg:", epsg.z, sep='')))
    }
    
    # adjust extent and coordinate system of raster stack
    raster::extent(masterRaster) <- rbind(c(xmn = raster::xmin(utmTow) - 150.5*locAttr$distReso, 
                                            xmx = raster::xmax(utmTow) + 150.5*locAttr$distReso),
                                          c(ymn = raster::ymin(utmTow) - 150.5*locAttr$distReso, 
                                            ymx = raster::ymax(utmTow) + 150.5*locAttr$distReso))
    raster::crs(masterRaster) <- paste("+init=epsg:", epsg.z, sep='')
  }
  
  # add top layer raster to stack: mean of all layers
  summaryRaster <- raster::mean(masterRaster, na.rm=T)
  masterRaster <- raster::addLayer(summaryRaster, masterRaster)
  names(masterRaster)[1] <- paste(site, "summary")
  
  cat(messages)
  
  return(masterRaster)
  
}
