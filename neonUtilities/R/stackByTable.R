##############################################################################################
#' @title Join data files in a zipped NEON data package by table type

#' @author
#' Christine Laney \email{claney@battelleecology.org}
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Given a zipped data file, do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.

#' @param filepath The location of the zip file
#' @param savepath The location to save the output files to
#' @param folder T or F: does the filepath point to a parent, unzipped folder, or a zip file? If F, assumes the filepath points to a zip file. Defaults to F. No longer needed; included for back compatibility.
#' @param saveUnzippedFiles T or F: should the unzipped monthly data folders be retained?
#' @param dpID Data product ID of product to stack. Ignored and determined from data unless input is a vector of files, generally via stackFromStore().
#' @param package Data download package, either basic or expanded. Ignored and determined from data unless input is a vector of files, generally via stackFromStore().
#' @param nCores The number of cores to parallelize the stacking procedure. To automatically use the maximum number of cores on your machine we suggest setting nCores=parallel::detectCores(). By default it is set to a single core.
#' @return All files are unzipped and one file for each table type is created and written. If savepath="envt" is specified, output is a named list of tables; otherwise, function output is null and files are saved to the location specified.

#' @examples
#' \dontrun{
#' # To unzip and merge files downloaded from the NEON Data Portal
#' stackByTable("~/NEON_par.zip")
#'
#' # To unzip and merge files downloaded using zipsByProduct()
#' stackByTable("~/filesToStack00024")
#' }

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2017-07-02 (Christine Laney): Original creation
#   2017-09-28 (Claire Lunch): Add error messages
#   2018-04-03 (Christine Laney):
#     * Add error/warning messages for AOP, eddy covariance, and hemispheric
#       digital photo data products (and if the latter, don't allow user to remove the unzipped files).
#     * Allow user to specify the filepath to save to
#   2018-05-08 (Christine Laney):
#     * Remove extranous parameters dpID and package (obtain from data package)
#   2019-11-14 (Nathan Mietkiewicz)
#     * Parallelized the function
#   2020-10-25 (Claire Lunch)
#     * Add handling for input vector of file names; enables working with stackFromStore()
##############################################################################################

stackByTable <- function(filepath, savepath=NA, folder=FALSE, saveUnzippedFiles=FALSE, dpID=NA, package=NA, nCores=1){

  if(length(filepath)>1) {
    folder <- "ls"
    saveUnzippedFiles <- TRUE
  } else {
    if(substring(filepath, nchar(filepath)-3, nchar(filepath))==".zip") {
      folder <- FALSE
    } else {
      folder <- TRUE
    }
  }

  if(identical(savepath, "envt") & saveUnzippedFiles == TRUE & folder!="ls") {
    cat("Warning: savepath = 'envt' can't be combined with saveUnzippedFiles = TRUE unless stacking from an archive. Unzipped files won't be saved.\n")
  }

  #### Check whether data should be stacked ####
  if(folder==FALSE){
    files <- listFilesInZip(filepath)
    files <- files$Name[grep(files$Name, pattern = "NEON.D[[:digit:]]{2}.[[:alpha:]]{4}.")]
    if(length(files) == 0){
      stop("Data files are not present in specified filepath.")
    }
  }

  if(folder==TRUE){
    files <- list.files(filepath, pattern = "NEON.D[[:digit:]]{2}.[[:alpha:]]{4}.")
    if(length(files)==0) {
      stop("Data files are not present in specified filepath.")
    }
  }

  if(folder=="ls"){
    if(is.na(dpID)) {
      stop("dpID must be provided when input is not a single filepath.")
    }
    if(is.na(package)) {
      stop("package (basic or expanded) must be provided when input is not a single filepath.")
    }
    files <- filepath
    if(length(files) == 0){
      stop("Data files are not present in specified filepath.")
    }
    if(any(!file.exists(files))) {
      stop("Files not found in specified filepaths. Check that the input list contains the full filepaths.")
    }
  } else {
    # this regexpr allows for REV = .001 or .002
    dpID <- unique(regmatches(basename(files), 
                       regexpr("DP[1-4][.][0-9]{5}[.]00[1-2]{1}", 
                               basename(files))))
    if(!identical(length(dpID), as.integer(1))) {
      stop("Data product ID could not be determined. Check that filepath contains data files, from a single NEON data product.")
    }
    pack.files <- unique(regmatches(basename(files), 
                                 regexpr("basic|expanded",
                                         basename(files))))
    # expanded package can contain basic files
    if(any(pack.files=="expanded")) { 
      package <- "expanded"
    } else {
      package <- "basic"
    }
  }
  
  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.00[0-9]{1}",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.00#, where the first placeholder must be between 1 and 4.", sep=" "))
  }

  # error message for AOP data
  if(substr(dpID, 5, 5) == "3" & dpID!="DP1.30012.001"){
    stop("This is an AOP data product, files cannot be stacked. Use byFileAOP() or byTileAOP() if you would like to download data.")
  }

  # error message for SAE data
  if(dpID == "DP4.00200.001"){
    stop("This eddy covariance data product is in HDF5 format. Stack using stackEddy()")
  }

  if(dpID == "DP1.10017.001" && package != 'basic'){
    saveUnzippedFiles = TRUE
    writeLines("Note: Digital hemispheric photos (in NEF format) cannot be stacked; only the CSV metadata files will be stacked.\n")
  }
  
  # warning about soil sensor data
  if(dpID %in% c("DP1.00094.001","DP1.00041.001") & length(files)>24) {
    message("Attempting to stack soil sensor data. Note that due to the number of soil sensors at each site, data volume is very high for these data. Consider dividing data processing into chunks, using the nCores= parameter to parallelize stacking, and/or using a high-performance system.")
  }
  
  #### If all checks pass, unzip and stack files ####
  envt <- 0
  if(folder==FALSE) {
    if(is.na(savepath)){savepath <- substr(filepath, 1, nchar(filepath)-4)}
    if(savepath=="envt") {
      savepath <- file.path(tempdir(), paste("store", format(Sys.time(), "%Y%m%d%H%M%S"), sep=""))
      envt <- 1
    }
    if(length(grep(files, pattern = ".zip")) > 0){
      zipList <- unzipZipfileParallel(zippath = filepath, outpath = savepath, level = "all", nCores)
    } else {
      if(!dir.exists(savepath)){dir.create(savepath)}
      if(envt==0) {
        utils::unzip(zipfile=filepath, exdir=dirname(savepath))
      } else {
        utils::unzip(zipfile=filepath, exdir=savepath)
      }
      zipList <- list.files(savepath)
      zipList <- zipList[grep("NEON[.]D[0-9]{2}[.][A-Z]{4}[.]DP[0-4]{1}[.]", 
                              zipList)]
    }
  }

  if(folder==TRUE) {
    if(is.na(savepath)){savepath <- filepath}
    if(savepath=="envt") {
      savepath <- file.path(tempdir(), paste("store", format(Sys.time(), "%Y%m%d%H%M%S"), sep=""))
      envt <- 1
    }
    zipList <- files
    if(length(grep(files, pattern = ".zip")) > 0){
      unzipZipfileParallel(zippath = filepath, outpath = savepath, level = "in", nCores)
    } else {
      if(filepath!=savepath) {
        if(!dir.exists(savepath)){dir.create(savepath)}
        for(i in files) {
          file.copy(paste(filepath, i, sep="/"), savepath)
        }
      }
    }
  }
  
  # logic: if zipped, unzip. save list of unzipped folder names.
  # from there, either way there is a list of unzipped folders.
  # copy from list of folders to temporary directory (this is inefficient) (do not delete originals)
  # use temporary directory as single filepath for stacking
  # if savepath != envt, copy stackedFiles folder from temporary directory to savepath directory
  # delete temporary directory
  if(folder=="ls") {
    if(identical(savepath, "envt")) {envt <- 1}
    if(is.na(savepath) | identical(savepath, "envt")) {
      finalpath <- dirname(files[1])
    } else {
      finalpath <- savepath
    }
    if(!dir.exists(finalpath)){dir.create(finalpath)}
    if(length(grep(files, pattern = ".zip$"))==length(files)){
      fols <- sapply(files, function(x) {utils::unzip(x, exdir=paste(finalpath, 
                                                             substring(basename(x), 1, 
                                                                       nchar(basename(x))-4), 
                                                             sep="/"))})
      files <- substring(names(fols), 1, nchar(names(fols))-4)
    } else {
      if(length(grep(files, pattern = ".zip$"))>I(length(files)/5)) {
        cat("There are a large number of zip files in the input list.\nFiles are only unzipped if all input files are zip files.\n")
      }
    }
    if(length(grep(files, pattern = ".zip$"))>0) {
      files <- files[grep(files, pattern = ".zip$", invert=T)]
    }
    savepath <- file.path(tempdir(), paste("store", format(Sys.time(), "%Y%m%d%H%M%S"), sep=""))
    dir.create(savepath)
    for(i in files) {
      file.copy(i, savepath, recursive=T)
    }
  }

  stackDataFilesParallel(savepath, nCores, dpID)
  getReadmePublicationDate(savepath, out_filepath = paste(savepath, "stackedFiles", sep="/"), dpID)

  if(saveUnzippedFiles == FALSE & envt!=1){
    zipList <- unlist(zipList)
    zipList <- basename(zipList)
    zipList <- gsub('.zip', '', zipList)

    cleanUp(savepath, zipList)
  }
  
  if(folder=="ls" & envt!=1) {
    file.copy(paste(savepath, "stackedFiles", sep="/"), finalpath, recursive=T)
    unlink(savepath, recursive=T)
  }

  if(envt==1) {

    stacked_files <- list.files(paste(savepath, "stackedFiles", sep="/"), full.names = TRUE)
    v <- utils::read.csv(stacked_files[grep('variables', stacked_files)][1], header=T, stringsAsFactors=F)

    stacked_list <- lapply(stacked_files, function(x) {
      if(length(grep("sensor_position", basename(x)))>0) {
        fls <- suppressWarnings(data.table::fread(x, sep=',', keepLeadingZeros = TRUE, colClasses = list(character = c('HOR.VER'))))
      } else if(length(grep("readme", basename(x)))>0) {
        fls <- suppressMessages(utils::read.delim(x, header=FALSE))
        } else if(length(grep("variables", basename(x)))>0 | length(grep("validation", basename(x)))>0 |
                  length(grep("categoricalCodes", basename(x)))>0) {
          fls <- suppressWarnings(data.table::fread(x, sep=",", header=TRUE, 
                                                    encoding="UTF-8", keepLeadingZeros=TRUE))
        } else {
          fls <- try(readTableNEON(x, v), silent=T)
          if(class(fls)=='try-error') {
            fls <- suppressWarnings(data.table::fread(x, sep=",", header=TRUE, 
                                                      encoding="UTF-8", keepLeadingZeros=TRUE))
          }
          return(fls)
      }
    })
    names(stacked_list) <- substring(basename(stacked_files), 1, nchar(basename(stacked_files))-4)
    
    # rename 2D wind tables
    if(length(grep("^2D", names(stacked_list)))>0) {
      names(stacked_list) <- gsub(pattern="^2D", replacement="twoD", x=names(stacked_list))
      message("'2D' has been replaced by 'twoD' in table names to conform to R object rules.")
    }

    # remove temporary directory
    unlink(savepath, recursive=T)

    return(stacked_list)
  }

}
