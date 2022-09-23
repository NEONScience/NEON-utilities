##############################################################################################
#' @title Get files from NEON ECS Bucket using URLs in stacked data

#' @author
#' Kaelin Cawley \email{kcawley@battelleecology.org}

#' @description
#' Read in a set of URLs from NEON data tables and then download the data from the NEON ECS buckets. 
#' Assumes data tables are in the format resulting from merging files using stackByTable().
#' File downloads from ECS can be extremely large; be prepared for long download times and large file storage.
#'
#' @param filepath The location of the NEON data containing URIs. Can be either a local directory containing NEON tabular data or a list object containing tabular data.
#' @param savepath The location to save the output files from the ECS bucket, optional. 
#' Defaults to creating a "ECS_zipFiles" folder in the filepath directory.
#' @param pick.files T or F, should the user be told the name of each file before downloading? 
#' Defaults to F. When working in batch mode, or other non-interactive workflow, use pick.files=F.
#' @param check.size T or F, should the user be told the total file size before downloading? 
#' Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param unzip T or F, indicates if the downloaded zip files from ECS buckets should be 
#' unzipped into the same directory, defaults to T. Supports .zip and .tar.gz files currently.
#' @param saveZippedFiles T or F: should the zip files be retained after unzipping? Defaults to F.

#' @return A folder in the working directory (or in savepath, if specified), containing all files meeting query criteria.

#' @examples
#' \dontrun{
#' # To download stream morphology data from stacked data:
#' zipsByURI(filepath="~/filesToStack00131/stackedFiles")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin Cawley (2018-11-01)
#     original creation, heavily adapted from zipsByProdcut and stackByTable
#   Claire Lunch (2022-02-25)
#     added option to use R objects as input data
##############################################################################################
zipsByURI <- function(filepath, 
                      savepath = paste0(filepath, "/ECS_zipFiles"), 
                      pick.files=FALSE,
                      check.size=TRUE,
                      unzip = TRUE,
                      saveZippedFiles = FALSE) {

  # check that filepath points to either a directory or an R object
  if(!identical(class(filepath), "list")) {
    if(!dir.exists(filepath)) {
      stop("Input filepath is not a list object in the environment nor an existing file directory.")
    }
  }
  
  # if filepath is a directory, read in contents
  if(identical(class(filepath), "list")) {
    tabList <- filepath
    if(all(!dir.exists(savepath))) {
      dc <- try(dir.create(savepath), silent=TRUE)
      if(identical(class(dc), "try-error")) {
        message(paste("Could not create savepath directory. Files will be saved to ", 
                      paste0(getwd(), "/ECS_zipFiles"), sep=""))
        savepath <- paste0(getwd(), "/ECS_zipFiles")
      }
    }
  } else {
    files <- list.files(filepath, full.names=TRUE, recursive=FALSE)
    tabList <- vector("list", length(files))
    names(tabList) <- files
    for(j in 1:length(files)) {
      tabList[[j]] <- try(utils::read.csv(files[j], stringsAsFactors=FALSE), silent=TRUE)
      if(identical(class(tabList[[j]]), "try-error")) {
        message(paste("File", files[j], "could not be read.", sep=" "))
        names(tabList)[j] <- paste("error", j, sep="")
        next
      }
    }
    tabList <- tabList[grep("error", names(tabList), invert=TRUE)]
  }
  
  #### Check for the variables file in the filepath
  varList <- grep("variables", names(tabList))
  if(length(varList) == 0){
    stop("Variables file was not found in specified filepath.")
  }
  if(length(varList)>1) {
    stop("More than one variables file found in filepath.")
  }
  varFile <- tabList[[grep("variables", names(tabList))]]
  
  URLs <- varFile[varFile$dataType == "uri",]
  
  #All of the tables in the package with URLs to download
  allTables <- unique(URLs$table)
  
  #Loop through tables and fields to compile a list of URLs to download
  URLsToDownload <- NA
  URLsNotToDownload <- NA #Avoids asking about duplicates
  if(pick.files==TRUE){
    #Loop through tables
    for(i in seq(along = allTables)){
      tableData <- tabList[[grep(allTables[i], names(tabList))]]
      URLsPerTable <- names(tableData)[names(tableData)%in%URLs$fieldName]
      
      #Loop through fields
      #Ideally, get size info, but for some reason file.info and CUrl::getURL both don't display a size
      for(j in URLsPerTable){
        if(j %in% URLsToDownload | j %in% URLsNotToDownload){
          next
        }
        resp <- readline(paste("Continuing will download",length(tableData[,j]),"files for",
                               j,"in", allTables[i], "table. Do you want to include y/n: ", sep=" "))
        if(resp %in% c("y","Y")) {
          URLsToDownload <- c(URLsToDownload,tableData[,j])
        }else{
          URLsNotToDownload <- c(URLsNotToDownload,tableData[,j])
        }
      }
      
    }
  }else{
    #Compile all of the possible URLs from all tables that contain uri type fields
    #Loop through tables
    for(i in seq(along = allTables)){
      tableData <- tabList[[grep(allTables[i], names(tabList))]]
      URLsPerTable <- which(names(tableData)%in%URLs$fieldName)
      URLsToDownload <- c(URLsToDownload,unlist(tableData[,URLsPerTable]))
    }
    #Remove duplicates from the list of URLs
    URLsToDownload <- unique(URLsToDownload)
  }
  
  #Remove NA values from the list of URLs
  URLsToDownload <- URLsToDownload[!is.na(URLsToDownload)]
  
  if(length(URLsToDownload)==0){
    stop("There are no URLs other than NA for the stacked data.")
  }
  
  #Create the directory only if it doesn't already exist
  if(!dir.exists(savepath)){
    dir.create(savepath)
  }
  
  #Loop to check existence and cumulative size of files
  cat("checking file sizes...\n")
  fileSize <- rep(NA,length(URLsToDownload))
  idx <- 0
  idxrem <- NA
  for(i in URLsToDownload) {
    idx <- idx + 1
    # get file metadata
    response <- httr::HEAD(i)
    
    # check for file found
    if(is.null(httr::headers(response)[["Content-Length"]])) {
      cat(paste('No files found for url ', i, '\n', sep=''))
      idxrem <- c(idxrem, idx)
    } else {
      # grab file size and convert bytes to MB
      fileSize[idx] <- as.numeric(httr::headers(response)[["Content-Length"]])
    }
  }
  totalFileSize <- convByteSize(sum(fileSize, na.rm = TRUE))
  
  if(check.size==TRUE) {
    resp <- readline(paste("Continuing will download",length(URLsToDownload), "files totaling approximately",
                           totalFileSize, ". Do you want to proceed y/n: ", sep=" "))
    if(!(resp %in% c("y","Y"))) stop()
  }else{
    cat("Downloading",length(URLsToDownload), "files totaling approximately",totalFileSize,".\n")
  }

  # remove URLs with no data
  idxrem <- idxrem[-1]
  if(length(idxrem)>0) {
    URLsToDownload <- URLsToDownload[-idxrem]
  }
  
  # copy zip files into folder
  numDownloads <- 0
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 1/(length(URLsToDownload)-1))
  for(i in URLsToDownload) {
    dl <- try(downloader::download(i, paste(savepath, gsub("^.*\\/","",i), sep="/"), quiet = TRUE, mode = "wb"))
    if(!is.null(attr(dl, "class")) && attr(dl, "class") == "try-error"){
      message(paste("Unable to download data for URL: ",i,"\n", sep=""))
      message(paste("This may be a timeout error. Current timeout setting is", getOption("timeout"), 
                    "seconds. Timeout can be increased using options() function.\n", sep=" "))
      next
    }
    numDownloads <- numDownloads + 1
    utils::setTxtProgressBar(pb, numDownloads/(length(URLsToDownload)-1))
    if(unzip == TRUE && grepl("\\.zip|\\.ZIP",i)){
      utils::unzip(paste(savepath, gsub("^.*\\/","",i), sep="/"), 
                     exdir=paste(savepath, gsub("^.*\\/|\\..*$","",i), sep="/"), 
                     overwrite = TRUE)
      if(!saveZippedFiles){
        unlink(paste(savepath, gsub("^.*\\/","",i), sep="/"),recursive = FALSE)
      }
    } else if(unzip == TRUE && grepl("\\.tar\\.gz",i)){
      utils::untar(paste(savepath, gsub("^.*\\/","",i), sep="/"), 
                     exdir=paste(savepath, gsub("^.*\\/|\\..*$","",i), sep="/"))
      if(!saveZippedFiles){
        unlink(paste(savepath, gsub("^.*\\/","",i), sep="/"),recursive = FALSE)
      }
    } else if(unzip == TRUE && (grepl("\\.fastq\\.gz", i))) {
      
      # check if R.utils is installed (only suggested in package)
      if(!requireNamespace("R.utils", quietly=T)) {
        stop("Package R.utils is required for this function to work on fastq files. Please install and try again.")
      }
      
      R.utils::gunzip(paste(savepath, gsub("^.*\\/", "", i), 
                            sep = "/"), remove=FALSE)
      if (!saveZippedFiles) {
        unlink(paste(savepath, gsub("^.*\\/", "", i),
                     sep = "/"), recursive = FALSE)
      }
    } else if(grepl("\\.csv|\\.CSV",i)){
      next
    } else if(unzip == TRUE && !(grepl("\\.zip|\\.ZIP",i) | grepl("\\.tar\\.gz",i))){
      cat("Unable to unzip data for URL:",i,"\n")
    }
  }
  utils::setTxtProgressBar(pb, 1)
  close(pb)
  cat(numDownloads, "file(s) successfully downloaded to", savepath, "\n", sep=" ")

}


