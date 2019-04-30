##############################################################################################
#' @title Get files from NEON ECS Bucket using URLs in stacked data function

#' @author
#' Kaelin Cawley \email{kcawley@battelleecology.org}

#' @description
#' Read in a set of URLs from stacked data and then download the data from the NEON ECS buckets
#'
#' @param filepath The location of the stacked NEON data containing URIs
#' @param savepath The location to save the output files from the ECS bucket, optional. 
#' Defaults to creating a "ECS_zipFiles" folder in the filepath directory.
#' @param pick.files T or F, should the user be told the name of each file before downloading? 
#' Defaults to F. When working in batch mode, or other non-interactive workflow, use pick.files=F.
#' @param check.size T or F, should the user be told the total file size before downloading? 
#' Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param unzip T or F, indicates if the downloaded zip files from ECS buckets should be 
#' unziped into the same directory, defaults to T. Supports .zip and .tar.gz files currently.

#' @return A folder in the working directory (or in savepath, if specified), containing all zip files meeting query criteria.

#' @examples
#' \dontrun{
#' # To download stream morphology data from stacked data:
#' zipsByURI(filepath="~/filesToStack00131")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Kaelin Cawley (2018-11-01)
#     original creation, heavily adapted from zipsByProdcut and stackByTable
##############################################################################################
zipsByURI <- function(filepath, 
                      savepath = paste0(filepath, "/ECS_zipFiles"), 
                      pick.files=FALSE,
                      check.size=TRUE,
                      unzip = TRUE) {

  
  #### Check for the variables file in the filepath
  files <- list.files(filepath, pattern = "variables")
  if(length(files) == 0){
    stop("Variables file is not present in specified filepath.")
  }
  
  variablesFile <- utils::read.csv(paste(filepath,"variables.csv",sep = "/"),stringsAsFactors = FALSE)
  URLs <- variablesFile[variablesFile$dataType == "uri",]
  
  #All of the tables in the package with URLs to download
  allTables <- unique(URLs$table)
  
  #Loop through tables and fields to compile a list of URLs to download
  URLsToDownload <- NA
  URLsNotToDownload <- NA #Avoids asking about duplicates
  if(pick.files==TRUE){
    #Loop through tables
    for(i in seq(along = allTables)){
      tableData <- utils::read.csv(paste(filepath,"/",allTables[i],".csv",sep = ""),stringsAsFactors = FALSE)
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
      tableData <- utils::read.csv(paste(filepath,"/",allTables[i],".csv",sep = ""),stringsAsFactors = FALSE)
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
  
  #Loop to check cumulative size of files
  for(i in URLsToDownload) {
    # get file metadata
    response <- httr::HEAD(i)  
    # grab file size and convert bytes to MB
    fileSize[i] <- round(as.numeric(httr::headers(response)[["Content-Length"]])/1048576, 1)   
  }
  totalFileSize <- sum(fileSize)
  
  if(check.size==TRUE) {
    resp <- readline(paste("Continuing will download",length(URLsToDownload), "files totaling approximately",
                           totalFileSize, "MB. Do you want to proceed y/n: ", sep=" "))
    if(!(resp %in% c("y","Y"))) stop()
  }else{
    cat("Continuing will download",length(URLsToDownload), "files totaling approximately",totalFileSize,"MB.")
  }

  # copy zip files into folder
  numDownloads <- 0
  for(i in URLsToDownload) {
    dl <- try(downloader::download(i, paste(savepath, gsub("^.*\\/","",i), sep="/"), mode = "wb"))
    if(!is.null(attr(dl, "class")) && attr(dl, "class") == "try-error"){
      cat("Unable to download data for URL:",i)
      next
    }
    numDownloads <- numDownloads + 1
    if(unzip == TRUE && grepl("\\.zip|\\.ZIP",i)){
      utils::unzip(paste(savepath, gsub("^.*\\/","",i), sep="/"), 
                     exdir=paste(savepath, gsub("^.*\\/|\\..*$","",i), sep="/"), 
                     overwrite = TRUE)
    }else if(unzip == TRUE && grepl("\\.tar\\.gz",i)){
      utils::untar(paste(savepath, gsub("^.*\\/","",i), sep="/"), 
                     exdir=paste(savepath, gsub("^.*\\/|\\..*$","",i), sep="/"))
    }else if(unzip == TRUE && !(grepl("\\.zip|\\.ZIP",i) | grepl("\\.tar\\.gz",i))){
      cat("Unable to unzip data for URL:",i)
    }
  }

  cat(numDownloads, "file(s) successfully downloaded to", savepath, sep=" ")

}


