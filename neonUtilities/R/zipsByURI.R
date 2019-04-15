##############################################################################################
#' @title Get files from NEON ECS Bucket using URLs in stacked data function

#' @author
#' Kaelin Cawley \email{kcawley@battelleecology.org}

#' @description
#' Read in a set of URLs from stacked data and then download the data from the NEON ECS buckets
#'
#' @param filepath The location of the zip file containing NEON data
#' @param savepath The location to save the output files from the ECS bucket
#' @param pick.files T or F, should the user be told the name of each file before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use pick.files=F.
#' @param check.size T or F, should the user be told the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param unzip T or F, indicates if the downloaded zip files from ECS buckets should be unziped into the same directory. Defaults to T.

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
                      savepath = filepath, 
                      pick.files=TRUE,
                      check.size=TRUE,
                      unzip = TRUE) {

  
  #### Check for the variables file in the filepath
  files <- list.files(filepath, pattern = "variables")
  if(length(files) == 0){
    stop("Variables file is not present in specified filepath.")
  }
  
  variablesFile <- read.csv(paste(filepath,"variables.csv",sep = "/"),stringsAsFactors = FALSE)
  URLs <- variablesFile[variablesFile$dataType == "uri",]
  
  #All of the tables in the package with URLs to download
  allTables <- unique(URLs$table)
  
  #Loop through tables and fields to compile a list of URLs to download
  URLsToDownload <- NA
  if(pick.files==TRUE){
    #Loop through tables
    for(i in seq(along = allTables)){
      tableData <- read.csv(paste(filepath,"/",allTables[i],".csv",sep = ""),stringsAsFactors = FALSE)
      URLsPerTable <- names(tableData)[names(tableData)%in%URLs$fieldName]
      
      #Loop through fields
      #Ideally, get size info, but for some reason file.info and CUrl::getURL both don't display a size
      for(j in URLsPerTable){
        resp <- readline(paste("Continuing will download",length(tableData[,j]),"files for",
                               j,"in", allTables[i], "table. Do you want to include y/n: ", sep=" "))
        if(resp %in% c("y","Y")) {
          URLsToDownload <- c(URLsToDownload,tableData[,j])
        }
      }
      
    }
  }else{
    #Compile all of the possible URLs from all tables that contain uri type fields
    #Loop through tables
    for(i in seq(along = allTables)){
      tableData <- read.csv(paste(filepath,"/",allTables[i],".csv",sep = ""),stringsAsFactors = FALSE)
      URLsPerTable <- which(names(tableData)%in%URLs$fieldName)
      URLsToDownload <- c(URLsToDownload,unlist(tableData[,URLsPerTable]))
    }
  }
  
  #Remove NA values from the list of URLs
  URLsToDownload <- URLsToDownload[!is.na(URLsToDownload)]
  
  if(length(URLsToDownload)==0){
    stop("There are no URLs other than NA for the stacked data.")
  }else{
    # create folder in working directory or savepath to put files in
    if(is.na(savepath)) {
      filepath <- paste(getwd(), "/ECS_zipFiles", sep="")
    } else {
      filepath <- paste(savepath, "/ECS_zipFiles", sep="")
    }
    dir.create(filepath)
    
    # copy zip files into folder
    numDownloads <- 0
    for(i in URLsToDownload) {
      # ask user if they want to proceed
      # can disable this with check.size=F
      if(check.size==TRUE) {
        response <- httr::HEAD(i)  # get file metadata
        fileSize <- round(as.numeric(httr::headers(response)[["Content-Length"]])/1048576, 1)   # grab file size and convert bytes to MB
        
        resp <- readline(paste("Continuing will download files totaling approximately",
                               fileSize, "MB. Do you want to proceed y/n: ", sep=" "))
        if(!(resp %in% c("y","Y"))) {
          next
        }
      }
      try(dl <- downloader::download(i, paste(filepath, gsub("^.*\\/","",i), sep="/"), mode = "wb"))
      if(!exists("dl")){
        cat("Unable to download data for URL:",i)
        next
      }
      rm(dl)
      numDownloads <- numDownloads + 1
      if(unzip == TRUE){
        utils::unzip(paste(filepath, gsub("^.*\\/","",i), sep="/"), exdir=paste(filepath, gsub("^.*\\/|\\..*$","",i), sep="/"), overwrite = TRUE)
      }
    }
  }

  cat(numDownloads, "file(s) downloaded to", filepath, sep=" ")

}


