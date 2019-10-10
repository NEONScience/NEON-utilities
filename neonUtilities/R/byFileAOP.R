##############################################################################################
#' @title Serially download all AOP files for a given site, year, and product

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Query the API for AOP data by site, year, and product, and download all files found, preserving original
#' folder structure. Downloads serially to avoid overload; may take a very long time.
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site The four-letter code of a single NEON site, e.g. 'CLBJ'.
#' @param year The four-digit year to search for data. Defaults to 2017.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param savepath The file path to download to. Defaults to NA, in which case the working directory is used.

#' @return A folder in the working directory, containing all files meeting query criteria.

#' @examples
#' \dontrun{
#' # To download 2017 vegetation index data from San Joaquin Experimental Range:
#' byFileAOP(dpID="DP3.30026.001", site="SJER", year="2017")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# Changelog and author contributions / copyrights
#   Claire Lunch (2018-02-19): original creation
#   Christine Laney (2018-03-05): Added functionality to get new list of URLs if the old ones expire, during the download stream.

##############################################################################################

byFileAOP <- function(dpID, site="SJER", year="2017", check.size=TRUE, savepath=NA) {

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.001",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", sep=" "))
  }

  # query the products endpoint for the product requested
  productUrl <- paste0("http://data.neonscience.org/api/v0/products/", dpID)
  req <- httr::GET(productUrl)
  avail <- jsonlite::fromJSON(httr::content(req, as="text"), simplifyDataFrame=TRUE, flatten=TRUE)

  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product", dpID, sep=" "))
  }

  # error message if data are not from AOP
  if(avail$data$productScienceTeamAbbr!="AOP") {
    stop(paste(dpID, "is not a remote sensing product. Use zipsByProduct()"))
  }

  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- month.urls[grep(paste(site, year, sep="/"), month.urls)]

  # error message if nothing is available
  if(length(month.urls)==0) {
    stop("There are no data at the selected site and year.")
  }



  # get and stash the file names, S3 URLs, file size, and download status (default = 0) in a data frame
  getFileUrls <- function(m.urls){
    url.messages <- character()
    file.urls <- c(NA, NA, NA)
    for(i in 1:length(m.urls)) {
      tmp <- httr::GET(m.urls[i])
      tmp.files <- jsonlite::fromJSON(httr::content(tmp, as="text"),
                                      simplifyDataFrame=T, flatten=T)

      # check for no files
      if(length(tmp.files$data$files)==0) {
        url.messages <- c(url.messages, paste("No files found for site", tmp.files$data$siteCode,
                                      "and year", tmp.files$data$month, sep=" "))
        next
      }

      file.urls <- rbind(file.urls, cbind(tmp.files$data$files$name,
                                          tmp.files$data$files$url,
                                          tmp.files$data$files$size))

      # get size info
      file.urls <- data.frame(file.urls, row.names=NULL)
      colnames(file.urls) <- c("name", "URL", "size")
      file.urls$URL <- as.character(file.urls$URL)
      file.urls$name <- as.character(file.urls$name)

      if(length(url.messages) > 0){writeLines(url.messages)}
      file.urls <- file.urls[-1, ]
      return(file.urls)
    }
  }

  file.urls.current <- getFileUrls(month.urls)
  downld.size <- sum(as.numeric(as.character(file.urls.current$size)), na.rm=T)
  downld.size.read <- humanReadable(downld.size, units = "auto", standard = "SI")

  # ask user if they want to proceed
  # can disable this with check.size=F
  if(check.size==TRUE) {
    resp <- readline(paste("Continuing will download", nrow(file.urls.current), "files totaling approximately",
                           downld.size.read, ". Do you want to proceed y/n: ", sep=" "))
    if(!(resp %in% c("y","Y"))) {
      stop("Download halted.")
    }
  } else {
    cat(paste("Downloading files totaling approximately", downld.size.read, "MB\n", sep=" "))
  }

  # create folder in working directory to put files in
  if(is.na(savepath)) {
    filepath <- paste(getwd(), "/", dpID, sep="")
  } else {
    filepath <- paste(savepath, "/", dpID, sep="")
  }
  if(dir.exists(filepath) == F) dir.create(filepath, showWarnings=F)

  # copy zip files into folder
  j <- 1
  messages <- list()
  writeLines(paste("Downloading ", nrow(file.urls.current), " files", sep=""))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 1/(nrow(file.urls.current)-1))
  while(j <= nrow(file.urls.current)) {
    path1 <- strsplit(file.urls.current$URL[j], "\\?")[[1]][1]
    pathparts <- strsplit(path1, "\\/")
    path2 <- paste(pathparts[[1]][4:(length(pathparts[[1]])-1)], collapse="/")
    newpath <- paste0(filepath, "/", path2)

    if(dir.exists(newpath) == F) dir.create(newpath, recursive = T)
    t <- try(downloader::download(file.urls.current$URL[j],
                                  paste(newpath, file.urls.current$name[j], sep="/"),
                                  mode="wb", quiet=T), silent = T)

    if(class(t) == "try-error"){
      writeLines("File could not be downloaded. URLs may have expired. Getting new URLs.")
      file.urls.new <- getFileUrls(month.urls)
      file.urls.current <- file.urls.new
      writeLines("Continuing downloads.")}
    if(class(t) != "try-error"){
      messages[j] <- paste(file.urls.current$name[j], "downloaded to", newpath, sep=" ")
      j = j + 1
    }
    utils::setTxtProgressBar(pb, j/(nrow(file.urls.current)-1))
  }
  utils::setTxtProgressBar(pb, 1)
  close(pb)
  
  writeLines(paste("Successfully downloaded ", length(messages), " files."))
  writeLines(paste0(messages, collapse = "\n"))
}
