##############################################################################################
#' @title Serially download all AOP files for a given site, year, and product

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Query the API for AOP data by site, year, and product, and download all files found. Downloads serially to avoid overload; may take a very long time.
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site The four-letter code of a single NEON site, e.g. 'CLBJ'.
#' @param year The four-digit year to search for data. Defaults to 2017.
#' @param check.size T or F, should the user be told the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.

#' @return A folder in the working directory, containing all files meeting query criteria.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2018-02-19)
#     original creation
##############################################################################################

byFileAOP <- function(dpID, site="SJER", year="2017", check.size=TRUE) {

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

  # get all the file names, and stash the URLs in an object
  file.urls <- c(NA, NA, NA)
  messages <- NA
  for(i in 1:length(month.urls)) {
    tmp <- httr::GET(month.urls[i])
    tmp.files <- jsonlite::fromJSON(httr::content(tmp, as="text"),
                                    simplifyDataFrame=T, flatten=T)

    # check for no files
    if(length(tmp.files$data$files)==0) {
      messages <- c(messages, paste("No files found for site", tmp.files$data$siteCode,
                                    "and year", tmp.files$data$month, sep=" "))
      next
    }
    
    file.urls <- rbind(file.urls, cbind(tmp.files$data$files$name,
                                      tmp.files$data$files$url,
                                      tmp.files$data$files$size))
  }

  # get size info
  file.urls <- data.frame(file.urls, row.names=NULL)
  colnames(file.urls) <- c("name", "URL", "size")
  downld.size <- sum(as.numeric(as.character(file.urls$size)), na.rm=T)/1e6
  file.urls$URL <- as.character(file.urls$URL)
  file.urls$name <- as.character(file.urls$name)

  # ask user if they want to proceed
  # can disable this with check.size=F
  if(check.size==TRUE) {
    resp <- readline(paste("Continuing will download files totaling approximately",
                           downld.size, "MB. Do you want to proceed y/n: ", sep=" "))
    if(!(resp %in% c("y","Y"))) {
      stop("Download halted.")
    }
  }
  
  # create folder in working directory to put files in
  filepath <- paste(getwd(), "/files", substr(dpID, 5, 9), sep="")
  dir.create(filepath)

  # copy zip files into folder
  for(i in 2:nrow(file.urls)) {
    downloader::download(file.urls$URL[i], paste(filepath, file.urls$name[i], sep="/"), mode="wb")
  }
  
  messages <- c(messages, paste(nrow(file.urls)-1, "files downloaded to",
                                filepath, sep=" "))
  writeLines(paste0(messages[-1], collapse = "\n"))
  
}


