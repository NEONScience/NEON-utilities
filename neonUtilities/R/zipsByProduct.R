##############################################################################################
#' @title Get files from NEON API to feed the stackByTable() function

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Pull files from the NEON API, by data product, in a structure that will allow them to be stacked by the stackByTable() function
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site Either the string 'all', or the four-letter code of a single NEON site, e.g. 'CLBJ'. Future versions may allow more options for subsetting than one or all sites. Defaults to all.
#' @param package Either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param check.size T or F, should the user be told the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.

#' @return A folder in the working directory, containing all zip files meeting query criteria.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2017-09-28)
#     original creation
##############################################################################################

zipsByProduct <- function(dpID, site="all", package="basic", avg="all", check.size=TRUE) {

  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(package, "is not a valid package name. Package must be basic or expanded", sep=" "))
  }

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

  # get the urls for months with data available
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)

  # subset to site if requested
  if(site!="all") {
    month.urls <- month.urls[grep(site, month.urls)]
  } else {
    month.urls <- month.urls
  }

  # error message if nothing is available
  if(length(month.urls)==0) {
    stop("There are no data at the selected site.")
  }

  # get all the file names, and stash the URLs for just the zips in an object
  zip.urls <- c(NA, NA, NA)
  messages <- NA
  for(i in 1:length(month.urls)) {
    tmp <- httr::GET(month.urls[i])
    tmp.files <- jsonlite::fromJSON(httr::content(tmp, as="text"),
                                    simplifyDataFrame=T, flatten=T)

    # check for no files
    if(length(tmp.files$data$files)==0) {
      messages <- c(messages, paste("No files found for site", tmp.files$data$siteCode,
                                    "and month", tmp.files$data$month, sep=" "))
      next
    }
    
    # if only one averaging interval, subset to those
    if(avg!="all") {
      
      # check, if this is an OS product, proceed with normal zip download
      if(avail$data$productScienceTeamAbbr %in% c("TOS","AOS") | 
         dpID %in% c("DP1.20267.001","DP1.00101.001","DP1.00013.001","DP1.00038.001")) {
        messages <- c(messages, paste(dpID, "is an observational product; 
                                      averaging interval is not a download option. 
                                      Downloading all zip files.", sep=" "))
      } else {
        # I'm thinking send it to a new function here?
      }
    }

    # to get all data, select the zip files
    all.zip <- grep(".zip", tmp.files$data$files$name, fixed=T)

    # error message if there are no zips in the package
    if(length(all.zip)==0) {
      messages <- c(messages, paste("No zip files found for site", tmp.files$data$siteCode,
                 "and month", tmp.files$data$month, sep=" "))
      next
    }

    # if package==expanded, check that expanded package exists
    # if it doesn't, download basic package
    pk <- package
    if(pk=="expanded") {
      if(length(grep(pk, tmp.files$data$files$name))==0) {
        pk <- "basic"
        messages <- c(messages, paste("No expanded package found for site ",
                                      tmp.files$data$siteCode, " and month ",
                                      tmp.files$data$month,
                                      ". Basic package downloaded instead.",
                                      sep=""))
      }
    }

    which.zip <- intersect(grep(pk, tmp.files$data$files$name, fixed=T),
                           grep(".zip", tmp.files$data$files$name, fixed=T))

    # check again for no files
    if(length(which.zip)==0) {
      messages <- c(messages, paste("No basic package files found for site",
                                    tmp.files$data$siteCode,
                                    "and month", tmp.files$data$month, sep=" "))
      next
    }

    zip.urls <- rbind(zip.urls, cbind(tmp.files$data$files$name[which.zip],
                                      tmp.files$data$files$url[which.zip],
                                      tmp.files$data$files$size[which.zip]))
  }

  # get size info
  zip.urls <- data.frame(zip.urls, row.names=NULL)
  colnames(zip.urls) <- c("name", "URL", "size")
  downld.size <- sum(as.numeric(as.character(zip.urls$size)), na.rm=T)/1e6
  zip.urls$URL <- as.character(zip.urls$URL)
  zip.urls$name <- as.character(zip.urls$name)

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
  filepath <- paste(getwd(), "/filesToStack", substr(dpID, 5, 9), sep="")
  dir.create(filepath)

  # copy zip files into folder
  for(i in 2:nrow(zip.urls)) {
    downloader::download(zip.urls$URL[i], paste(filepath, zip.urls$name[i], sep="/"), mode="wb")
  }

  messages <- c(messages, paste(nrow(zip.urls)-1, "zip files downloaded to",
                                filepath, sep=" "))
  writeLines(paste0(messages[-1], collapse = "\n"))

}


