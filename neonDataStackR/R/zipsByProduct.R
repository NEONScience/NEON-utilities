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
#' @param check.size T or F, should the user be told the total file size before downloading? Defaults to T.

#' @return A folder in the working directory, containing all zip files meeting query criteria.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2017-09-28)
#     original creation
##############################################################################################

zipsByProduct <- function(dpID, site="all", package="basic", check.size=TRUE) {

  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(package, "is not a valid package name. Package must be basic or expanded", sep=" "))
  }

  # query the products endpoint for the product requested
  productUrl <- paste0("http://data.neonscience.org/api/v0/products/", dpID)
  req <- httr::GET(productUrl)
  avail <- jsonlite::fromJSON(httr::content(req, as="text"), simplifyDataFrame=TRUE, flatten=TRUE)

  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product", dpID, sep=" "))
  }

  # error message if data are from AOP
  # Christine do we want to put this here, or in stackByTable? People could
  # use this function to pull data files even if they don't want to stack them
  if(avail$data$productScienceTeamAbbr=="AOP") {
    stop(paste(dpID, "is a remote sensing product and is not stackable."))
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
  for(i in 1:length(month.urls)) {
    tmp <- httr::GET(month.urls[i])
    tmp.files <- jsonlite::fromJSON(httr::content(tmp, as="text"),
                                    simplifyDataFrame=T, flatten=T)

    # if package==expanded, check that expanded package exists
    if(package=="expanded") {
      if(length(grep(package, tmp.files$data$files$name))==0) {
        stop(paste("There is no expanded package for product", dpID, sep=" "))
      }
    }

    which.zip <- intersect(grep(package, tmp.files$data$files$name, fixed=T),
                           grep("zip", tmp.files$data$files$name, fixed=T))

    # error message if there are no zips in the package
    if(length(which.zip)==0) {
      stop(paste("There are no zip files in the", package,
                 "package for product", dpID,
                 "\n Re-publication is pending for many IS products, check back soon."))
    }

    zip.urls <- rbind(zip.urls, cbind(tmp.files$data$files$name[which.zip],
                                      tmp.files$data$files$url[which.zip],
                                      tmp.files$data$files$size[which.zip]))
  }

  # get size info
  zip.urls <- data.frame(zip.urls[-1,])
  colnames(zip.urls) <- c("name", "URL", "size")
  downld.size <- sum(as.numeric(as.character(zip.urls$size)), na.rm=T)/1e6
  zip.urls$URL <- as.character(zip.urls$URL)
  zip.urls$name <- as.character(zip.urls$name)

  # ask user if they want to proceed
  # can disable this with check.size=F
  if(check.size==TRUE) {
    resp <- readline(paste("Continuing will download files totaling approximately",
                           downld.size, "MB. Do you want to proceed y/n: ", sep=" "))
    if(resp=="n" | resp=="N") {
      stop("Download halted.")
    }
  }
  
  # create folder in working directory to put files in
  filepath <- paste(getwd(), "/filesToStack", substr(dpID, 5, 9), sep="")
  dir.create(filepath)

  # copy zip files into folder
  for(i in 1:nrow(zip.urls)) {
    if(Sys.info()["sysname"] == "Windows"){
    download.file(zip.urls$URL[i], paste(filepath, zip.urls$name[i], sep="/"))
    } else {
      download.file(zip.urls$URL[i], paste(filepath, zip.urls$name[i], sep="/"), method = "curl")
    }
  }
}


