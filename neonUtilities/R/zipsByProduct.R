##############################################################################################
#' @title Get files from NEON API to feed the stackByTable() function

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Pull files from the NEON API, by data product, in a structure that will allow them to be stacked by the stackByTable() function
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site Either the string 'all', meaning all available sites, or a character vector of 4-letter NEON site codes, e.g. c('ONAQ','RMNP'). Defaults to all.
#' @param startdate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param enddate Either NA, meaning all available dates, or a character vector in the form YYYY-MM, e.g. 2017-01. Defaults to NA.
#' @param package Either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param avg Either the string 'all', or the averaging interval to download, in minutes. Only applicable to sensor (IS) data. Defaults to 'all'.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param savepath The location to save the output files to
#' @param load T or F, are files saved locally or loaded directly? Used silently with loadByProduct(), do not set manually.

#' @details All available data meeting the query criteria will be downloaded. Most data products are collected at only a subset of sites, and dates of collection vary. Consult the NEON data portal for sampling details.
#' Dates are specified only to the month because NEON data are provided in monthly packages. Any month included in the search criteria will be included in the download. Start and end date are inclusive.

#' @return A folder in the working directory (or in savepath, if specified), containing all zip files meeting query criteria.

#' @examples
#' \dontrun{
#' # To download plant foliar properties data from all sites, expanded data package:
#' zipsByProduct(dpID="DP1.10026.001", site="all", package="expanded")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2017-09-28)
#     original creation
##############################################################################################

zipsByProduct <- function(dpID, site="all", startdate=NA, enddate=NA, package="basic", 
                          avg="all", check.size=TRUE, savepath=NA, load=F) {

  messages <- NA
  
  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(package, "is not a valid package name. Package must be basic or expanded", sep=" "))
  }

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.001",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", sep=" "))
  }
  
  # error message if dpID can't be downloaded by zipsByProduct()
  if(substring(dpID, 5, 5)==3) {
    stop(paste(dpID, "is a remote sensing data product. Use the byFileAOP() function.", sep=" "))
  }

  if(dpID %in% c("DP1.00033.001", "DP1.00042.001")) {
    stop(paste(dpID, "is a phenological image product, data are hosted by Phenocam.", sep=" "))
  }
  
  # query the products endpoint for the product requested
  productUrl <- paste0("http://data.neonscience.org/api/v0/products/", dpID)
  req <- httr::GET(productUrl)
  avail <- jsonlite::fromJSON(httr::content(req, as="text"), simplifyDataFrame=TRUE, flatten=TRUE)

  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product", dpID, sep=" "))
  }

  # error message if averaging interval is invalid
  if(avg!="all") {
    # if product is OS, proceed with normal download
    if(avail$data$productScienceTeamAbbr %in% c("TOS","AOS","AOP") | 
       dpID %in% c("DP1.20267.001","DP1.00101.001","DP1.00013.001","DP1.00038.001")) {
      cat(paste(dpID, " is not a streaming sensor (IS) data product; cannot subset by averaging interval. Proceeding to download all available data.\n", 
                sep=""))
  } else {
    # exceptions for water quality, SAE, summary weather statistics
    if(dpID %in% c("DP1.20288.001","DP4.00001.001","DP4.00200.001")) {
      cat(paste("Subsetting by averaging interval is not available for ", dpID, 
                ". Proceeding to download all available data.\n", sep=""))
    } else {
      # check and make sure the averaging interval is valid for the product
      if(!avg %in% table_types$tableTMI[which(table_types$productID==dpID)]) {
        stop(paste(avg, " is not a valid averaging interval for ", dpID, 
                   ". Use function getAvg() to find valid averaging intervals.", sep=""))
        }
      }
    }
  }
  
  # get the urls for months with data available
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)

  # subset by sites if requested
  if(!"all" %in% site) {
    month.urls <- month.urls[sort(unlist(sapply(site, grep, month.urls)))]
  } else {
    month.urls <- month.urls
  }
  
  # error message if nothing is available
  if(length(month.urls)==0) {
    stop("There are no data at the selected site(s).")
  }
  
  # subset by dates if requested
  if(!is.na(startdate)) {
    datelist <- substring(month.urls, nchar(month.urls[1])-6, nchar(month.urls[1]))
    month.urls <- month.urls[which(datelist >= startdate)]
  }
  
  # error message if nothing is available
  if(length(month.urls)==0) {
    stop("There are no data at the selected date(s).")
  }
  
  if(!is.na(enddate)) {
    datelist <- substring(month.urls, nchar(month.urls[1])-6, nchar(month.urls[1]))
    month.urls <- month.urls[which(datelist <= enddate)]
  }
  
  # error message if nothing is available
  if(length(month.urls)==0) {
    stop("There are no data at the selected date(s).")
  }
  
  # get all the file names, and stash the URLs for just the zips in an object
  zip.urls <- c(NA, NA, NA)
  for(i in 1:length(month.urls)) {
    tmp <- httr::GET(month.urls[i])
    if(tmp$status_code==500) {
      messages <- c(messages, paste("Query for url ", month.urls[i], 
                                    " failed. API may be unavailable; check data portal data.neonscience.org for outage alert.", 
                                    sep=""))
      next
    }
    tmp.files <- jsonlite::fromJSON(httr::content(tmp, as="text"),
                                    simplifyDataFrame=T, flatten=T)

    # check for no files
    if(length(tmp.files$data$files)==0) {
      messages <- c(messages, paste("No files found for site", substring(month.urls[i], 58, 61),
                                    "and month", substring(month.urls[i], 63, 69), sep=" "))
      next
    }
    
    # if only one averaging interval is requested, filter by file names
    if(avg!="all") {
      
      # select files by averaging interval
      all.file <- union(grep(paste(avg, "min", sep=""), tmp.files$data$files$name, fixed=T),
                        grep(paste(avg, "_min", sep=""), tmp.files$data$files$name, fixed=T))
      
      if(length(all.file)==0) {
        messages <- c(messages, paste("No files found for site", tmp.files$data$siteCode,
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
      
      # subset to package
      which.file <- intersect(grep(pk, tmp.files$data$files$name, fixed=T),
                             union(grep(paste(avg, "min", sep=""), 
                                        tmp.files$data$files$name, fixed=T), 
                                   grep(paste(avg, "_min", sep=""), 
                                        tmp.files$data$files$name, fixed=T)))
      
      # check again for no files
      if(length(which.file)==0) {
        messages <- c(messages, paste("No basic package files found for site",
                                      tmp.files$data$siteCode,
                                      "and month", tmp.files$data$month, sep=" "))
        next
      }
      
      zip.urls <- rbind(zip.urls, cbind(tmp.files$data$files$name[which.file],
                                        tmp.files$data$files$url[which.file],
                                        tmp.files$data$files$size[which.file]))
      
      # add url for one copy of variables file and readme file
      if(i==1) {
        which.var <- grep("variables", tmp.files$data$files$name, fixed=T)[1]
        zip.urls <- rbind(zip.urls, cbind(tmp.files$data$files$name[which.var],
                                          tmp.files$data$files$url[which.var],
                                          tmp.files$data$files$size[which.var]))
        
        # commented out - readme still needs general solution, they are specific to a site-month
        # which.read <- grep("readme", tmp.files$data$files$name, fixed=T)[1]
        # zip.urls <- rbind(zip.urls, cbind(tmp.files$data$files$name[which.read],
        #                                   tmp.files$data$files$url[which.read],
        #                                   tmp.files$data$files$size[which.read]))
      }
      
    } else {
      
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
      
      # subset to package
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

  }

  # check for no files
  if(is.null(nrow(zip.urls))) {
    writeLines(paste0(messages[-1], collapse = "\n"))
    stop(paste("No files found. This indicates either your internet connection failed, or the API is temporarily unavailable, or the data available for ", 
               dpID, 
               " are all hosted elsewhere. Check the data portal data.neonscience.org for outage alerts, and check the ", 
               dpID, " data download page for external links.", sep=""))
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
  } else {
    cat(paste("Downloading files totaling approximately", downld.size, "MB\n", sep=" "))
  }

  # create folder in working directory or savepath to put files in
  if(is.na(savepath)) {
    filepath <- paste(getwd(), "/filesToStack", substr(dpID, 5, 9), sep="")
  } else {
    filepath <- paste(savepath, "/filesToStack", substr(dpID, 5, 9), sep="")
  }
  dir.create(filepath)

  writeLines(paste("Downloading ", nrow(zip.urls)-1, " files", sep=""))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 1/(nrow(zip.urls)-1))
  # copy zip files into folder
  for(i in 2:nrow(zip.urls)) {
    zip_out <- paste(filepath, zip.urls$name[i], sep="/")
    if(!file.exists(substr(zip_out, 1, nchar(zip_out)-4)) || !file.exists(zip_out)) {
      downloader::download(zip.urls$URL[i], zip_out, 
                           mode="wb", quiet=T)
      utils::setTxtProgressBar(pb, i/(nrow(zip.urls)-1))
    }
  }
  utils::setTxtProgressBar(pb, 1)
  close(pb)

  if(load==F) {
    messages <- c(messages, paste(nrow(zip.urls)-1, "files downloaded to",
                                  filepath, sep=" "))
  }
  writeLines(paste0(messages[-1], collapse = "\n"))

}


