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
#' @param avg Deprecated; use timeIndex
#' @param timeIndex Either the string 'all', or the time index of data to download, in minutes. Only applicable to sensor (IS) data. Defaults to 'all'.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param savepath The location to save the output files to
#' @param load T or F, are files saved locally or loaded directly? Used silently with loadByProduct(), do not set manually.
#' @param token User specific API token (generated within neon.datascience user accounts). Optional.

#' @details All available data meeting the query criteria will be downloaded. Most data products are collected at only a subset of sites, and dates of collection vary. Consult the NEON data portal for sampling details.
#' Dates are specified only to the month because NEON data are provided in monthly packages. Any month included in the search criteria will be included in the download. Start and end date are inclusive.
#' timeIndex: NEON sensor data are published at pre-determined averaging intervals, usually 1 and 30 minutes. The default download includes all available data. Download volume can be greatly reduced by downloading only the 30 minute files, if higher frequency data are not needed. Use getTimeIndex() to find the available averaging intervals for each sensor data product.

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
                          timeIndex="all", check.size=TRUE, savepath=NA, load=F, token=NA, avg=NA) {

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
  if(substring(dpID, 5, 5)==3 & dpID!='DP1.30012.001') {
    stop(paste(dpID, "is a remote sensing data product. Use the byFileAOP() or byTileAOP() function.", sep=" "))
  }
  
  # error message if dates aren't formatted correctly
  # separate logic for each, to easily allow only one to be NA
  if(!is.na(startdate)) {
    if(regexpr("[0-9]{4}-[0-9]{2}", startdate)!=1) {
      stop("startdate and enddate must be either NA or valid dates in the form YYYY-MM")
    }
  }
  
  if(!is.na(enddate)) {
    if(regexpr("[0-9]{4}-[0-9]{2}", enddate)!=1) {
      stop("startdate and enddate must be either NA or valid dates in the form YYYY-MM")
    }
  }
  
  # warning message if using deprecated avg= instead of timeIndex=
  if(!is.na(avg)) {
    cat('Input parameter avg is deprecated; use timeIndex to download by time interval.\n')
  } else {
    avg <- timeIndex
  }

  # error for Phenocam data
  if(dpID %in% c("DP1.00033.001", "DP1.00042.001")) {
    stop(paste(dpID, "is a phenological image product, data are hosted by Phenocam.", sep=" "))
  }

  # error message for individual SAE products
  if(dpID %in% c('DP1.00007.001','DP1.00010.001','DP1.00034.001','DP1.00035.001',
                 'DP1.00036.001','DP1.00037.001','DP1.00099.001','DP1.00100.001',
                 'DP2.00008.001','DP2.00009.001','DP2.00024.001','DP3.00008.001',
                 'DP3.00009.001','DP3.00010.001','DP4.00002.001','DP4.00007.001',
                 'DP4.00067.001','DP4.00137.001','DP4.00201.001')) {
    stop(paste(dpID, 'is only available in the bundled eddy covariance data product. Download DP4.00200.001 to access these data.', sep=' '))
  }

  # query the products endpoint for the product requested
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", 
                                    dpID, sep=""), token = token)

  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  
  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product", dpID, sep=" "))
  }
  
  # check that token was used
  if(!is.na(token) & !is.null(prod.req$headers$`x-ratelimit-limit`)) {
    if(prod.req$headers$`x-ratelimit-limit`==200) {
      cat('API token was not recognized. Public rate limit applied.\n')
    }
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
      cat(paste("Downloading by time interval is not available for ", dpID,
                ". Proceeding to download all available data.\n", sep=""))
      avg <- "all"
    } else {
      # check and make sure the averaging interval is valid for the product
      if(!avg %in% table_types$tableTMI[which(table_types$productID==dpID)]) {
        stop(paste(avg, " is not a valid time interval for ", dpID,
                   ". Use function getTimeIndex() to find valid time intervals.", sep=""))
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

  zip.urls <- getZipUrls(month.urls, avg=avg, package=package, dpID=dpID, messages=messages, token = token) %>%
    tidyr::drop_na()

  downld.size <- convByteSize(sum(as.numeric(zip.urls$size), na.rm=T))

  # ask user if they want to proceed
  # can disable this with check.size=F
  if(check.size==TRUE) {
    resp <- readline(paste0("Continuing will download files totaling approximately ",
                           downld.size, ". Do you want to proceed y/n: "))
    if(!(resp %in% c("y","Y"))) {
      stop("Download halted.")
    }
  } else {
    cat(paste0("Downloading files totaling approximately ", downld.size, "\n"))
  }

  # create folder in working directory or savepath to put files in
  if(is.na(savepath)) {
    filepath <- paste(getwd(), "/filesToStack", substr(dpID, 5, 9), sep="")
  } else {
    filepath <- paste(savepath, "/filesToStack", substr(dpID, 5, 9), sep="")
  }
  dir.create(filepath)

  writeLines(paste("Downloading ", nrow(zip.urls), " files", sep=""))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 1/(nrow(zip.urls)-1))

  j <- 1
  counter<- 1

  while(j <= nrow(zip.urls)) {

    if (counter > 3) {
      cat(paste0("\nURL query ", zip.urls$name[j],
                 " failed. The API or data product requested may be unavailable at this time; check data portal (data.neonscience.org/news) for possible outage alert."))
      j <- j + 1
      counter <- 1
    } else {
      zip_out <- paste(filepath, zip.urls$name[j], sep="/")
      if(!file.exists(substr(zip_out, 1, nchar(zip_out)-4)) || !file.exists(zip_out)) {
        t <- tryCatch(
          {
            suppressWarnings(downloader::download(zip.urls$URL[j], zip_out,
                                                  mode="wb", quiet=T))
          }, error = function(e) { e } )

        if(inherits(t, "error")) {
          writeLines(paste0(zip.urls$name[j], " could not be downloaded. URLs may have expired. Trying new URLs."))

          zip.urls <- quietMessages(getZipUrls(month.urls, avg=avg, package=package, dpID=dpID, messages=messages, token = token) %>%
                              tidyr::drop_na())

          counter <- counter + 1

        } else {
          j <- j + 1
          counter <- 1
        }
      }

      utils::setTxtProgressBar(pb, j/(nrow(zip.urls)-1))
    }
  }

  utils::setTxtProgressBar(pb, 1)
  close(pb)

  if(load==F) {
    messages <- c(messages, paste0(nrow(zip.urls), " files successfully downloaded to ", filepath))
  }

  writeLines(paste0(messages[-1], collapse = "\n"))
}


