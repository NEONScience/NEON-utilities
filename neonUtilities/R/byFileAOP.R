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
#' @param token User specific API token (generated within neon.datascience user accounts)

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

byFileAOP <- function(dpID, site, year, check.size=TRUE, savepath=NA, token = NA) {

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.00[1-2]{1}",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.00#", sep=" "))
  }

  # error message if site is left blank
  if(regexpr('[[:alpha:]]{4}', site)!=1) {
    stop("A four-letter site code is required. NEON sites codes can be found here: https://www.neonscience.org/field-sites/field-sites-map/list")
  }

  # error message if year is left blank
  if(regexpr('[[:digit:]]{4}', year)!=1) {
    stop("Year is required (e.g. '2017').")
  }

  # query the products endpoint for the product requested
  req <- getAPI(paste("http://data.neonscience.org/api/v0/products/", dpID, sep=""), token)
  avail <- jsonlite::fromJSON(httr::content(req, as="text", encoding="UTF-8"), 
                              simplifyDataFrame=TRUE, flatten=TRUE)

  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product", dpID, sep=" "))
  }
  
  # check that token was used
  if(!is.na(token) & !is.null(req$headers$`x-ratelimit-limit`)) {
    if(req$headers$`x-ratelimit-limit`==200) {
      cat('API token was not recognized. Public rate limit applied.\n')
    }
  }

  # error message if field spectra data are attempted
  if(dpID=='DP1.30012.001') {
    stop('DP1.30012.001 is the Field spectral data product, which is published as tabular data. Use zipsByProduct() or loadByProduct() to download these data.')
  }

  # error message if data are not from AOP
  if(avail$data$productScienceTeamAbbr!="AOP") {
    stop(paste(dpID, "is not a remote sensing product. Use zipsByProduct()"))
  }

  # check for sites that are flown under the flight box of a different site
  if(site %in% shared_flights$site) {
    flightSite <- shared_flights$flightSite[which(shared_flights$site==site)]
    if(site %in% c('TREE','CHEQ','KONA')) {
      cat(paste(site, ' is part of the flight box for ', flightSite,
                '. Downloading data from ', flightSite, '.\n', sep=''))
    } else {
      cat(paste(site, ' is an aquatic site and is sometimes included in the flight box for ', flightSite,
                '. Aquatic sites are not always included in flight coverage every year.\nDownloading data from ',
                flightSite, '. Check data to confirm coverage of ', site, '.\n', sep=''))
    }
    site <- flightSite
  }

  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- month.urls[grep(paste(site, year, sep="/"), month.urls)]

  # error message if nothing is available
  if(length(month.urls)==0) {
    stop("There are no data at the selected site and year.")
  }

  file.urls.current <- getFileUrls(month.urls, token = token)
  downld.size <- sum(as.numeric(as.character(file.urls.current$size)), na.rm=T)
  downld.size.read <- convByteSize(downld.size)

  # ask user if they want to proceed
  # can disable this with check.size=F
  if(check.size==TRUE) {
    resp <- readline(paste("Continuing will download ", nrow(file.urls.current), " files totaling approximately ",
                           downld.size.read, ". Do you want to proceed y/n: ", sep=""))
    if(!(resp %in% c("y","Y"))) {
      stop("Download halted.")
    }
  } else {
    cat(paste("Downloading files totaling approximately", downld.size.read, "\n", sep=" "))
    }

  # create folder in working directory to put files in
  if(is.na(savepath)) {
    filepath <- paste(getwd(), "/", dpID, sep="")
  } else {
    filepath <- paste(savepath, "/", dpID, sep="")
  }
  if(dir.exists(filepath) == F) {
    dir.create(filepath, showWarnings=F)
    }

  # copy zip files into folder
  j <- 1
  messages <- list()
  writeLines(paste("Downloading ", nrow(file.urls.current), " files", sep=""))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 1/(nrow(file.urls.current)-1))

  counter <- 1

  while(j <= nrow(file.urls.current)) {

    if (counter > 2) {
      cat(paste0("\nRefresh did not solve the isse. URL query for file ", file.urls.current$name[j],
                  " failed. If all files fail, check data portal (data.neonscience.org/news) for possible outage alert.\n"))

      j <- j + 1
      counter <- 1
    } else {
      path1 <- strsplit(file.urls.current$URL[j], "\\?")[[1]][1]
      pathparts <- strsplit(path1, "\\/")
      path2 <- paste(pathparts[[1]][4:(length(pathparts[[1]])-1)], collapse="/")
      newpath <- paste0(filepath, "/", path2)

      if(dir.exists(newpath) == FALSE) {
        dir.create(newpath, recursive = TRUE)
      }

      t <- tryCatch(
        {
          suppressWarnings(downloader::download(file.urls.current$URL[j],
                                                paste(newpath, file.urls.current$name[j], sep="/"),
                                                mode="wb", quiet=T))
        }, error = function(e) { e } )

      if(inherits(t, "error")) {
        
        # re-attempt download once with no changes
        if(counter < 2) {
          writeLines(paste0("\n", file.urls.current$name[j], " could not be downloaded. Re-attempting."))
          t <- tryCatch(
            {
              suppressWarnings(downloader::download(file.urls.current$URL[j],
                                                    paste(newpath, file.urls.current$name[j], sep="/"),
                                                    mode="wb", quiet=T))
            }, error = function(e) { e } )
          if(inherits(t, "error")) {
            counter <- counter + 1
          } else {
            messages[j] <- paste(file.urls.current$name[j], "downloaded to", newpath, sep=" ")
            j <- j + 1
            counter <- 1
          }
        } else {
          writeLines(paste0("\n", file.urls.current$name[j], " could not be downloaded. URLs may have expired. Refreshing URL list."))
          file.urls.new <- getFileUrls(month.urls, token = token)
          file.urls.current <- file.urls.new
          counter <- counter + 1
        }

      } else {
        messages[j] <- paste(file.urls.current$name[j], "downloaded to", newpath, sep=" ")
        j <- j + 1
        counter <- 1
        utils::setTxtProgressBar(pb, j/(nrow(file.urls.current)-1))
      }
      
    }
  }
  utils::setTxtProgressBar(pb, 1)
  close(pb)

  writeLines(paste("Successfully downloaded ", length(messages), " files."))
  writeLines(paste0(messages, collapse = "\n"))
}
