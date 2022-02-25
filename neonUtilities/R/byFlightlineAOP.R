##############################################################################################
#' @title Download AOP tiles overlapping specified coordinates for a given site, year, and product

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Query the API for AOP data by site, year, product, and flightline location, and download all files found.
#' Downloads serially to avoid overload; may take a very long time.
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site The four-letter code of a single NEON site, e.g. 'CLBJ'.
#' @param year The four-digit year to search for data. Defaults to 2017.
#' @param easting A vector containing the easting UTM coordinates of the locations to download.
#' @param northing A vector containing the northing UTM coordinates of the locations to download.
#' @param flightlines A vector of flightline numbers to download.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param savepath The file path to download to. Defaults to NA, in which case the working directory is used.
#' @param token User specific API token (generated within neon.datascience user accounts)

#' @return A folder in the working directory, containing all files meeting query criteria.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# Changelog and author contributions / copyrights
#   Claire Lunch (2022-02-25): adapted from byTileAOP()

##############################################################################################

byFlightlineAOP <- function(dpID, site, year, easting=NA, northing=NA, 
                            flightlines=NA, check.size=TRUE, savepath=NA, 
                            token=NA_character_) {

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
  
  # check for sp and sf packages
  if(!requireNamespace("sf", quietly=T)) {
    stop("Package sf is required for this function to work. Install and re-try.")
  }
  if(!requireNamespace("sp", quietly=T)) {
    stop("Package sp is required for this function to work. Install and re-try.")
  }
  
  # input can be either flightlines or coordinates, not both
  if(!is.na(flightlines) & any(!is.na(easting), !is.na(northing))) {
    stop("Input easting and northing coordinates or flightline numbers, not both.")
  }

  # if token is an empty string, set to NA
  if(identical(token, "")) {
    token <- NA_character_
  }

  # get flightline numbers that contain the easting and northing coordinates
  if(is.na(flightlines)) {
    flightlineByCoord()
    
  }
  
  # will need to make a getFlightlineUrls function once flightlineByCoord() is working,
  # then can proceed as below
  file.urls.current <- getTileUrls(month.urls,
                                   format(tileEasting, scientific=F, justified='none'),
                                   format(tileNorthing, scientific=F, justified='none'))
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
  if(dir.exists(filepath) == F) {dir.create(filepath, showWarnings=F)}

  # copy zip files into folder
  j <- 1
  messages <- list()
  writeLines(paste("Downloading ", nrow(file.urls.current), " files", sep=""))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 1/(nrow(file.urls.current)-1))

  counter<- 1

  while(j <= nrow(file.urls.current)) {

    if (counter > 2) {
      cat(paste0("\nRefresh did not solve the isse. URL query for file ", file.urls.current$name[j],
                  " failed. If all files fail, check data portal (data.neonscience.org/news) for possible outage alert.\n",
                 "If file sizes are large, increase the timeout limit on your machine: options(timeout=###)"))

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
          file.urls.new <- getTileUrls(month.urls, tileEasting, tileNorthing, token=token)
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
  
  issues <- getIssueLog(dpID=dpID, token=token)
  utils::write.csv(issues, paste0(filepath, "/issueLog_", dpID, ".csv"),
                   row.names=FALSE)

  writeLines(paste("Successfully downloaded ", length(messages), " files."))
  writeLines(paste0(messages, collapse = "\n"))
}
