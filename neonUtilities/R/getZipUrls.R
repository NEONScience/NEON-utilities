##############################################################################################
#' @title Get and store the file names, S3 URLs, file size, and download status (default = 0) in a data frame

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}
#' Christine Laney \email{claney@battelleecology.org}

#' @description Used to generate a data frame of available zipfile URLs.
#'
#' @param tmp.files The monthly API URL for the AOP files
#' @param month.urls The monthly API URL for the AOP files
#' @param avg Global variable for averaging interval
#' @param package Global varaible for package type (basic or expanded)
#' @param max.pub Global variable for the most recent URL publication date
#' @param max.pub.site Global variable for the most recent site  from publication date
#' @param dpID Global variable for data product ID

#' @return A dataframe comprised of file names, S3 URLs, file size, and download status (default = 0)

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2018-02-19): original creation
#   Christine Laney (2018-03-05): Added functionality to get new list of URLs if the old ones expire, during the download stream.

##############################################################################################

getZipUrls <- function(tmp.files, month.urls=month.urls, avg, package, max.pub, max.pub.site, dpID) {
  # stash the URLs for just the zips in an object
  zip.urls <- c(NA, NA, NA)
  for(i in 1:length(tmp.files)) {

    # check for no files
    if(length(tmp.files[[i]]$data$files)==0) {
      messages <- c(messages, paste("No files found for site",
                                    substring(month.urls[i],
                                              nchar(month.urls[i])-11,
                                              nchar(month.urls[i])-8),
                                    "and month", substring(month.urls[i],
                                                           nchar(month.urls[i])-6,
                                                           nchar(month.urls[i])), sep=" "))
      next
    }

    # if only one averaging interval is requested, filter by file names
    if(avg!="all") {

      # select files by averaging interval
      all.file <- union(grep(paste(avg, "min", sep=""), tmp.files[[i]]$data$files$name, fixed=T),
                        grep(paste(avg, "_min", sep=""), tmp.files[[i]]$data$files$name, fixed=T))

      if(length(all.file)==0) {
        messages <- c(messages, paste("No files found for site", tmp.files[[i]]$data$siteCode,
                                      "and month", tmp.files[[i]]$data$month, sep=" "))
        next
      }

      # if package==expanded, check that expanded package exists
      # if it doesn't, download basic package
      pk <- package
      if(pk=="expanded") {
        if(length(grep(pk, tmp.files[[i]]$data$files$name))==0) {
          pk <- "basic"
          messages <- c(messages, paste("No expanded package found for site ",
                                        tmp.files[[i]]$data$siteCode, " and month ",
                                        tmp.files[[i]]$data$month,
                                        ". Basic package downloaded instead.",
                                        sep=""))
        }
      }

      # subset to package
      which.file <- intersect(grep(pk, tmp.files[[i]]$data$files$name, fixed=T),
                              union(grep(paste(avg, "min", sep=""),
                                         tmp.files[[i]]$data$files$name, fixed=T),
                                    grep(paste(avg, "_min", sep=""),
                                         tmp.files[[i]]$data$files$name, fixed=T)))

      # check again for no files
      if(length(which.file)==0) {
        messages <- c(messages, paste("No basic package files found for site",
                                      tmp.files[[i]]$data$siteCode,
                                      "and month", tmp.files[[i]]$data$month, sep=" "))
        next
      }

      zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.file],
                                        tmp.files[[i]]$data$files$url[which.file],
                                        tmp.files[[i]]$data$files$size[which.file]))

      # add url for most recent variables & readme
      if(i==max.pub) {
        which.var <- grep("variables", tmp.files[[i]]$data$files$name, fixed=T)[1]
        zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.var],
                                          tmp.files[[i]]$data$files$url[which.var],
                                          tmp.files[[i]]$data$files$size[which.var]))

        which.read <- grep("readme", tmp.files[[i]]$data$files$name, fixed=T)[1]
        zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.read],
                                          tmp.files[[i]]$data$files$url[which.read],
                                          tmp.files[[i]]$data$files$size[which.read]))

      }

      # add url for most recent sensor position file for each site
      if(i %in% max.pub.site) {

        which.sens <- grep("sensor_position", tmp.files[[i]]$data$files$name, fixed=T)[1]
        zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.sens],
                                          tmp.files[[i]]$data$files$url[which.sens],
                                          tmp.files[[i]]$data$files$size[which.sens]))

      }

    } else {

      # to get all data, select the zip files
      all.zip <- grep(".zip", tmp.files[[i]]$data$files$name, fixed=T)

      # error message if there are no zips in the package
      if(length(all.zip)==0) {
        messages <- c(messages, paste("No zip files found for site", tmp.files[[i]]$data$siteCode,
                                      "and month", tmp.files[[i]]$data$month, sep=" "))
        next
      }

      # if package==expanded, check that expanded package exists
      # if it doesn't, download basic package
      pk <- package
      if(pk=="expanded") {
        if(length(grep(pk, tmp.files[[i]]$data$files$name))==0) {
          pk <- "basic"
          messages <- c(messages, paste("No expanded package found for site ",
                                        tmp.files[[i]]$data$siteCode, " and month ",
                                        tmp.files[[i]]$data$month,
                                        ". Basic package downloaded instead.",
                                        sep=""))
        }
      }

      # subset to package
      which.zip <- intersect(grep(pk, tmp.files[[i]]$data$files$name, fixed=T),
                             grep(".zip", tmp.files[[i]]$data$files$name, fixed=T))

      # check again for no files
      if(length(which.zip)==0) {
        messages <- c(messages, paste("No basic package files found for site",
                                      tmp.files[[i]]$data$siteCode,
                                      "and month", tmp.files[[i]]$data$month, sep=" "))
        next
      }

      zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.zip],
                                        tmp.files[[i]]$data$files$url[which.zip],
                                        tmp.files[[i]]$data$files$size[which.zip]))
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
  zip.urls$URL <- as.character(zip.urls$URL)
  zip.urls$name <- as.character(zip.urls$name)
  zip.urls$size <- as.character(zip.urls$size)

  return(zip.urls)
}
