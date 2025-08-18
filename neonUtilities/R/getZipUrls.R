##############################################################################################
#' @title Get and store the file names, S3 URLs, file size, and download status (default = 0) in a data frame

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}
#' Christine Laney \email{claney@battelleecology.org}

#' @description Used to generate a data frame of available zipfile URLs.
#'
#' @param month.urls The monthly API URL for the URL files
#' @param avg Global variable for averaging interval
#' @param package Global varaible for package type (basic or expanded)
#' @param dpID Global variable for data product ID
#' @param release Data release to be downloaded
#' @param tabl Table name to get
#' @param include.provisional Should provisional data be included?
#' @param token User specific API token (generated within data.neonscience.org user accounts)
#' @param progress T or F: should progress bars be printed?
#' 
#' @keywords internal

#' @return A dataframe comprised of file names, S3 URLs, file size, and download status (default = 0)

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2018-02-19): original creation
#   Christine Laney (2018-03-05): Added functionality to get new list of URLs if the old ones expire, during the download stream.

##############################################################################################

getZipUrls <- function(month.urls, avg, package, dpID, 
                       release, tabl, include.provisional,
                       token = NA_character_,
                       progress=TRUE) {

  if(isTRUE(progress)) {
    message("Finding available files")
    pb <- utils::txtProgressBar(style=3)
    utils::setTxtProgressBar(pb, 0)
  }
  
  # get all the file names
  tmp.files <- list(length(month.urls))
  for(j in 1:length(month.urls)) {
    
    tmp.files[[j]] <- getAPI(month.urls[j], token=token)

    if(tmp.files[[j]]$status_code==500) {
      message(paste("Query for url ", month.urls[j],
                                    " failed. API may be unavailable; check data portal data.neonscience.org for outage alert.",
                                    sep=""))
      next
    }
    
    # short delay if using a token, to delay hitting rate limit
    if(!is.na(token) & !is.null(tmp.files[[j]]$headers$`x-ratelimit-limit`)) {
      if(abs(round(j/4, digits=0)-j/4)<0.01 & as.numeric(tmp.files[[j]]$headers$`x-ratelimit-limit`)>200) {
        Sys.sleep(0.25)
      }
    }
    
    tmp.files[[j]] <- jsonlite::fromJSON(httr::content(tmp.files[[j]], as="text", encoding='UTF-8'),
                                         simplifyDataFrame=T, flatten=T)
    if(isTRUE(progress)) {
      utils::setTxtProgressBar(pb, j/length(month.urls))
    }
    
  }
  
  if(isTRUE(progress)) {
    utils::setTxtProgressBar(pb, 1)
    close(pb)
  }
  
  # if a release is selected, subset to the release
  if(release!="current") {
    tmp.ind <- lapply(tmp.files, function(x) {
      x$data$release==release
    })
    tmp.files <- tmp.files[which(unlist(tmp.ind))]
    
    if(length(tmp.files)==0) {
      stop(paste("No files found for release ", release, 
                 " and query parameters. Check release name and dates.", sep=""))
    }
  }
  
  # if include.provisional==F, exclude provisional data
  if(!include.provisional) {
    tmp.ind <- lapply(tmp.files, function(x) {
      x$data$release!="PROVISIONAL"
    })
    if(length(which(unlist(tmp.ind)))!=length(tmp.files)) {
      message("Provisional data were excluded from available files list. To download provisional data, use input parameter include.provisional=TRUE.")
    }
    tmp.files <- tmp.files[which(unlist(tmp.ind))]
    
    if(length(tmp.files)==0) {
      stop("All files found were provisional. Modify download query (dates and/or sites) or, if you want to use provisional data, use input parameter include.provisional=TRUE.")
    }
  }

  # identify index of most recent publication date, and most recent publication date by site
  rdme.nm <- character(length(tmp.files))
  site.nm <- character(length(tmp.files))
  for(k in 1:length(tmp.files)) {
    if(length(tmp.files[[k]]$data$files)!=0) {
      rdme.nm[k] <- tmp.files[[k]]$data$files$name[grep("readme", tmp.files[[k]]$data$files$name)[1]]
      if(nchar(rdme.nm[k])==0) {
        next
      }
      site.nm[k] <- substring(rdme.nm[k], 10, 13)
      rdme.nm[k] <- substring(rdme.nm[k], nchar(rdme.nm[k])-19, nchar(rdme.nm[k])-4)
    }
  }
  max.pub <- which(rdme.nm==max(rdme.nm, na.rm=T))[1]
  if(length(unique(site.nm))==1) {
    max.pub.site <- max.pub
  } else {
    max.pub.site <- numeric(length(unique(site.nm)))
    max.site.val <- tapply(rdme.nm, site.nm, max, na.rm=T)
    ind <- 0
    for(m in unique(site.nm)) {
      ind <- ind + 1
      max.pub.site[ind] <- which(rdme.nm==max.site.val[m] & site.nm==m)[1]
    }
  }

  # stash the URLs for just the zips in an object
  zip.urls <- c(NA, NA, NA, NA)
  for(i in 1:length(tmp.files)) {

    # check for no files
    if(length(tmp.files[[i]]$data$files)==0) {
      message(paste("No files found for site",
                                    substring(month.urls[i],
                                              nchar(month.urls[i])-11,
                                              nchar(month.urls[i])-8),
                                    "and month", substring(month.urls[i],
                                                           nchar(month.urls[i])-6,
                                                           nchar(month.urls[i])), sep=" "))
      next
    }

    # if only one averaging interval or one table is requested, filter by file names
    if(avg!="all" | tabl!="all") {
      
      # get zip file path to append to name
      h <- getAPIHeaders(tmp.files[[i]]$data$packages$url
                         [which(tmp.files[[i]]$data$packages$type==package)], token=token)
      flhd <- httr::headers(h)
      flnm <- gsub('\"', '', flhd$`content-disposition`, fixed=T)
      flnm <- gsub("inline; filename=", "", flnm, fixed=T)
      flnm <- gsub(".zip", "", flnm, fixed=T)
      
      tmp.files[[i]]$data$files$name <- paste(flnm, tmp.files[[i]]$data$files$name, sep="/")
      
      # start with metadata
      # get url for most recent readme
      # get all variables files (as of v3.0.0 - needed for schema resolution in arrow)
      which.var <- grep("variables", tmp.files[[i]]$data$files$name, fixed=T)
      zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.var],
                                        tmp.files[[i]]$data$files$url[which.var],
                                        tmp.files[[i]]$data$files$size[which.var],
                                        rep(tmp.files[[i]]$data$release, 
                                            length(tmp.files[[i]]$data$files$name[which.var]))))
      if(i==max.pub) {
        which.read <- grep("readme", tmp.files[[i]]$data$files$name, fixed=T)[1]
        if(is.na(which.read)) {
          zip.urls <- zip.urls
        } else {
          zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.read],
                                            tmp.files[[i]]$data$files$url[which.read],
                                            tmp.files[[i]]$data$files$size[which.read],
                                            rep(tmp.files[[i]]$data$release, 
                                                length(tmp.files[[i]]$data$files$name[which.read]))))
        }
      }
      
      # add url for most recent sensor position file for each site
      if(i %in% max.pub.site) {
        
        which.sens <- grep("sensor_position", tmp.files[[i]]$data$files$name, fixed=T)[1]
        if(is.na(which.sens)) {
          zip.urls <- zip.urls
        } else {
          zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.sens],
                                            tmp.files[[i]]$data$files$url[which.sens],
                                            tmp.files[[i]]$data$files$size[which.sens],
                                            rep(tmp.files[[i]]$data$release, 
                                                length(tmp.files[[i]]$data$files$name[which.sens]))))
        }
        
      }
      
      # get science review flag files
      if(any(grepl("science_review_flags", tmp.files[[i]]$data$files$name, fixed=T))) {
        which.srf <- grep("science_review_flags", tmp.files[[i]]$data$files$name, fixed=T)[1]
        zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.srf],
                                          tmp.files[[i]]$data$files$url[which.srf],
                                          tmp.files[[i]]$data$files$size[which.srf],
                                          rep(tmp.files[[i]]$data$release, 
                                              length(tmp.files[[i]]$data$files$name[which.srf]))))
      }
      
      unique.files <- tmp.files[[i]]$data$files
      
      # select files by averaging interval
      if(avg!="all") {
        all.file <- union(grep(paste(avg, "min", sep=""), unique.files$name, fixed=T),
                          grep(paste(avg, "_min", sep=""), unique.files$name, fixed=T))
        
        if(length(all.file)==0) {
          message(paste("No files found for site", tmp.files[[i]]$data$siteCode,
                                        "and month", tmp.files[[i]]$data$month, sep=" "))
          next
        }
      }
      
      if(tabl!="all") {
        all.file <- grep(paste("[.]", tabl, "[.]", sep=""), unique.files$name)
      }

      # no message if table has no files - prints huge numbers if e.g. downloading only a litter chem table
      if(length(all.file)==0) {
        next
      }

      # if package==expanded, check that expanded package exists
      # if it doesn't, download basic package
      pk <- package
      pk.files <- grep(pk, basename(unique.files$name), fixed=T)
      if(pk=="expanded") {
        if(length(pk.files)==0) {
          pk <- "basic"
          pk.files <- grep(pk, basename(unique.files$name), fixed=T)
          message(paste("No expanded package found for site ",
                                        tmp.files[[i]]$data$siteCode, " and month ",
                                        tmp.files[[i]]$data$month,
                                        ". Basic package downloaded instead.",
                                        sep=""))
        }
      }

      # subset to package. expanded package can contain basic files,
      # have to account for that when downloading by file and not by zip
      if(length(intersect(pk.files, all.file))==0) {
        which.file <- all.file
      } else {
        which.file <- intersect(pk.files, all.file)
      }

      # check again for no files
      if(length(which.file)==0) {
        message(paste("No basic package files found for site",
                                      tmp.files[[i]]$data$siteCode,
                                      "and month", tmp.files[[i]]$data$month, sep=" "))
        next
      }

      zip.urls <- rbind(zip.urls, cbind(unique.files$name[which.file],
                                        unique.files$url[which.file],
                                        unique.files$size[which.file],
                                        rep(tmp.files[[i]]$data$release, 
                                            length(unique.files$name[which.file]))))

    # if downloading everything for product-site-month, instead of specific files, get zips
    } else {

      # check for packages section in response
      if("packages" %in% names(tmp.files[[i]]$data)) {
        
        # check for no files
        if(length(tmp.files[[i]]$data$packages)==0) {
          message(paste("No files found for site", tmp.files[[i]]$data$siteCode,
                                        "and month", tmp.files[[i]]$data$month, sep=" "))
          next
        }
        
        # if package==expanded, check that expanded package exists
        # if it doesn't, download basic package
        pk <- package
        if(pk=="expanded") {
          if(!pk %in% tmp.files[[i]]$data$packages$type) {
            pk <- "basic"
            message(paste("No expanded package found for site ",
                                          tmp.files[[i]]$data$siteCode, " and month ",
                                          tmp.files[[i]]$data$month,
                                          ". Basic package downloaded instead.",
                                          sep=""))
          }
        }

        # get the file name, and estimate the size
        z <- tmp.files[[i]]$data$packages$url[which(tmp.files[[i]]$data$packages$type==pk)]
        h <- getAPIHeaders(apiURL=z, token=token)
        
        # if no response, move to next silently - message will be printed by getAPIHeaders()
        if(is.null(h)) {
          next
        }
        
        flhd <- httr::headers(h)
        flnm <- gsub('\"', '', flhd$`content-disposition`, fixed=T)
        flnm <- gsub("inline; filename=", "", flnm, fixed=T)
        sz <- sum(tmp.files[[i]]$data$files$size[grep(pk, tmp.files[[i]]$data$files$name)], 
                  na.rm=T)
        rel <- rep(tmp.files[[i]]$data$release, length(flnm))
        
        zip.urls <- rbind(zip.urls, cbind(flnm, z, sz, rel))
        
      } else {
      
      # if no packages, look for pre-packaged zip files
      all.zip <- grep(".zip", tmp.files[[i]]$data$files$name, fixed=T)

      # check for no zips
      if(length(all.zip)==0) {
        
        message(paste("No zip files found for site", tmp.files[[i]]$data$siteCode,
                                      "and month", tmp.files[[i]]$data$month, sep=" "))
        next
        
      }

      # if package==expanded, check that expanded package exists
      # if it doesn't, download basic package
      pk <- package
      if(pk=="expanded") {
        if(length(grep(pk, tmp.files[[i]]$data$files$name))==0) {
          pk <- "basic"
          message(paste("No expanded package found for site ",
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
        message(paste("No basic package files found for site",
                                      tmp.files[[i]]$data$siteCode,
                                      "and month", tmp.files[[i]]$data$month, sep=" "))
        next
      }

      zip.urls <- rbind(zip.urls, cbind(tmp.files[[i]]$data$files$name[which.zip],
                                        tmp.files[[i]]$data$files$url[which.zip],
                                        tmp.files[[i]]$data$files$size[which.zip],
                                        rep(tmp.files[[i]]$data$release, 
                                            length(tmp.files[[i]]$data$files$name[which.zip]))))
    }
    }
  }

  # check for no files
  if(is.null(nrow(zip.urls))) {
    message(paste("No files found. This indicates either your internet connection failed, or the API is temporarily unavailable, or the data available for ",
               dpID,
               " are all hosted elsewhere. Check the data portal data.neonscience.org for outage alerts, and check the ",
               dpID, " data download page for external links.", sep=""))
    return(invisible())
  }

  # get size info
  zip.urls <- data.frame(zip.urls, row.names=NULL)
  colnames(zip.urls) <- c("name", "URL", "size", "release")
  zip.urls$URL <- as.character(zip.urls$URL)
  zip.urls$name <- as.character(zip.urls$name)
  zip.urls$size <- as.character(zip.urls$size)
  zip.urls$release <- as.character(zip.urls$release)
  
  # check for bad table name
  if(tabl!="all" & length(grep(paste("[.]", tabl, "[.]", sep=""), zip.urls$name))==0) {
    message(paste("No files found for ", tabl, ". Check that this is a valid table name in ", 
               dpID, ".", sep=""))
    return(invisible())
  }

  return(zip.urls)
}

