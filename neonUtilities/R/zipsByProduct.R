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
#' @param release The data release to be downloaded; either 'current' or the name of a release, e.g. 'RELEASE-2021'. 'current' returns the most recent release, as well as provisional data if include.provisional is set to TRUE. To download only provisional data, use release='PROVISIONAL'. Defaults to 'current'.
#' @param avg Deprecated; use timeIndex
#' @param timeIndex Either the string 'all', or the time index of data to download, in minutes. Only applicable to sensor (IS) data. Defaults to 'all'.
#' @param tabl Either the string 'all', or the name of a single data table to download. Defaults to 'all'.
#' @param check.size T or F, should the user approve the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.
#' @param include.provisional T or F, should provisional data be included in downloaded files? Defaults to F. See https://www.neonscience.org/data-samples/data-management/data-revisions-releases for details on the difference between provisional and released data.
#' @param cloud.mode T or F, are data transferred from one cloud environment to another? If T, this function returns a list of url paths to data files.
#' @param savepath The location to save the output files to
#' @param load T or F, are files saved locally or loaded directly? Used silently with loadByProduct(), do not set manually.
#' @param token User specific API token (generated within data.neonscience.org user accounts). Optional.
#' @param progress T or F, should progress bars be printed? Defaults to TRUE.

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
                          release="current", timeIndex="all", tabl="all", check.size=TRUE, 
                          include.provisional=FALSE, cloud.mode=FALSE, savepath=NA, 
                          load=FALSE, token=NA_character_, avg=NA, 
                          progress=TRUE) {

  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(package, "is not a valid package name. Package must be basic or expanded", sep=" "))
  }

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.00[0-9]{1}",dpID)[1]!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.00#", sep=" "))
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
    message('Input parameter avg is deprecated; use timeIndex to download by time interval.')
  } else {
    avg <- timeIndex
  }
  
  # error message if using timeIndex & tabl
  if(avg!="all" & tabl!="all") {
    stop("Either timeIndex or tabl can be specified, but not both.")
  }
  
  # check and warning message if using tabl=
  if(tabl!="all") {
    message(paste("Warning: Downloading only table ", tabl, ". Downloading by table is not recommended unless you are already familiar with the data product and its contents.\n", sep=""))
    if(!tabl %in% table_types$tableName) {
      message(paste("Warning: ", tabl, " is not in list of known tables. Download will be attempted, but check name and check neonUtilities for updates.\n", sep=""))
    } else {
      if(!dpID %in% table_types$productID[which(table_types$tableName==tabl)]) {
        message(paste(tabl, " is a table in ", 
                   paste(table_types$productID[which(table_types$tableName==tabl)], collapse=" "), 
                   ", not in ", dpID, 
                   ". Download will be attempted, but check for updates.\n", sep=""))
      }
      if("lab-current" %in% table_types$tableType[which(table_types$tableName==tabl)] | 
         "lab-all" %in% table_types$tableType[which(table_types$tableName==tabl)]) {
        stop(paste("Download by table is not available for lab metadata tables. To get the complete dataset for table ", 
                   tabl, ", download the most recently published site and month of data for ", 
                   paste(table_types$productID[which(table_types$tableName==tabl)], collapse=" or "), 
                   ".", sep=""))
      }
    }
  }

  # error for Phenocam data
  if(dpID %in% c("DP1.00033.001", "DP1.00042.001")) {
    stop(paste(dpID, "is a phenological image product, data are hosted by Phenocam.", sep=" "))
  }

  # error for Aeronet data
  if(dpID %in% c("DP1.00043.001")) {
    stop(paste("Spectral sun photometer (", dpID, ") data are hosted by Aeronet.", sep=""))
  }
  
  # error for DHP expanded package
  if(dpID=="DP1.10017.001" & package=="expanded") {
    stop("Digital hemispherical images expanded file packages exceed R download limits. Either download from the data portal, or download the basic package and use the URLs in the data to download the images themselves. Follow instructions in the Data Product User Guide for image file naming.")
  }

  # error message for individual SAE products
  if(dpID %in% c('DP1.00007.001','DP1.00010.001','DP1.00034.001','DP1.00035.001',
                 'DP1.00036.001','DP1.00037.001','DP1.00099.001','DP1.00100.001',
                 'DP2.00008.001','DP2.00009.001','DP2.00024.001','DP3.00008.001',
                 'DP3.00009.001','DP3.00010.001','DP4.00002.001','DP4.00007.001',
                 'DP4.00067.001','DP4.00137.001','DP4.00201.001','DP1.00030.001')) {
    stop(paste(dpID, 'is only available in the bundled eddy covariance data product. Download DP4.00200.001 to access these data.', sep=' '))
  }
  
  # redirect for met/precip data shared between terrestrial & aquatic sites
  # site=='all' not addressed, because in that case all available sites for the product are returned
  if(length(intersect(which(shared_aquatic$product==dpID), which(shared_aquatic$site %in% site)))>0) {
    message(paste("Some sites in your download request are aquatic sites where ", 
              dpID, " is collected at a nearby terrestrial site. The sites you requested, and the sites that will be accessed instead, are listed below:\n", 
              sep=""))
    site <- unlist(lapply(site, function(x) {
      if(x %in% shared_aquatic$site) {
        if(dpID %in% shared_aquatic$product[which(shared_aquatic$site==x)]) {
          terrSite <- unique(shared_aquatic$towerSite[which(shared_aquatic$site==x)])
          
          message(paste(x, " -> ", terrSite, sep=""))
          return(terrSite)
        }
        else {
          return(x)
        }
      }
      else {
        return(x)
      }
    }))
  }
  
  # redirect for chemistry data product bundles
  if(dpID %in% chem_bundles$product) {
    if(chem_bundles$homeProduct[which(chem_bundles$product==dpID)]==
       "depends") {
      stop("Root chemistry and isotopes have been bundled with the root biomass data. For root chemistry from Megapits, download DP1.10066.001. For root chemistry from periodic sampling, download DP1.10067.001.")
    } else {
      newDPID <- chem_bundles$homeProduct[which(chem_bundles$product==dpID)]
      stop(paste(dpID, " has been bundled with ", newDPID, 
                " and is not available independently. Please download ", 
                newDPID, sep=""))
    }
  }

  # redirect for veg structure and sediment data product bundles
  if(dpID %in% other_bundles$product & release!="RELEASE-2021") {
    newDPID <- other_bundles$homeProduct[which(other_bundles$product==dpID)]
    stop(paste("Except in RELEASE-2021, ", dpID, " has been bundled with ", newDPID, 
               " and is not available independently. Please download ", 
               newDPID, sep=""))
  }
  
  # redirect for air temp on buoy
  if(dpID=="DP1.20046.001" & !release %in% c("RELEASE-2021",
                                             "RELEASE-2022",
                                             "RELEASE-2023",
                                             "RELEASE-2024")) {
    newDPID <- "DP1.20271.001"
    stop(paste("Starting in 2025, ", dpID, " is included as a subset of product ", 
               newDPID, " and is not available independently. Please download ", 
               newDPID, sep=""))
  }
  
  # check for incompatible values of release= and include.provisional=
  if(release=="PROVISIONAL" & isFALSE(include.provisional)) {
    stop("Download request is for release=PROVISIONAL. To download PROVISIONAL data, enter input parameter include.provisional=TRUE.")
  }
  if(grepl(pattern="RELEASE", x=release) & isTRUE(include.provisional)) {
    warning(paste("Download request is for release=", release, 
                  " but include.provisional=TRUE. Only data in ", release, 
                  " will be downloaded."), 
            sep="")
  }
  
  # if token is an empty string, set to NA
  if(identical(token, "")) {
    token <- NA_character_
  }
  
  # check for token expiration
  token <- tokenCheck(token)
  
  # if in cloud mode, pass to queryFiles(). otherwise download
  if(isTRUE(cloud.mode)) {
    out <- queryFiles(dpID=dpID, site=site, startdate=startdate, 
                      enddate=enddate, package=package, release=release,
                      timeIndex=avg, tabl=tabl, metadata=TRUE, 
                      include.provisional=include.provisional, token=token)
    return(out)
  } else {
    
    # query the products endpoint for the product requested
    if(release=="current" | release=="PROVISIONAL") {
      prod.req <- getAPI(apiURL = paste("https://data.neonscience.org/api/v0/products/", 
                                        dpID, sep=""), token = token)
    } else {
      prod.req <- getAPI(apiURL = paste("https://data.neonscience.org/api/v0/products/", 
                                        dpID, "?release=", release, sep=""), token = token)
    }
    
    if(is.null(prod.req)) {
      return(invisible())
    }
    avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                                simplifyDataFrame=TRUE, flatten=TRUE)
    
    # error message if product not found
    if(!is.null(avail$error$status)) {
      if(release=="LATEST") {
        stop(paste("No data found for product ", dpID, 
                   ". LATEST data requested; check that token is valid for LATEST access.", sep=""))
      } else {
        if(any(grepl("Release not found", avail$error$detail))) {
          stop(paste("Release not found. Valid releases for product ", dpID, 
                     " are ", paste0(avail$data$validReleases, collapse=" "), sep=""))
        } else {
          stop(paste("No data found for product", dpID, sep=" "))
        }
      }
    }
    
    # check that token was used
    if(!is.na(token) & !is.null(prod.req$headers$`x-ratelimit-limit`)) {
      if(prod.req$headers$`x-ratelimit-limit`==200) {
        message('API token was not recognized. Public rate limit applied.')
      }
    }
    
    # error message if averaging interval is invalid
    if(avg!="all") {
      # if product is OS, proceed with normal download
      if(avail$data$productScienceTeamAbbr %in% c("TOS","AOS","AOP") |
         dpID %in% c("DP1.20267.001","DP1.00101.001","DP1.00013.001","DP1.00038.001")) {
        message(paste(dpID, " is not a streaming sensor (IS) data product; cannot subset by averaging interval. Proceeding to download all available data.\n",
                      sep=""))
        avg <- "all"
      } else {
        # exceptions for water quality, SAE, summary weather statistics
        if(dpID %in% c("DP1.20288.001","DP4.00001.001","DP4.00200.001")) {
          message(paste("Downloading by time interval is not available for ", dpID,
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
    
    # error message if nothing is available
    if(length(month.urls)==0) {
      stop("There are no data matching the search criteria.")
    }
    
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
      datelist <- regmatches(month.urls, regexpr("20[0-9]{2}-[0-9]{2}", month.urls))
      month.urls <- month.urls[which(datelist >= startdate)]
    }
    
    # error message if nothing is available
    if(length(month.urls)==0) {
      stop("There are no data at the selected date(s).")
    }
    
    if(!is.na(enddate)) {
      datelist <- regmatches(month.urls, regexpr("20[0-9]{2}-[0-9]{2}", month.urls))
      month.urls <- month.urls[which(datelist <= enddate)]
    }
    
    # error message if nothing is available
    if(length(month.urls)==0) {
      stop("There are no data at the selected date(s).")
    }
    
    zip.urls <- getZipUrls(month.urls, avg=avg, package=package, dpID=dpID, tabl=tabl,
                           release=release,
                           include.provisional=include.provisional,
                           token=token, progress=progress)
    if(is.null(zip.urls)) { return(invisible()) }
    zip.urls <- tidyr::drop_na(zip.urls)
    
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
      if(isTRUE(progress)) {
        message(paste0("Downloading files totaling approximately ", downld.size))
      }
    }
    
    # create folder in working directory or savepath to put files in
    if(is.na(savepath)) {
      filepath <- paste(getwd(), "/filesToStack", substr(dpID, 5, 9), sep="")
    } else {
      filepath <- paste(savepath, "/filesToStack", substr(dpID, 5, 9), sep="")
    }
    if(!dir.exists(filepath)) {
      dirc <- dir.create(filepath)
      if(!dirc) {
        stop("filesToStack directory could not be created. Check that savepath is a valid directory.")
      }
    } else {
      message(paste(filepath, " already exists. Download will proceed, but check for duplicate files.", sep=""))
    }
    
    # set user agent
    usera <- paste("neonUtilities/", utils::packageVersion("neonUtilities"), " R/", 
                   R.Version()$major, ".", R.Version()$minor, " ", commandArgs()[1], 
                   " ", R.Version()$platform, sep="")
    
    if(isTRUE(progress)) {
      message(paste("Downloading ", nrow(zip.urls), " files", sep=""))
      pb <- utils::txtProgressBar(style=3)
      utils::setTxtProgressBar(pb, 1/(nrow(zip.urls)-1))
    }
    
    j <- 1
    counter <- 1
    
    while(j <= nrow(zip.urls)) {
      
      if (counter > 2) {
        message(paste0("\nRefresh did not solve the issue. URL query for file ", zip.urls$name[j],
                       " failed. If all files fail, check data portal (data.neonscience.org/news) for possible outage alert.\n",
                       "If file sizes are large, increase the timeout limit on your machine: options(timeout=###)"))
        j <- j + 1
        counter <- 1
      } else {
        zip_out <- paste(filepath, zip.urls$name[j], sep="/")
        if(!dir.exists(dirname(zip_out))) {
          dir.create(dirname(zip_out))
        }
        if(!file.exists(substr(zip_out, 1, nchar(zip_out)-4)) || !file.exists(zip_out)) {
          if(is.na(token)) {
            t <- tryCatch(
              {
                suppressWarnings(downloader::download(zip.urls$URL[j], destfile=zip_out,
                                                      mode="wb", quiet=T,
                                                      headers=c("User-Agent"=usera)))
              }, error = function(e) { e } )
          } else {
            t <- tryCatch(
              {
                suppressWarnings(downloader::download(zip.urls$URL[j], destfile=zip_out,
                                                      mode="wb", quiet=T,
                                                      headers=c("User-Agent"=usera,
                                                                "X-API-Token"=token)))
              }, error = function(e) { e } )
          }
          
          if(inherits(t, "error")) {
            
            # use getAPI() to check for rate limit, then re-attempt download once with no other changes
            if(counter < 2) {
              message(paste0("\n", zip.urls$name[j], " could not be downloaded. Re-attempting."))
              testget <- getAPI(zip.urls$URL[j], token=token)
              
              if(is.na(token)) {
                t <- tryCatch(
                  {
                    suppressWarnings(downloader::download(zip.urls$URL[j], destfile=zip_out,
                                                          mode="wb", quiet=T,
                                                          headers=c("User-Agent"=usera)))
                  }, error = function(e) { e } )
              } else {
                t <- tryCatch(
                  {
                    suppressWarnings(downloader::download(zip.urls$URL[j], destfile=zip_out,
                                                          mode="wb", quiet=T,
                                                          headers=c("User-Agent"=usera,
                                                                    "X-API-Token"=token)))
                  }, error = function(e) { e } )
              }
              
              if(inherits(t, "error")) {
                counter <- counter + 1
              } else {
                j <- j + 1
                counter <- 1
              }
            } else {
              message(paste0("\n", zip.urls$name[j], " could not be downloaded. URLs may have expired. Refreshing URL list."))
              
              zip.urls <- quietMessages(getZipUrls(month.urls, avg=avg, package=package, tabl=tabl, 
                                                   include.provisional=include.provisional,
                                                   dpID=dpID, release=release, token=token))
              zip.urls <- tidyr::drop_na(zip.urls)
              
              counter <- counter + 1
            }
          } else {
            j <- j + 1
            counter <- 1
            if(isTRUE(progress)) {
              utils::setTxtProgressBar(pb, j/(nrow(zip.urls)-1))
            }
          }
        }
        
      }
    }
    
    if(isTRUE(progress)) {
      utils::setTxtProgressBar(pb, 1)
      close(pb)
    }
    
    if(isFALSE(load) & isTRUE(progress)) {
      message(paste0(nrow(zip.urls), " files successfully downloaded to ", filepath))
    }
    
  }
  
}


