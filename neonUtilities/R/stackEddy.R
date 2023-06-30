##############################################################################################
#' @title Extract eddy covariance data from HDF5 format

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Convert data of choice from HDF5 to tabular format. Specific to eddy covariance data product: DP4.00200.001
#'
#' @param filepath One of: a folder containing NEON EC H5 files, a zip file of DP4.00200.001 data downloaded from the NEON data portal, a folder of DP4.00200.001 data downloaded by the neonUtilities::zipsByProduct() function, or a single NEON EC H5 file [character]
#' @param level The level of data to extract; one of dp01, dp02, dp03, dp04 [character]
#' @param var The variable set to extract. Can be any of the variables in the "name" level or the "system" level of the H5 file; use the getVarsEddy() function to see the available variables. From the inputs, all variables from "name" and all variables from "system" will be returned, but if variables from both "name" and "system" are specified, the function will return only the intersecting set. This allows the user to, e.g., return only the pressure data ("pres") from the CO2 storage system ("co2Stor"), instead of all the pressure data from all instruments.  [character]
#' @param avg The averaging interval to extract, in minutes [numeric]
#' @param metadata Should the output include metadata from the attributes of the H5 files? Defaults to false. Even when false, variable definitions, issue logs, and science review flags will be included. [logical]
#' @param useFasttime Should the fasttime package be used to convert time stamps to time format? Decreases stacking time but can introduce imprecision at the millisecond level. Defaults to false. [logical]

#' @details Given a filepath containing H5 files of DP4.00200.001 data, extracts variables, stacks data tables over time, and joins variables into a single table.
#' For data product levels 2-4 (dp02, dp03, dp04), joins all available data, except for the flux footprint data in the expanded package.
#' For dp01, an averaging interval and a set of variable names must be provided as inputs.

#' @return A named list of data frames. One data frame per site, plus one data frame containing the metadata (objDesc) table and one data frame containing units for each variable (variables).

#' @examples
#' \dontrun{
#' # To extract and merge Level 4 data tables, where data files are in the working directory
#' flux <- stackEddy(filepath=getwd(), level='dp04', var=NA, avg=NA)
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-05-29)
#     partially adapted from eddy4R.base::def.hdf5.extr() authored by David Durden
##############################################################################################

stackEddy <- function(filepath, 
                      level="dp04", 
                      var=NA, 
                      avg=NA,
                      metadata=FALSE,
                      useFasttime=FALSE) {
  
  # first check for rhdf5 package
  if(!requireNamespace("rhdf5", quietly=T)) {
    stop("Package rhdf5 is required for this function to work.
         \nrhdf5 is a Bioconductor package. To install, use:\ninstall.packages('BiocManager')\nBiocManager::install('rhdf5')\n")
  }
  
  # also check for fasttime package, if used
  if(useFasttime & !requireNamespace("fasttime", quietly=T)) {
    stop("Parameter useFasttime is TRUE but fasttime package is not installed. Install and re-try.")
  }
  
  files <- NA
  releases <- NA
  # check for vector of files as input
  if(length(filepath)>1) {
    if(length(grep(".h5$", filepath))==length(filepath)) {
      files <- filepath
    } else {
      stop("Input list of files must be .h5 files.")
    }
    if(any(!file.exists(files))) {
      stop("Files not found in specified filepaths. Check that the input list contains the correct filepaths.")
    }
  }
  
  # get list of files, unzipping if necessary
  if(any(is.na(files)) & identical(substring(filepath, nchar(filepath)-3, nchar(filepath)), ".zip")) {
    outpath <- gsub(".zip", "", filepath)
    if(!dir.exists(outpath)) {
      dir.create(outpath)
    }
    if(length(grep(".zip", utils::unzip(filepath, list=TRUE)$Name, fixed=TRUE))>0) {
      utils::unzip(filepath, exdir=outpath)
    } else {
      # get release info before discarding file paths
      allDirs <- dirname(utils::unzip(filepath, list=TRUE)$Name)
      dir.splitName <- strsplit(allDirs, split = "\\.")
      release.files <- unlist(lapply(dir.splitName, 
                                       FUN=function(x){grep(pattern="RELEASE|PROVISIONAL|LATEST", 
                                                            x=x, value=TRUE)}))
      relind <- grep(pattern="RELEASE|PROVISIONAL|LATEST", x=allDirs)
      release.status <- cbind(allDirs[relind], release.files)
      names(release.status) <- c("name","release")
      releases <- unique(release.files)
      
      # write release status file
      utils::write.csv(release.status, file=paste(outpath, "/release_status_",
                                           paste0(gsub("\\D", "", Sys.time()), 
                                                  collapse=""), ".csv", sep=""),
                       row.names=FALSE)
      
      # unzip
      utils::unzip(filepath, exdir=outpath, junkpaths=T)
    }
    filepath <- outpath
  }
  
  # allow for a single H5 file
  if(any(is.na(files)) & identical(substring(filepath, nchar(filepath)-2, nchar(filepath)), ".h5")) {
    files <- filepath
  } else {
    if(any(is.na(files))) {
      files <- list.files(filepath, recursive=F, full.names=T)
    }
  }
  
  # unzip files if necessary
  if(length(grep(".zip", files))==length(files)) {
    lapply(files, function(x) {
      utils::unzip(x, exdir=filepath)
    })
    files <- list.files(filepath, recursive=F, full.names=T)
  }
  
  # after unzipping, check for .gz
  if(length(grep(".h5.gz", files))>0) {
    lapply(files[grep(".h5.gz", files)], function(x) {
      R.utils::gunzip(x)
    })
    files <- list.files(filepath, recursive=F, full.names=T)
  }
  
  # need the H5 files for data extraction and the SRF tables
  scienceReviewList <- unique(files[grep("science_review_flags", files)])
  files <- files[grep(".h5$", files)]
  
  # check for duplicate files and use the most recent
  fileDups <- gsub("[0-9]{8}T[0-9]{6}Z.h5", "", files)
  if(any(base::duplicated(fileDups))) {
    maxFiles <- character()
    for(i in unique(fileDups)) {
      maxFiles <- c(maxFiles, 
                    max(files[grep(i, files)]))
    }
    files <- maxFiles
  }
  
  # check for no files
  if(identical(length(files), as.integer(0))) {
    stop("No .h5 files found in specified file path. Check the inputs and file contents.")
  }
  
  # check for original zip files and use to determine releases and citations
  if(all(is.na(releases))) {
    allFiles <- list.files(filepath, recursive=TRUE, full.names=TRUE)
    relfl <- grep(pattern="release_status", x=allFiles, value=TRUE)
    if(length(relfl)==1) {
      reltab <- data.table::fread(relfl,
                                  header=TRUE, encoding="UTF-8")
      releases <- unique(reltab$release)
    } else {
      splitName <- strsplit(allFiles, split = "\\.")
      releases <- unique(unlist(lapply(splitName, 
                                       FUN=function(x){grep(pattern="RELEASE|PROVISIONAL|LATEST", 
                                                            x=x, value=TRUE)})))
    }
  }
  
  # get DOIs and generate citation(s)
  citP <- NA
  citR <- NA
  if("PROVISIONAL" %in% releases) {
    citP <- try(getCitation(dpID="DP4.00200.001", release="PROVISIONAL"), silent=TRUE)
    if(inherits(citP, "try-error")) {
      citP <- NA
    }
  }
  if(length(grep("RELEASE", releases))==0) {
    releases <- releases
  } else {
    if(length(grep("RELEASE", releases))>1) {
      stop("Attempting to stack multiple data releases together. This is not appropriate, check your input data.")
    } else {
      rel <- releases[grep("RELEASE", releases)]
      citR <- try(getCitation(dpID="DP4.00200.001", release=rel), silent=TRUE)
      if(inherits(citR, "try-error")) {
        citR <- NA
      }
    }
  }
  
  # determine basic vs expanded package and check for inconsistencies
  pkglist <- regmatches(files, regexpr("basic", files))
  if(length(pkglist)==length(files)) {
    pkg <- "basic"
  } else {
    if(length(pkglist)==0) {
      pkglist <- regmatches(files, regexpr("expanded", files))
      pkg <- "expanded"
    }
  }
  if(length(pkglist)!=length(files)) {
    stop("File path contains both basic and expanded package files, these can't be stacked together.")
  }
  if(pkg=="basic" & metadata) {
    message("For the basic package, attribute metadata are the values from the beginning of the month. To get attributes for each day, use the expanded package.")
  }
  
  # make empty, named list for the data tables
  tableList <- vector("list", length(files))
  names(tableList) <- substring(basename(files), 1, nchar(basename(files))-3)
  
  # set up progress bar
  writeLines(paste0("Extracting data"))
  pb <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb, 0)

  # extract data from each file
  for(i in 1:length(files)) {
    
    listObj <- base::try(rhdf5::h5ls(files[i]), silent=T)
    
    if(inherits(listObj, "try-error")) {
      cat(paste("\n", paste(files[i], " could not be read.", sep="")))
      next
    }
    
    listDataObj <- listObj[listObj$otype == "H5I_DATASET",]
    listDataName <- base::paste(listDataObj$group, listDataObj$name, sep = "/")
    listObjSpl <- tidyr::separate(listDataObj, col="group", 
                                  into=c(NA, "site", "level", "category", "system", 
                                         "horvertmi", "subsys"), 
                                  sep="/", fill="right")
    
    # filter by variable/level selections
    levelInd <- grep(level, listDataName)
    
    if(level!="dp04" & level!="dp03" & level!="dp02" & !all(is.na(var))) {
      if(length(which(listObjSpl$system %in% var))>0) {
        if(length(which(listDataObj$name %in% var))>0) {
          varInd <- base::intersect(which(listDataObj$name %in% var), 
                                    which(listObjSpl$system %in% var))
        } else {
          varInd <- which(listObjSpl$system %in% var)
        }
      } else {
        if(length(which(listDataObj$name %in% var))>0) {
          varInd <- which(listObjSpl$name %in% var)
        } else {
          stop(paste("No data found for variables ", paste(var, collapse=" "), sep=""))
        }
      }
      
    } else {
      varInd <- 1:length(listDataName)
    }
    if(level!="dp04" & level!="dp03" & level!="dp02" & !is.na(avg)) {
      avgInd <- grep(paste(avg, "m", sep=""), listDataName)
    } else {
      if(level=="dp01") {
        stop("If level=='dp01', avg is a required input.")
      } else {
        avgInd <- 1:length(listDataName)
      }
    }
    
    # exclude footprint grid data from expanded packages
    if(length(grep("foot/grid", listDataName))>0) {
      gridInd <- grep("foot/grid", listDataName, invert=T)
    } else {
      gridInd <- 1:length(listDataName)
    }
    
    # index that includes all filtering criteria
    ind <- intersect(intersect(levelInd, intersect(varInd, avgInd)), gridInd)
    
    # check that you haven't filtered to nothing
    if(length(ind)==0) {
      stop(paste("There are no data meeting the criteria level ", level, 
                 ", averaging interval ", avg, ", and variables ", 
                 paste(var, collapse=" "), sep=""))
    }
    
    listDataName <- listDataName[ind]
    
    # add extracted data to the list
    tableList[[i]] <- base::lapply(listDataName, rhdf5::h5read, 
                                 file=files[i], read.attributes=T)
    base::names(tableList[[i]]) <- substring(listDataName, 2, nchar(listDataName))
    
    utils::setTxtProgressBar(pb, i/length(files))
    
  }
  close(pb)
  
  # get variable units
  variables <- getVariablesEddy(tableList)
  
  # convert all time stamps to time format, then filter out instances with:
  # 1) only one record for a day
  # 2) all values = NaN
  # these are instances when a sensor was offline, and they don't join correctly
  err <- FALSE
  for(ti in 1:length(tableList)) {
    tableList[[ti]] <- lapply(tableList[[ti]], function(x) {
      tabtemp <- eddyStampCheck(x, useFasttime=useFasttime)
      if(tabtemp[[2]]) {
        err <- TRUE
      }
      return(tabtemp[[1]])
    })
  }
  if(err) {
    message("Some time stamps could not be converted. Variable join may be affected; check data carefully for disjointed time stamps.")
  }
  
  # within each site-month set, join matching tables
  # create empty, named list for the tables
  mergTableList <- vector("list", length(tableList))
  names(mergTableList) <- names(tableList)
  
  # set up progress bar
  writeLines(paste0("Joining data variables by file"))
  pb2 <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb2, 0)
  
  for(tb in 1:length(tableList)) {
    
    namesSpl <- data.frame(matrix(unlist(strsplit(names(tableList[[tb]]), split="/", fixed=T)), 
                                  nrow=length(names(tableList[[tb]])), byrow=T))
    nc <- ncol(namesSpl)
    
    # dp01 and dp02 have sensor levels
    if(nc==6) {
      
      # find sensor level sets
      sens <- unique(namesSpl$X5)
      
      # create list for merged tables, nested in mergTableList
      mergTableList[[tb]] <- vector("list", length(sens))
      names(mergTableList[[tb]]) <- sens
      
      # join for each sensor level
      for(si in sens) {
        
        # get all tables for a sensor level and get variable names
        inds <- which(namesSpl$X5==si)
        tbsub <- tableList[[tb]][inds]
        nmsub <- paste(namesSpl$X3[inds], namesSpl$X4[inds], namesSpl$X6[inds], sep=".")
        
        # get consensus time stamps
        mergTabl <- timeStampSet(tbsub)
        
        # rename and merge
        for(ib in 1:length(tbsub)) {
          names(tbsub[[ib]])[grep("timeBgn|timeEnd", names(tbsub[[ib]]), invert=TRUE)] <-
            paste(nmsub[ib], names(tbsub[[ib]])[grep("timeBgn|timeEnd", names(tbsub[[ib]]), invert=TRUE)],
                  sep=".")
          
          tbsub[[ib]] <- data.table::as.data.table(tbsub[[ib]][,-which(names(tbsub[[ib]])=="timeEnd")])
          
          mergTabl <- base::merge(mergTabl, 
                                  tbsub[[ib]],
                                  by="timeBgn", all.x=T, all.y=F)
        }
        
        mergTableList[[tb]][[si]] <- mergTabl
        
      }
      
    } else {
      
      # dp03 and dp04 - no sensor levels
      
      # get consensus time stamps
      tbsub <- tableList[[tb]]
      mergTabl <- timeStampSet(tbsub)
      nmsub <- paste(namesSpl$X3, namesSpl$X4, namesSpl$X5, sep=".")
      
      # rename and merge
      for(ibl in 1:length(tbsub)) {
        names(tbsub[[ibl]])[grep("timeBgn|timeEnd", names(tbsub[[ibl]]), invert=TRUE)] <-
          paste(nmsub[ibl], names(tbsub[[ibl]])[grep("timeBgn|timeEnd", names(tbsub[[ibl]]), invert=TRUE)],
                sep=".")
        
        tbsub[[ibl]] <- data.table::as.data.table(tbsub[[ibl]][,-which(names(tbsub[[ibl]])=="timeEnd")])
        
        mergTabl <- base::merge(mergTabl, 
                                tbsub[[ibl]],
                                by="timeBgn", all.x=T, all.y=F)
      }
      
      mergTableList[[tb]] <- mergTabl
      
    }
    utils::setTxtProgressBar(pb2, tb/length(tableList))
    
  }
  close(pb2)
  
  # for dp01 and dp02, stack tower levels and calibration gases
  if(level=="dp01" | level=="dp02") {
    
    for(ni in 1:length(mergTableList)) {
      for(mi in 1:length(mergTableList[[ni]])) {

        # get indices from table names
        verSpl <- tidyr::separate_wider_delim(data.frame(x=names(mergTableList[[ni]])[mi]), 
                                              cols="x",
                                              delim="_", names=c("hor", "ver", "tmi"),
                                              too_few="align_end")
        
        # append columns with indices
        verticalPosition <- rep(verSpl$ver, nrow(mergTableList[[ni]][[mi]]))
        horizontalPosition <- rep(verSpl$hor, nrow(mergTableList[[ni]][[mi]]))
        mergTableList[[ni]][[mi]] <- cbind(horizontalPosition, verticalPosition, mergTableList[[ni]][[mi]])
      }
    }
    
    # stack everything within site and month into a new list
    verMergList <- vector("list", length(mergTableList))
    names(verMergList) <- names(mergTableList)
    
    for(vi in 1:length(verMergList)) {
      verMergList[[vi]] <- data.table::rbindlist(mergTableList[[vi]], fill=TRUE)
    }
    
  } else {
    
    verMergList <- mergTableList
    
  }
  
  # check for weird isotope joining
  if(level=="dp01") {
    if(avg==9 | avg=="9" | avg=="09") {
      allNm <- unique(unlist(lapply(X=verMergList, FUN=names)))
      if(length(grep(pattern="dlta13CCo2", x=allNm))>0 &
         length(grep(pattern="dlta18OH2o|dlta2HH2o", x=allNm))>0) {
        dup.iso.list <- lapply(X=verMergList, FUN=function(x){
          dup.temp <- base::as.difftime(x$timeEnd[1:20] - x$timeBgn[1:20])
          dup.diff <- mean(base::as.numeric(dup.temp, units="secs"), na.rm=TRUE)
          return(dup.diff<537)
        })
        dup.iso <- any(unlist(dup.iso.list))
        if(dup.iso) {
          warning("Stacking appears to include both carbon and water isotopes, with inconsistent time stamps. Carbon isotopes are measured every 6 minutes, water isotopes every 9 minutes. This issue affects RELEASE-2023 and provisional data published between RELEASE-2023 and RELEASE-2024. Check data carefully. The recommended workflow is to stack the carbon and water isotope data separately.")
        }
      }
    }
  }
  
  # set up tables for data and metadata
  # stack months within each site
  sites <- regmatches(x=names(verMergList), m=regexpr(pattern="[.][A-Z]{4}[.]", text=names(verMergList)))
  sites <- gsub(pattern=".", replacement="", x=sites, fixed=TRUE)
  sites <- unique(sites)
  
  # which attributes should be extracted?
  vNames <- unique(unlist(lapply(verMergList, names)))
  if(metadata) {
    if(any(grepl("rtioMoleDryCo2Vali", vNames))) {
      varNames <- c(sites, "variables", "objDesc", "siteAttributes", 
                    "codeAttributes", "validationAttributes", 
                    "issueLog", "scienceReviewFlags")
      numTabs <- 7
    } else {
      varNames <- c(sites, "variables", "objDesc", "siteAttributes", 
                    "codeAttributes", "issueLog", "scienceReviewFlags")
      numTabs <- 6
    }
    
  } else {
    varNames <- c(sites, "variables", "objDesc", 
                  "issueLog", "scienceReviewFlags")
    numTabs <- 4
  }
  
  # set up progress bar
  writeLines(paste0("Stacking files by month"))
  pb3 <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb3, 0)
  
  # set up final list
  varMergList <- vector("list", length(sites)+numTabs)
  names(varMergList) <- varNames
  
  for(o in 1:length(sites)) {
    
    mergSubList <- verMergList[grep(sites[o], names(verMergList))]
    varMergList[[o]] <- data.table::rbindlist(mergSubList, fill=TRUE)
    if(level=="dp01" | level=="dp02") {
      varMergList[[o]] <- as.data.frame(varMergList[[o]][order(varMergList[[o]]$horizontalPosition, 
                                                 varMergList[[o]]$verticalPosition, 
                                                 varMergList[[o]]$timeBgn),])
    } else {
      varMergList[[o]] <- as.data.frame(varMergList[[o]][order(varMergList[[o]]$timeBgn),])
    }
    
    utils::setTxtProgressBar(pb3, o/length(sites))
  }
  close(pb3)
  
  # attributes, objDesc, SRF table, and issue log
  writeLines(paste0("Getting metadata tables"))
  pb4 <- utils::txtProgressBar(style=3)
  utils::setTxtProgressBar(pb4, 0)
  
  # attributes are only included if metadata==TRUE
  if(metadata) {
    
    # get site attributes
    siteAttributes <- vector(mode="list", length=length(sites))
    for(p in 1:length(sites)) {
      siteAttr <- lapply(files[grep(sites[p], files)], getAttributes, sit=sites[p], type="site")
      siteAttributes[[p]] <- data.table::rbindlist(siteAttr, fill=T)
    }
    siteAttributes <- data.table::rbindlist(siteAttributes, fill=T)
    varMergList[["siteAttributes"]] <- siteAttributes
    utils::setTxtProgressBar(pb4, 0.15)
    
    # get attributes from root level
    codeAttributes <- vector(mode="list", length=length(sites))
    for(p in 1:length(sites)) {
      codeAttr <- lapply(files[grep(sites[p], files)], getAttributes, sit=sites[p], type="root")
      codeAttributes[[p]] <- data.table::rbindlist(codeAttr, fill=T)
    }
    codeAttributes <- data.table::rbindlist(codeAttributes, fill=T)
    varMergList[["codeAttributes"]] <- codeAttributes
    utils::setTxtProgressBar(pb4, 0.25)
    
    # get CO2 validation attributes if CO2 variables were extracted
    if("validationAttributes" %in% varNames) {
      
      valAttributes <- vector(mode="list", length=length(sites))
      for(p in 1:length(sites)) {
        tbls <- tableList[grep(sites[p], names(tableList))]
        fls <- grep(sites[p], files, value=T)
        valAttr <- vector(mode="list", length=length(fls))
        for(q in 1:length(fls)) {
          valAttr[[q]] <- getAttributes(fls[q], sit=sites[p], type="val",
                                        valName=grep("rtioMoleDryCo2Vali", 
                                                     names(tbls[[q]]), value=T))
        }
        valAttributes[[p]] <- data.table::rbindlist(valAttr, fill=T)
      }
      valAttributes <- data.table::rbindlist(valAttributes, fill=T)
      varMergList[["validationAttributes"]] <- valAttributes
      utils::setTxtProgressBar(pb4, 0.35)
      
    }
    
  }
  
  # get one objDesc table and add it and variables table to list
  objDesc <- base::try(rhdf5::h5read(files[1], name="//objDesc"), silent=T)
  # if processing gets this far without failing, don't fail here, just return data without objDesc table
  if(inherits(objDesc, "try-error")) {
    objDesc <- NA
  }
  varMergList[["variables"]] <- variables
  varMergList[["objDesc"]] <- objDesc
  
  utils::setTxtProgressBar(pb4, 0.5)
  
  # get issue log
  if(!curl::has_internet()) {
    message("No internet connection, issue log file not accessed. Issue log can be found on the data product details pages.")
  } else {
    # token not used here, since token is not otherwise used/accessible in this function
    varMergList[["issueLog"]] <- getIssueLog(dpID="DP4.00200.001")
  }
  
  utils::setTxtProgressBar(pb4, 0.75)
  
  # aggregate the science_review_flags files
  if(length(scienceReviewList)>0) {
    outputScienceReview <- data.table::rbindlist(lapply(scienceReviewList, 
                                                                   function(x) {
                                                                     
              outTbl <- data.table::fread(x, header=TRUE, encoding="UTF-8", keepLeadingZeros = TRUE,
                                      colClasses = list(character = c('startDateTime','endDateTime',
                                                                      'createDateTime',
                                                                      'lastUpdateDateTime')))
                            if(identical(nrow(outTbl), as.integer(0))) {
                                return()
                            }
                            return(outTbl)
                  }), fill=TRUE)
    
    # remove duplicates
    outputScienceReview <- unique(outputScienceReview)
    
    # check for non-identical duplicates with the same ID and keep the most recent one
    if(length(unique(outputScienceReview$srfID))!=nrow(outputScienceReview)) {
      dupRm <- numeric()
      rowids <- 1:nrow(outputScienceReview)
      origNames <- colnames(outputScienceReview)
      outputScienceReview <- cbind(rowids, outputScienceReview)
      for(k in unique(outputScienceReview$srfID)) {
        scirvwDup <- outputScienceReview[which(outputScienceReview$srfID==k),]
        if(nrow(scirvwDup)>1) {
          dupRm <- c(dupRm, 
                     scirvwDup$rowids[which(scirvwDup$lastUpdateDateTime!=max(scirvwDup$lastUpdateDateTime))])
        }
      }
      if(length(dupRm)>0) {
        outputScienceReview <- outputScienceReview[-dupRm,origNames]
      } else {
        outputScienceReview <- outputScienceReview[,origNames]
      }
    }
    varMergList[["scienceReviewFlags"]] <- outputScienceReview
  } else {
    varMergList <- varMergList[-grep("scienceReviewFlags", names(varMergList))]
  }
  
  utils::setTxtProgressBar(pb4, 1)
  close(pb4)
  
  # add citations to output
  if(!is.na(citP)) {
    vlen <- length(varMergList)
    varMergList[[vlen+1]] <- citP
    names(varMergList)[vlen+1] <- "citation_00200_PROVISIONAL"
  }
  if(!is.na(citR)) {
    vlen <- length(varMergList)
    varMergList[[vlen+1]] <- citR
    names(varMergList)[vlen+1] <- paste("citation_00200_", rel, sep="")
  }
  
  return(varMergList)
}
