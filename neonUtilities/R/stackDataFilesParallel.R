##############################################################################################
#' @title Join data files in a unzipped NEON data package by table type

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Given a folder of unzipped files (unzipped NEON data file), do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.

#' @param folder The location of the data
#' @param nCores The number of cores to parallelize the stacking procedure. To automatically use the maximum number of cores on your machine we suggest setting 'nCores=parallel::detectCores()'. By default it is set to a single core. If the files are less than 25000 bytes the userdefined nCores will be overridden to a single core.
#' @param dpID The data product identifier
#' @return One file for each table type is created and written.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2017-07-02 (Christine Laney): Original creation
#   2018-04-03 (Christine Laney):
#     * Swap read.csv() for data.table::fread() for faster data table loading
#     * Swap data.table::rbind() for dplyr::join() for faster table joins
#     * Remove join messages, replace with progress bars
#     * Provide comparison of number of rows expected per stacked table vs number of row in final table
#   2018-04-13 (Christine Laney):
#     * Continuous stream discharge (DP4.00130.001) is an OS product in IS format. Adjusted script to stack properly.
#   2019-11-14 (Nathan Mietkiewicz)
#     * Parallelized the function
##############################################################################################

stackDataFilesParallel <- function(folder, nCores=1, dpID){
  
  starttime <- Sys.time()
  requireNamespace('stringr', quietly = TRUE)
  requireNamespace('dplyr', quietly = TRUE)
  requireNamespace("magrittr", quietly = TRUE)
  requireNamespace('data.table', quietly = TRUE)
  
  messages <- character()
  
  # get the in-memory list of table types (site-date, site-all, etc.). This list must be updated often.
  #data("table_types")
  ttypes <- table_types[which(table_types$productID==dpID),]
  dpnum <- substring(dpID, 5, 9)
  
  # filenames without full path
  filenames <- findDatatables(folder = folder, fnames = F)
  
  # filenames with full path
  filepaths <- findDatatables(folder = folder, fnames = T)
  
  # make a list, where filenames are the keys to the filepath values
  filelist <- stats::setNames(as.list(filepaths), filenames)
  
  datafls <- filelist
  
  # if there are no datafiles, exit
  if(length(datafls) == 0){
    stop("No data files are present in specified file path.")
  }
  
  # if there is just one data file (and thus one table name), copy file into stackedFiles folder
  if(length(datafls) == 1){
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    file.copy(from = datafls[1][[1]], to = "/stackedFiles")
    m <- 0
    n <- 1
  }
  
  # if there is more than one data file, stack files
  if(length(datafls) > 1){
    
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    
    # detecting table types by file format, then checking against table_types
    # reducing dependency on table_types updating
    tableForm <- findTablesByFormat(names(datafls))
    tables <- tableForm$tableName
    
    # check against table_types. use grep() since format only identifies to 'lab'
    mis <- 0
    newtables <- character()
    for(j in 1:nrow(tableForm)) {
      ind <- which(ttypes$tableName==tableForm$tableName[j])
      if(length(ind)==0) {
        newtables <- rbind(newtables, tableForm[j,])
        next
        }
      indt <- grep(tableForm$tableType[j], ttypes$tableType[ind])
      if(length(indt)==0) {
        mis <- mis+1
      }
    }
    
    # for new tables, always use inferred types. for mismatches, decide based on publication dates
    if(!is.null(nrow(newtables))) {
      cat("Table(s)", newtables[,1], "are unexpected. Stacking will proceed based on inferred table format; check for updates to neonUtilities.\n")
      ttypes <- plyr::rbind.fill(ttypes, newtables)
    }
    
    if(mis>0) {
      dats <- character()
      filespl <- strsplit(filenames, "\\.")
      for(k in 1:length(filespl)) {
        dats <- c(dats, filespl[[k]][length(filespl[[k]])-1])
      }
      dats <- as.POSIXct(dats, format="%Y%m%dT%H%M%SZ")
      if(min(dats, na.rm=T) > utils::packageDate("neonUtilities")) {
        cat("Downloaded data formats do not match expected formats. Data publication dates are more recent than neonUtilities version. Stacking will proceed using inference from downloaded data formats. Check results carefully, and check for updates to neonUtilities.\n")
        ttypes <- tableForm
      } else {
        if(max(dats, na.rm=T) < utils::packageDate("neonUtilities")) {
          cat("Downloaded data formats do not match expected formats. neonUtilities version is more recent than data publication dates. Stacking will proceed based on neonUtilities expectations. Check results carefully, and consider re-downloading data.\n")
        } else {
        cat("Downloaded data formats do not match expected formats. Data publication dates include both older and more recent data than neonUtilities package version. Stacking will proceed based on neonUtilities expectations. Check results carefully, and check for updates to neonUtilities.\n")
      }
      }
    }
    
    n <- 0
    m <- 0
    
    # find external lab tables (lab-current, lab-all) and stack the most recently published file from each lab
    labTables <- tables[which(tables %in% ttypes$tableName[grep("lab", ttypes$tableType)])]
    if(length(labTables)>0){
      externalLabs <- unique(names(datafls)[grep(paste(paste('[.]', labTables, '[.]', sep=''), 
                                                       collapse='|'), names(datafls))])
      
      for(j in 1:length(labTables)) {
        
        tablesj <- externalLabs[grep(paste("[.]", labTables[j], "[.]", sep=""), externalLabs)]
        if(length(tablesj)>0) {

          writeLines(paste0("Stacking table ", labTables[j]))
          
          outputLab <- data.table::rbindlist(pbapply::pblapply(as.list(tablesj), function(x, filepaths) {
            labpath <- getRecentPublication(filepaths[grep(x, filepaths)])
            outputj <- data.table::fread(labpath[[1]], header=TRUE, encoding="UTF-8")
            outputj$publicationDate <- rep(labpath[[2]], nrow(outputj))
            return(outputj)
            }, filepaths=filepaths), fill=TRUE)
        
        data.table::fwrite(outputLab, paste0(folder, "/stackedFiles/", labTables[j], ".csv"))
        n <- n + 1
        }
      }
      tables <- setdiff(tables, labTables)
    }

    # copy variables and validation files to /stackedFiles using the most recent publication date
    if(TRUE %in% stringr::str_detect(filepaths,'variables.20')) {
      varpath <- getRecentPublication(filepaths[grep("variables.20", filepaths)])[[1]]
      variables <- getVariables(varpath)   # get the variables from the chosen variables file
      v <- suppressWarnings(data.table::fread(varpath, sep=','))
      vlist <- base::split(v, v$table)
    }
    
    if(TRUE %in% stringr::str_detect(filepaths,'validation')) {
      valpath <- getRecentPublication(filepaths[grep("validation", filepaths)])[[1]]
      file.copy(from = valpath, to = paste0(folder, "/stackedFiles/validation_", dpnum, ".csv"))
      messages <- c(messages, "Copied the most recent publication of validation file to /stackedFiles")
      m <- m + 1
    }

    # copy categoricalCodes file to /stackedFiles using the most recent publication date
    if(TRUE %in% stringr::str_detect(filepaths,'categoricalCodes')) {
      lovpath <- getRecentPublication(filepaths[grep("categoricalCodes", filepaths)])[[1]]
      file.copy(from = lovpath, to = paste0(folder, "/stackedFiles/categoricalCodes_", dpnum, ".csv"))
      messages <- c(messages, "Copied the most recent publication of categoricalCodes file to /stackedFiles")
      m <- m + 1
    }
    
    # get most recent sensor_positions file for each site and stack
    if(TRUE %in% stringr::str_detect(filepaths,'sensor_position')) {
      sensorPositionList <- unique(filepaths[grep("sensor_position", filepaths)])
      uniqueSites <- unique(basename(sensorPositionList)) %>%
        stringr::str_split('\\.') %>%
        lapply(`[`, 3) %>%
        unlist() %>% 
        unique(.)

      outputSensorPositions <- data.table::rbindlist(pbapply::pblapply(as.list(uniqueSites), function(x, sensorPositionList) {
        
        sppath <- getRecentPublication(sensorPositionList[grep(x, sensorPositionList)])[[1]]
        outTbl <- data.table::fread(sppath, header=TRUE, encoding="UTF-8", keepLeadingZeros = TRUE,
                                    colClasses = list(character = c('HOR.VER','start','end',
                                                                    'referenceStart',
                                                                    'referenceEnd'))) %>%
          makePosColumns(., sppath, x)
        return(outTbl)
      }, sensorPositionList=sensorPositionList), fill=TRUE)
      
      data.table::fwrite(outputSensorPositions, paste0(folder, "/stackedFiles/sensor_positions_", dpnum, ".csv"))
      messages <- c(messages, "Merged the most recent publication of sensor position files for each site and saved to /stackedFiles")
      m <- m + 1
    }
    
    if(nCores > parallel::detectCores()) {
      stop(paste("The number of cores selected exceeds the available cores on your machine.  The maximum number of cores allowed is", parallel::detectCores(), "not", nCores))
    }
    
    # Make a decision on parallel processing based on the total size of directories or whether there are lots of 1_minute files
    directories <- sum(file.info(grep(list.files(folder, full.names=TRUE, pattern = 'NEON'), pattern = "stacked|*.zip", invert=TRUE, value=TRUE))$size)
    if(directories <= 25000 | nCores == 1) {
      cl <- 1
      if(nCores == 1) {
        writeLines(paste0("Stacking operation across a single core."))
      } else {
        writeLines(paste0("File requirements do not meet the threshold for automatic parallelization. Running on single core."))
      }
    } else {
      cl <- parallel::makeCluster(getOption("cl.cores", nCores),
                                  setup_strategy='sequential')
      parallel::clusterEvalQ(cl, c(library(dplyr), library(magrittr), library(data.table))) 
      writeLines(paste0("Parallelizing stacking operation across ", nCores, " cores."))
      # If error, crash, or completion , closes all clusters
      suppressWarnings(on.exit(parallel::stopCluster(cl)))
      }
    
    for(i in 1:length(tables)){
      tbltype <- unique(ttypes$tableType[which(ttypes$tableName == gsub(tables[i], pattern = "_pub", replacement = ""))])
      variables <- getVariables(varpath)  # get the variables from the chosen variables file

      writeLines(paste0("Stacking table ", tables[i]))
      file_list <- sort(union(filepaths[grep(paste(".", tables[i], "_pub.", sep=""), filepaths, fixed=T)],
                         filepaths[grep(paste(".", tables[i], ".", sep=""), filepaths, fixed=T)]))

      if(tbltype == "site-all") {
        sites <- as.list(unique(substr(basename(file_list), 10, 13)))

        tblfls <- lapply(sites, function(j, file_list) {
          tbl_list <- getRecentPublication(file_list[grep(j, file_list)])[[1]]
        }, file_list=file_list)
        
      }
      if(tbltype == "site-date") {
        tblfls <- file_list
      }
      
      stackedDf <- do.call(plyr::rbind.fill, pbapply::pblapply(tblfls, function(x, tables_i, variables, assignClasses, 
                                                      makePosColumns) {

        stackedDf <- suppressWarnings(data.table::fread(x, header=TRUE, encoding="UTF-8", keepLeadingZeros = TRUE)) %>%
          assignClasses(., variables) %>%
          makePosColumns(., basename(x))
        
        return(stackedDf)
      },
      tables_i=tables[i], variables=variables,
      assignClasses=assignClasses,
      makePosColumns=makePosColumns, cl=cl
      ))

      data.table::fwrite(stackedDf, paste0(folder, "/stackedFiles/", tables[i], ".csv"),
                         nThread = nCores)
      
      # add location and publication field names to variables file
      if(!is.null(vlist)) {
        vtable <- which(names(vlist)==tables[i])
        if(length(vtable==1)) {
          if("horizontalPosition" %in% names(stackedDf)) {
            vlist[[vtable]] <- data.table::rbindlist(list(data.frame(base::cbind(table=rep(tables[i],4), 
                                                                                 added_fields[1:4,])), 
                                           vlist[[vtable]]), fill=TRUE)
          }
          if("publicationDate" %in% names(stackedDf)) {
            vlist[[vtable]] <- data.table::rbindlist(list(vlist[[vtable]], 
                                           c(table=tables[i], added_fields[5,])), fill=TRUE)
          }
        }
      }
      invisible(rm(stackedDf))
      n <- n + 1
    }
    
    # write out complete variables file
    vfull <- data.table::rbindlist(vlist, fill=TRUE)
    utils::write.csv(vfull, paste0(folder, "/stackedFiles/variables_", dpnum, ".csv"), row.names=F)
    messages <- c(messages, "Copied the most recent publication of variable definition file to /stackedFiles")
    m <- m + 1
    
  }
  
  writeLines(paste0(messages, collapse = "\n"))
  writeLines(paste("Finished: Stacked", n, "data tables and", m, "metadata tables!"))
  endtime <- Sys.time()
  writeLines(paste0("Stacking took ", format((endtime-starttime), units = "auto")))
  
}
