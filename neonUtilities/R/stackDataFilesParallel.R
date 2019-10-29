##############################################################################################
#' @title Join data files in a unzipped NEON data package by table type

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Given a folder of unzipped files (unzipped NEON data file), do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.

#' @param folder The location of the data
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
##############################################################################################

stackDataFilesParallel <- function(folder, nCores=1, forceParallel=FALSE){
  
  # get the in-memory list of table types (site-date, site-all, etc.). This list must be updated often.
  #data("table_types")
  ttypes <- table_types
  
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
  }
  
  # if there is more than one data file, stack files
  if(length(datafls) > 1){
    
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    
    tables <- findTablesUnique(names(datafls), ttypes)
    messages <- character()
    
    get_recent_publication <- function(in_list) {
      path_dates <-  lapply(in_list, function(x) {
        var_recent <- basename(dirname(x)) %>%
          gsub(pattern = "\\.csv$", "", .) %>%
          stringr::str_split(., '\\.|T') %>%
          unlist(.) %>%
          .[max(length(.))-1]
      }) 
      out_list <- in_list[grep(max(unlist(path_dates)), in_list)] %>%
        .[max(length(.))] 
      return(out_list)
    }
    
    # find external lab tables (lab-current, lab-all) and copy the first file from each lab into stackedFiles
    labTables <- tables[which(tables %in% table_types$tableName[which(table_types$tableType %in% c("lab-current","lab-all"))])]
    if(length(labTables)>0){
      test <- pbapply::pblapply(as.list(labTables), function(x) {
        labpath <- get_recent_publication(filepaths[grep(x, filepaths)])
        file.copy(labpath, paste0(folder, "/stackedFiles/"))
        messages <- c(messages, paste("Copied the most recent publication of", basename(labpath), "to /stackedFiles"))
      })
    }
    
    if(length(labTables)>0) {
      tables <- setdiff(tables, labTables)
    }

    # copy variables and validation files to /stackedFiles using the most recent collection date 
    if(TRUE %in% stringr::str_detect(filepaths,'variables.20')) {
      varpath <- get_recent_publication(filepaths[grep("variables.20", filepaths)])
      variables <- getVariables(varpath)   # get the variables from the chosen variables file
      file.copy(from = varpath, to = paste0(folder, "/stackedFiles/variables.csv"))
      messages <- c(messages, "Copied the most recent publication of variable definition file to /stackedFiles and renamed as variables.csv")
    }
    
    if(TRUE %in% stringr::str_detect(filepaths,'validation')) {
      valpath <- get_recent_publication(filepaths[grep("validation", filepaths)])
      file.copy(from = valpath, to = paste0(folder, "/stackedFiles/validation.csv"))
      messages <- c(messages, "Copied the most recent publication of validation file to /stackedFiles and renamed as validation.csv")
    }
    
    if(TRUE %in% stringr::str_detect(filepaths,'sensor_position')) {
      sppath <- get_recent_publication(filepaths[grep("sensor_position", filepaths)])
      sppath <- data.table::fread(sppath, header=TRUE, encoding="UTF-8", keepLeadingZeros = TRUE,
                                  colClasses = list(character = c('HOR.VER'))) %>%
        makePosColumns(., sppath) %>%
        data.table::fwrite(., paste0(folder, "/stackedFiles/sensor_positions.csv"))
      messages <- c(messages, "Copied the most recent publication of sensor position file to /stackedFiles and renamed as sensor_positions.csv")
    }
    
    if(nCores > parallel::detectCores()) {
      stop(paste("The number of cores selected exceeds the available cores on your machine.  The maximum number of cores allowed is", parallel::detectCores(), "not", nCores))
    }
    
    # Make a decision on parallel processing based on the total size of directories or whether there are lots of 1_minute files
    # All of this is overruled if forceParallel = TRUE
    if(forceParallel == TRUE) {
      cl <- parallel::makeCluster(getOption("cl.cores", nCores))
    } else {
      directories <- sum(file.info(grep(list.files(folder, full.names=TRUE, pattern = 'NEON'), pattern = "stacked|*.zip", invert=TRUE, value=TRUE))$size)
      if(directories >= 25000) {
        cl <- parallel::makeCluster(getOption("cl.cores", parallel::detectCores()))
        nCores <- parallel::detectCores()
        writeLines(paste0("Parallelizing stacking operation across ", parallel::detectCores(), " cores."))
      } else {
        cl <- parallel::makeCluster(getOption("cl.cores", nCores))
        writeLines(paste0("File requirements do not meet the threshold for automatic parallelization, please see forceParallel to run stacking operation across multiple cores. Running on single core."))
      }
    }
    
    # If error, crash, or completion , closes all clusters
    suppressWarnings(on.exit(parallel::stopCluster(cl)))
    
    for(i in 1:length(tables)){
      tbltype <- unique(ttypes$tableType[which(ttypes$tableName == gsub(tables[i], pattern = "_pub", replacement = ""))])
      variables <- getVariables(varpath)  # get the variables from the chosen variables file
      
      writeLines(paste0("Stacking table ", tables[i]))
      tblfls <- filepaths[grep(paste(".", tables[i], ".", sep=""), filepaths, fixed=T)]
      
      stackedDf <- pbapply::pblapply(tblfls, function(x, tables_i, variables, assignClasses, 
                                                      makePosColumns) {
        requireNamespace('dplyr')
        requireNamespace("magrittr")
        requireNamespace('data.table')
        
        stackedDf <- suppressWarnings(data.table::fread(x, header=TRUE, encoding="UTF-8", keepLeadingZeros = TRUE)) %>%
          assignClasses(., variables) %>%
          makePosColumns(., basename(x))
        
        return(stackedDf)
      },
      tables_i=tables[i], variables=variables,
      assignClasses=assignClasses,
      makePosColumns=makePosColumns, 
      cl=cl
      )
      data.table::fwrite(do.call(rbind, stackedDf), paste0(folder, "/stackedFiles/", tables[i], ".csv"),
                         nThread = nCores)
      invisible(rm(stackedDf))
    }
  }
}
