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

stackDataFilesParallel <- function(folder, nCores){
  
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
    if(TRUE %in% grepl('sensor_positions', names(datafls))) {
      tables <- c(tables, 'sensor_positions')
    }
    messages <- character()
    
    # find external lab tables (lab-current, lab-all) and copy the first file from each lab into stackedFiles
    labTables <- tables[which(tables %in% table_types$tableName[which(table_types$tableType %in% c("lab-current","lab-all"))])]
    if(length(labTables)>0){
      labFilePaths <- character()
      labFiles <- character()
      for(m in 1:length(labTables)) {
        labFilePaths <- c(labFilePaths, filepaths[grep(labTables[m], filepaths)])
        labFiles <- c(labFiles, unique(filenames[grep(labTables[m], filenames)]))
      }
      if(length(labFiles)>0){
        for(l in 1:length(labFiles)){
          file.copy(from = labFilePaths[grep(labFiles[l], labFilePaths)][1], to = paste0(folder, "/stackedFiles/"))
          messages <- c(messages, paste("Copied the first available", labFiles[l], "to /stackedFiles"))
        }
      }
    }
    
    # copy variables and validation files to /stackedFiles using the most recent collection date 
    if(TRUE %in% stringr::str_detect(filepaths,'variables.20')){
      varpath <- filepaths[grep("variables.20", filepaths)[max(length(filepaths[grep("variables.20", filepaths)]))]]
      variables <- getVariables(varpath)   # get the variables from the chosen variables file
      file.copy(from = varpath, to = paste0(folder, "/stackedFiles/variables.csv"))
      messages <- c(messages, "Copied the first available variable definition file to /stackedFiles and renamed as variables.csv")
    }
    
    if(TRUE %in% stringr::str_detect(filepaths,'validation')) {
      valpath <- filepaths[grep("validation", filepaths)[max(length(filepaths[grep("validation", filepaths)]))]]
      file.copy(from = valpath, to = paste0(folder, "/stackedFiles/validation.csv"))
      messages <- c(messages, "Copied the first available validation file to /stackedFiles and renamed as validation.csv")
    }
    
    # Spin-up for parallel processing
    cl <- parallel::makeCluster(getOption("cl.cores", nCores))
    # If error or crash, closes all clusters
    on.exit(parallel::stopCluster(cl))
    
    for(i in 1:length(tables)){
      tbltype <- unique(ttypes$tableType[which(ttypes$tableName == gsub(tables[i], pattern = "_pub", replacement = ""))])
      variables <- getVariables(varpath)  # get the variables from the chosen variables file
      
      writeLines(paste0("Stacking table ", tables[i]))
      tblfls <- filepaths[grep(paste(".", tables[i], ".", sep=""), filepaths, fixed=T)]
      tblnames <- filenames[grep(paste(".", tables[i], ".", sep=""), filenames, fixed=T)]
      
      df <- do.call(rbind, pbapply::pblapply(tblfls, function(x, tables_i, variables, tblfls, tblnames, assignClasses, 
                                         makePosColumns, folder, tbltype, messages) {
        require(tidyverse)
        require(data.table)
        
        if((length(tbltype) > 0 && tbltype == "site-all")){
          sites <- unique(substr(tblnames, 10, 13))
          sites <- sites[order(sites)]
          
          sitefls <- x[grep(sites, x)]
          sitenames <- tblnames[grep(sites, tblnames)]
          df <- suppressWarnings(data.table::fread(sitefls[1], header=T, encoding="UTF-8")) %>%
            assignClasses(., variables) %>%
            makePosColumns(., sitenames[1], spFolder=x)
          }
        
        if((length(tbltype)==0 && tables_i %in% "sensor_positions") || (length(tbltype) > 0 && tbltype == "site-date")){

          df <- suppressWarnings(data.table::fread(x, header=T, encoding="UTF-8")) %>%
            assignClasses(., variables) %>%
            makePosColumns(., tblnames, spFolder=x)
          }
          return(df)
        },
        tables_i=tables[i], variables=variables, tblfls=tblfls,
        tblnames=tblnames, assignClasses=assignClasses,
        makePosColumns=makePosColumns, folder=folder,
        messages=messages, tbltype=tbltype, cl=cl
        ))
      utils::write.csv(df, paste0(folder, "/stackedFiles/", tables[i], ".csv"), row.names = F)
      }
    }
}
