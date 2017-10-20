##############################################################################################
#' @title Join data files in a unzipped NEON data package by table type

#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Given a folder of unzipped files (unzipped NEON data file), do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.

#' @importFrom dplyr full_join
#' @param folder The location of the data
#' @return One file for each table type is created and written.

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Christine Laney (2017-07-02)
##############################################################################################

stackDataFiles <- function(folder){
  # get the in-memory list of table types (site-date, site-all, etc.). This list must be updated often.
  load("data/table_types.rda")
  ttypes <<- table_types

  # filenames without full path
  filenames <- findDatatables(folder = folder, fnames = F)

  # filenames with full path
  filepaths <- findDatatables(folder = folder, fnames = T)

  # make a list, where filenames are the keys to the filepath values
  filelist <- setNames(as.list(filepaths), filenames)

  datafls <- filelist

  # if there are no datafiles, exit
  if(length(datafls) == 0){
    writeLines("No data files are present")
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
    externalLabFile <- filepaths[grep("externalSummary", filepaths)[1]]
    varpath <- filepaths[grep("variables.20", filepaths)[1]]

    if(is.na(varpath)){
      varpath <- filepaths[grep("variables", filepaths)[1]]
    }
    valpath <- filepaths[grep("validation", filepaths)[1]]

    if(is.na(valpath)){
      tables <- c(tables, "variables")
    } else {
      tables <- c(tables, "variables","validation")
    }

    messages <- character()

    if(!is.na(externalLabFile)){
      file.copy(from = externalLabFile, to = paste0(folder, "/stackedFiles/"))
      messages <- c(messages, "Copied the first available external lab summary file to /stackedFiles")
    }

    if(!is.na(varpath)){
      variables <- getVariables(varpath)   # get the variables from the chosen variables file
      file.copy(from = varpath, to = paste0(folder, "/stackedFiles/variables.csv"))
      messages <- c(messages, "Copied the first available variable definition file to /stackedFiles and renamed as variables.csv")
    }

    if(!is.na(valpath)){
      file.copy(from = valpath, to = paste0(folder, "/stackedFiles/validation.csv"))
      messages <- c(messages, "Copied the first available validation file to /stackedFiles and renamed as validation.csv")
    }

    n <- 1

    for(i in 1:length(tables)){
      tbltype <- ttypes$tableType[which(ttypes$tableName == tables[i])]
      variables <- getVariables(varpath)  # get the variables from the chosen variables file

      if(length(tbltype) > 0 && tbltype != "site-date"){
        file.copy(from = filepaths[grep(tables[i], filepaths)][1], to = paste0(folder, "/stackedFiles/", tables[i], ".csv"))
        messages <- c(messages, paste("Copied the first available", tables[i], "file to /stackedFiles"))
      }

      if((length(tbltype)==0 && !(tables[i] %in% c("variables","validation"))) || (length(tbltype) > 0 && tbltype == "site-date")){
        tblfls <- filepaths[grep(tables[i], filepaths)]
        tblnames <- filenames[grep(tables[i], filenames)]
        d <- read.csv(tblfls[1], header = T, stringsAsFactors = F)
        d <- assignClasses(d, variables)
        d <- makePosColumns(d, tblnames[1])
        if(length(tblfls) > 1){
          for(j in 2:length(tblfls)){
            d.next <- read.csv(tblfls[j], header = T, stringsAsFactors = F)
            d.next <- assignClasses(d.next, variables)
            d.next <- makePosColumns(d.next, tblnames[j])
            d <- dplyr::full_join(d, d.next)
          }
        }
        write.csv(d, paste0(folder, "/stackedFiles/", tables[i], ".csv"), row.names = F)
        messages <- c(messages, paste("Stacked ", tables[i]))
        if(i > 1){n <- n + 1}
      }
    }
  }

  writeLines(paste("Finished: All of the data are stacked into ", n, " tables!"))
  writeLines(paste0(messages, collapse = "\n"))

}
