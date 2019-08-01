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

stackDataFiles <- function(folder){
  starttime <- Sys.time()

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

    # copy first variables and validation files to /stackedFiles
    varpath <- filepaths[grep("variables.20", filepaths)[1]]
    if(is.na(varpath)){
      varpath <- filepaths[grep("variables", filepaths)[1]]
    }
    if(!is.na(varpath)){
      variables <- getVariables(varpath)   # get the variables from the chosen variables file
      file.copy(from = varpath, to = paste0(folder, "/stackedFiles/variables.csv"))
      messages <- c(messages, "Copied the first available variable definition file to /stackedFiles and renamed as variables.csv")
    }

    valpath <- filepaths[grep("validation", filepaths)[1]]
    if(!is.na(valpath)){
      file.copy(from = valpath, to = paste0(folder, "/stackedFiles/validation.csv"))
      messages <- c(messages, "Copied the first available validation file to /stackedFiles and renamed as validation.csv")
    }

    if(!is.na(varpath)){
      tables <- c(tables, "variables")
    }
    if(!is.na(valpath)){
      tables <- c(tables, "validation")
    }

    n <- 0

    for(i in 1:length(tables)){
      tbltype <- unique(ttypes$tableType[which(ttypes$tableName == gsub(tables[i], pattern = "_pub", replacement = ""))])
      variables <- getVariables(varpath)  # get the variables from the chosen variables file

      if((length(tbltype)==0 && !(tables[i] %in% c("variables","validation"))) || (length(tbltype) > 0 && tbltype == "site-all")){
        writeLines(paste0("Stacking table ", tables[i]))
        pb <- utils::txtProgressBar(style=3)
        utils::setTxtProgressBar(pb, 0)
        #tblfls <- filepaths[grep(tables[i], filepaths, fixed=T)]
        #tblnames <- filenames[grep(tables[i], filenames, fixed=T)]
        tblfls <- filepaths[grep(paste(".", tables[i], ".", sep=""), filepaths, fixed=T)]
        tblnames <- filenames[grep(paste(".", tables[i], ".", sep=""), filenames, fixed=T)]
        sites <- unique(substr(tblnames, 10, 13))
        sites <- sites[order(sites)]
        d <- suppressWarnings(data.table::fread(tblfls[grep(sites[1], tblfls)][1], header=T, encoding="UTF-8"))
        d <- assignClasses(d, variables)
        d <- makePosColumns(d, tblnames[1])
        numRows <- nrow(d)
        utils::setTxtProgressBar(pb, 1/length(tblfls))
        if(length(sites) > 1){
          for(j in 2:length(sites)){
            sitefls <- tblfls[grep(sites[j], tblfls)]
            sitenames <- tblnames[grep(sites[j], tblnames)]
            d.next <- suppressWarnings(data.table::fread(sitefls[1], header=T, encoding="UTF-8"))
            d.next <- assignClasses(d.next, variables)
            d.next <- makePosColumns(d.next, sitenames[1])
            numRows <- sum(numRows, nrow(d.next))
            d <- rbind(d, d.next, fill = TRUE)
            utils::setTxtProgressBar(pb, (i*j)/length(tblfls))
          }
        }
        utils::write.csv(d, paste0(folder, "/stackedFiles/", tables[i], ".csv"), row.names = F)
        messages <- c(messages, paste0("Stacked ", tables[i], " which has ", numRows, " out of the expected ",
                                       nrow(d), " rows (", (numRows/nrow(d))*100, "%)."))
        n <- n + 1
        utils::setTxtProgressBar(pb, 1)
        close(pb)
      }


      if((length(tbltype)==0 && !(tables[i] %in% c("variables","validation"))) || (length(tbltype) > 0 && tbltype == "site-date")){
        writeLines(paste0("Stacking table ", tables[i]))
        pb <- utils::txtProgressBar(style=3)
        utils::setTxtProgressBar(pb, 0)
        #tblfls <- filepaths[grep(tables[i], filepaths, fixed=T)]
        #tblnames <- filenames[grep(tables[i], filenames, fixed=T)]
        tblfls <- filepaths[grep(paste(".", tables[i], ".", sep=""), filepaths, fixed=T)]
        tblnames <- filenames[grep(paste(".", tables[i], ".", sep=""), filenames, fixed=T)]
        d <- suppressWarnings(data.table::fread(tblfls[1], header=T, encoding="UTF-8"))
        d <- assignClasses(d, variables)
        d <- makePosColumns(d, tblnames[1])
        numRows <- nrow(d)
        utils::setTxtProgressBar(pb, 1/length(tblfls))
        if(length(tblfls) > 1){
          for(j in 2:length(tblfls)){
            d.next <- suppressWarnings(data.table::fread(tblfls[j], header=T, encoding="UTF-8"))
            d.next <- assignClasses(d.next, variables)
            d.next <- makePosColumns(d.next, tblnames[j])
            numRows <- sum(numRows, nrow(d.next))
            d <- rbind(d, d.next, fill = TRUE)
            utils::setTxtProgressBar(pb, (i*j)/length(tblfls))
          }
        }
        utils::write.csv(d, paste0(folder, "/stackedFiles/", tables[i], ".csv"), row.names = F)
        messages <- c(messages, paste0("Stacked ", tables[i], " which has ", numRows, " out of the expected ",
                                      nrow(d), " rows (", (numRows/nrow(d))*100, "%)."))
        n <- n + 1
        utils::setTxtProgressBar(pb, 1)
        close(pb)
      }
    }
  }

  writeLines(paste("Finished: All of the data are stacked into", n, "tables!"))
  writeLines(paste0(messages, collapse = "\n"))
  endtime <- Sys.time()
  writeLines(paste0("Stacking took ", format((endtime-starttime), units = "auto")))
}
