# test_functions.R

#' Get a file's size in megabytes
#' @importFrom gdata humanReadable
#' @param filepath The path to the file
#' @return The size of the file in megabytes
get.filesize <- function(filepath){
  fs <- humanReadable(file.size(filepath), units = "auto", standard = "SI")
  return(fs)
}

get.tabletypes <- function(){
  t <- read.csv("data/table_types.csv", header = T, stringsAsFactors = F)
  return(t)
}

#' Get a data frame with the names of all files within a zipped NEON data package
#'
#' Given the top level zip file, return dataframe of all of the files within it without
#' unzipping the file
#'
#' @param zippath The path to a zip file
#' @return A list of filenames within the given zip file
list.files.inZip <- function(zippath){
  unzip(zipfile = zippath, list = T)
}

#' Get all zip file names within a zipped NEON data package
#'
#' Given the data frame of all the files within the top level zip file,
#' return an array of just the zip file names (no pdf, xml, or other files).
#'
#' @param zippath The path to a zip file
#' @return An array of all zip files contained within the focal zip file
list.zipfiles <- function(zippath){
  df <- unzip(zipfile = zippath, list = T)
  ns <- df[,1]
  fn <- ns[which(substr(ns, nchar(ns)-3, nchar(ns)) == ".zip")]
  return(fn)
}

#' Unzip a zip file either at just the top level or recursively through the file
#' @param zippath The filepath of the input file
#' @param outpath The name of the folder to save unpacked files to
#' @param level Whether the unzipping should occur only for the 'top' zip file, or unzip 'all' recursively
unzip.zipfile <- function(zippath, outpath = substr(zippath, 1, nchar(zippath)-4), level="all"){
  if(level == "top"){
    unzip(zipfile = zippath, exdir=outpath)
  }
  if(level == "all"){
    unzip(zipfile = zippath, exdir=outpath)
    zps <- list.zipfiles(zippath)
    if(length(zps) >= 1){
      for(i in 1:length(zps)){
        p <- paste0(outpath, "/", zps[i])
        unzip(p, exdir=substr(p, 1, nchar(p)-4), overwrite = T)
        if (file.exists(p)) file.remove(p)
        writeLines(paste("Unpacked ", zps[i]))
      }
    } else writeLines("This zip file doesn't contain monthly data packages")
  }
}

#' List the names of the data tables within each folder
#' @keywords internal
#' @param folder The folder of the outputs
#' @param fnames Full names - if true, then return the full file names including enclosing folders, if false, return only the file names
#' @return a data frame of file names
find.datatables <- function(folder, fnames = T){
  fs <- list.dirs(folder)
  g <- grep(pattern = "stackedFiles", x = fs)
  if(length(g) > 0){fs <- fs[-g]}
  fls <- character()
  for(i in 1:length(fs)){
    fls <- c(fls, list.files(fs[i], full.names = fnames))
  }
  return(fls[which(substr(fls, nchar(fls)-3, nchar(fls)) == ".csv")])
}

#' Internal use only
#'
#' Find the data tables that are present in the dataset (e.g., 2 minute vs 30 minute, or pinning vs identification data)
#'
#' @keywords internal
#' @param datatables A list of data files
#' @return An array of unique table names
find.tables.unique <- function(datatables, tabletypes){
  dt <- datatables
  tt <- tabletypes
  splitNames <- strsplit(x = dt, split = "\\.")
  t <- character()
  for (i in 1:length(splitNames)){
    for(j in 1:length(splitNames[[i]])){
      s <- splitNames[[i]][j]
      if(s %in% tt$table){
        t <- c(t, s)
      }
    }
    if(length(splitNames[[i]]) == 12){
      n <- splitNames[[i]][11]
      t <- c(t, n)
    }
    if(length(splitNames[[i]]) == 14){
      n <- splitNames[[i]][10]
      t <- c(t, n)
    }
  }
  return(unique(t))
}

#' Internal use only
#'
#' Support way to force R to read assign correct data types to each column based on variables file
#'
#' @keywords internal
#' @param varFile A file that contains variable definitions
#' @return A data frame with fieldName and assigned column class, along with table if present
getVariables <- function(varFile){
  d <- read.csv(varFile, header = T, stringsAsFactors = F)                     # read in a variables file
  d$colClass <- rep("numeric", nrow(d))                                       # make a new colClass column defaulting to numeric
  d$colClass[which(d$dataType %in% c("string", "date", "dateTime"))] <- "character"   # change to character if string or dateTime
  if("table" %in% names(d)){                                                   # OS variables files have table, IS do not
    return(d[, c("table", "fieldName", "colClass")])
  }
  return(d[, c("fieldName","colClass")])
}

#' Internal use only
#'
#' Use the variables file to assign classes to each column in each data file
#'
#' @keywords internal
#' @param df A data frame
#' @param inVars The variables expected in the df
#' @return A data frame with corrected column classes
assignClasses <- function(df, inVars){
  for(i in 1:ncol(df)){
    if(names(df)[i] %in% inVars$fieldName){
      type <- inVars$colClass[which(inVars$fieldName==names(df)[i])[1]]
      if(type == "numeric")
        df[,i] <- as.numeric(df[,i])
      if(type == "character")
        df[,i] <- as.character(df[,i])
    }
  }
  return(df)
}

getPos <- function(d, datafl){
  d.splitFile <- strsplit(x = datafl, split = "\\/")
  d.splitName <- strsplit(x = d.splitFile[[1]][length(d.splitFile[[1]])], split = "\\.")
  nc <- ncol(d)
  if(length(d.splitName[[1]]) %in% c(12,14)){
    if(length(d.splitName[[1]]) == 12){
      horPos <- 8
      verPos <- 9
    }
    if(length(d.splitName[[1]]) == 14){
      horPos <- 7
      verPos <- 8
    }
    if(!("siteID" %in% names(d))){
      d$domainID <- rep(as.character(d.splitName[[1]][2]), nrow(d))
      d$siteID <- rep(as.character(d.splitName[[1]][3]), nrow(d))
    }
    d$horizontalPosition <- rep(as.character(d.splitName[[1]][horPos]), nrow(d))
    d$verticalPosition <- rep(as.character(d.splitName[[1]][verPos]), nrow(d))
    d$horizontalPosition <- as.character(d$horizontalPosition)
    d$verticalPosition <- as.character(d$verticalPosition)
    d <- d[ , c((nc+1):(nc+4), 1:nc)]
    return(d)
  }
  return(d)
}



#' Join data files in a unzipped NEON data package by table type
#'
#' Given a folder of unzipped files (unzipped NEON data file), do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.
#'
#' @importFrom dplyr full_join
#' @param folder The location of the data
#' @return One file for each table type is created and written.

stackDataFiles <- function(folder){
  ttypes <- get.tabletypes()
  filenames <- find.datatables(folder = folder, fnames = F)                    # filenames without full path
  filepaths <- find.datatables(folder = folder, fnames = T)                    # filenames with full path
  filelist <- setNames(as.list(filepaths), filenames)        # make a list, where filenames are the keys to the filepath values

#  nondatafls <- c(varfls, valfls)                         # these two lists together make up files that aren't data files
#  if(length(nondatafls) > 0){                             # remove the non-data files from the list
#    datafls <- filelist[-nondatafls]
# } else {
  datafls <- filelist

  if(length(datafls) == 0){                                  # if there are no datafiles, exit
    writeLines("No data files are present")
  }
  if(length(datafls) == 1){                                 # if there is just one data file (and thus one table name)
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    file.copy(from = datafls[1][[1]], to = "/stackedFiles")
  }
  if(length(datafls) > 1){                                  # if there is more than one data file
    if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
    tables <- find.tables.unique(names(datafls), ttypes)    # find the unique tables
    varpath <- filepaths[grep("variables.20", filepaths)[1]]
    valpath <- filepaths[grep("validation", filepaths)[1]]
    if(is.na(valpath)){
      tables <- c(tables, "variables")
      } else {
      tables <- c(tables, "variables","validation")
      }

    messages <- character()
    if(!is.na(varpath)){
      variables <- getVariables(varpath)                    # get the variables from the chosen variables file
      file.copy(from = varpath, to = paste0(folder, "/stackedFiles/variables.csv"))
      messages <- c(messages, "Copied the first available variable definition file to /stackedFiles and renamed as variables.csv")
    }
    if(!is.na(valpath)){
      file.copy(from = valpath, to = paste0(folder, "/stackedFiles/validation.csv"))
      messages <- c(messages, "Copied the first available validation file to /stackedFiles and renamed as validation.csv")
    }
    n <- 1
    for(i in 1:length(tables)){
      tbltype <- ttypes$tableType[which(ttypes$table == tables[i])]
      if(length(tbltype) > 0 && tbltype != "site-date"){
        file.copy(from = filepaths[grep(tables[i], filepaths)][1], to = paste0(folder, "/stackedFiles/", tables[i], ".csv"))
        messages <- c(messages, paste("Copied the first available", tables[i], "file to /stackedFiles"))
      }
      if((length(tbltype)==0 && !(tables[i] %in% c("variables","validation"))) || (length(tbltype) > 0 && tbltype == "site-date")){
        tblfls <- filepaths[grep(tables[i], filepaths)]
        tblnames <- filenames[grep(tables[i], filenames)]
        d <- read.csv(tblfls[1], header = T, stringsAsFactors = F)
        d <- assignClasses(d, variables)
        d <- getPos(d, tblnames[1])
        if(length(tblfls) > 1){
          for(j in 2:length(tblfls)){
            d.next <- read.csv(tblfls[j], header = T, stringsAsFactors = F)
            d.next <- assignClasses(d.next, variables)
            d.next <- getPos(d.next, tblnames[j])
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

# The ultimate function...

#' Primary function: Join data files in a zipped NEON data package by table type
#'
#' Given a zipped data file, do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.
#'
#' @param filepath The location of the zip file
#' @return All files are unzipped and one file for each table type is created and written.
#' @export
stackByTable <- function(filepath){
  location.data <- substr(filepath, 1, nchar(filepath)-4)
  unzip.zipfile(zippath = filepath, outpath = location.data, level = "all")
  stackDataFiles(location.data)
}


