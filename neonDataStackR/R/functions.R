#' Get a file's size in megabytes
#' @importFrom gdata humanReadable
#' @param filepath The path to the file
#' @return The size of the file in megabytes
#' @export
get.filesize <- function(filepath){
  fs <- humanReadable(file.size(filepath), units = "auto", standard = "SI")
  return(fs)
}


#' Get a data frame with the names of all files within a zipped NEON data package
#'
#' Given the top level zip file, return dataframe of all of the files within it without
#' unzipping the file
#'
#' @param zippath The path to a zip file
#' @return A list of filenames within the given zip file
#' @export
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
#' @export
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
#' @export
unzip.zipfile <- function(zippath, outpath = substr(zippath, 1, nchar(zippath)-4), level="all"){
  if(level == "top"){
    unzip(zipfile = zippath, exdir=outpath)
  }
  if(level == "all"){
    unzip(zipfile = zippath, exdir=outpath)
    zps <- list.zipfiles(zippath)
    for(i in 1:length(zps)){
      p <- paste0(outpath, "/", zps[i])
      unzip(p, exdir=substr(p, 1, nchar(p)-4), overwrite = T)
      if (file.exists(p)) file.remove(p)
      writeLines(paste("Unpacked ", zps[i]))
    }
  }
}

#' List the names of the data tables within each folder
#' @keywords internal
#' @param folder The folder of the outputs
#' @param fnames Full names - if true, then return the full file names including enclosing folders, if false, return only the file names
#' @return a data frame of file names
#' @export
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
find.tables.unique <- function(datatables){
  splitNames <- strsplit(x = datatables, split = "\\.")
  t <- character()
  for (i in 1:length(splitNames)){
    if(length(splitNames[[i]]) %in% c(8,11)){
      n <- splitNames[[i]][7]
      t <- c(t, n)
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
  d$colClass[which(d$dataType %in% c("string", "dateTime"))] <- "character"   # change to character if string or dateTime
  if("table" %in% names(d))                                                   # OS variables files have table, IS do not
    return(d[, c("table", "fieldName", "colClass")])
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



#' Join data files in a unzipped NEON data package by table type
#'
#' Given a folder of unzipped files (unzipped NEON data file), do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.
#'
#' @importFrom dplyr full_join
#' @param folder The location of the data
#' @return One file for each table type is created and written.
#' @export
stackDataFiles <- function(folder){
  filenames <- find.datatables(folder = folder, fnames = F)                    # filenames without full path
  filepaths <- find.datatables(folder = folder, fnames = T)                    # filenames with full path
  filelist <- setNames(as.list(filepaths), filenames)           # make a list, where filenames are the keys to the filepath values
  varfls <- grep(pattern = "variables", x = filenames)       # find the positions in the filename list where there are variables files

  if(length(varfls > 0)){                                 # choose the first variables file there is (need to replace with checking diffs between all)
    varFile <- filelist[varfls[1]][[1]]
  } else varFile <- NA

  valfls <- grep(pattern = "validation", x = filenames)      # find the positions in the filename list where there are validiation files
  nondatafls <- c(varfls, valfls)                         # these two lists together make up files that aren't data files
  datafls <- filelist

  if(length(nondatafls) > 0){                             # remove the non-data files from the list
    datafls <- filelist[-nondatafls]
  }

  if(length(datafls)==0){                                  # if there are no datafiles, exit
    return()
  }

  if(length(datafls)==1){                                 # if there is just one data file (and thus one table name)
    newdata <- read.csv(datafls[1][[1]], header = T, stringsAsFactors = F) # read it in
    table <- find.tables.unique(names(datafls))           # find the unique table names
    write.csv(newdata, paste0(folder, "/", table, ".csv"), row.names = F)  # then write out the data file with a new name into the top level folder.
  }

  if(length(datafls)>1){                                  # if there is more than one data file
    tables <- find.tables.unique(names(datafls))          # find the unique tables
    variables <- getVariables(varFile)                    # get the variables from the chosen variables file

    for(i in 1:length(tables)){
      f <- datafls[grep(pattern = tables[i], datafls)]
      d <- read.csv(f[1][[1]], header = T, stringsAsFactors = F)
      d <- assignClasses(d, variables)
      d.splitFile <- strsplit(x = f[1][[1]], split = "\\/")
      d.splitName <- strsplit(x = d.splitFile[[1]][length(d.splitFile[[1]])], split = "\\.")
      if(length(d.splitName[[1]]) == 12){
        d$horizontalPosition <- rep(as.character(d.splitName[[1]][8]), nrow(d))
        d$verticalPosition <- rep(as.character(d.splitName[[1]][9]), nrow(d))
        d$horizontalPosition <- as.character(d$horizontalPosition)
        d$verticalPosition <- as.character(d$verticalPosition)
      }
      if(length(d.splitName[[1]]) == 14){
        d$horizontalPosition <- rep(as.character(d.splitName[[1]][7]), nrow(d))
        d$verticalPosition <- rep(as.character(d.splitName[[1]][8]), nrow(d))
        d$horizontalPosition <- as.character(d$horizontalPosition)
        d$verticalPosition <- as.character(d$verticalPosition)
      }

      if(length(f) > 1){
        for(j in 2:length(f)){
          d.next <- read.csv(f[j][[1]], header = T, stringsAsFactors = F)
          d.next <- assignClasses(d.next, variables)
          d.next.splitFile <- strsplit(x = f[j][[1]], split = "\\/")
          d.next.splitName <- strsplit(x = d.next.splitFile[[1]][length(d.next.splitFile[[1]])], split = "\\.")
          if(length(d.next.splitName[[1]]) == 12){
            d.next$horizontalPosition <- rep(as.character(d.next.splitName[[1]][8]), nrow(d.next))
            d.next$verticalPosition <- rep(as.character(d.next.splitName[[1]][9]), nrow(d.next))
            d.next$horizontalPosition <- as.character(d.next$horizontalPosition)
            d.next$verticalPosition <- as.character(d.next$verticalPosition)
          }
          if(length(d.next.splitName[[1]]) == 14){
            d.next$horizontalPosition <- rep(as.character(d.next.splitName[[1]][7]), nrow(d.next))
            d.next$verticalPosition <- rep(as.character(d.next.splitName[[1]][8]), nrow(d.next))
            d.next$horizontalPosition <- as.character(d.next$horizontalPosition)
            d.next$verticalPosition <- as.character(d.next$verticalPosition)
          }
          d <- dplyr::full_join(d, d.next)
        }
        if(dir.exists(paste0(folder, "/stackedFiles")) == F) {dir.create(paste0(folder, "/stackedFiles"))}
        write.csv(d, paste0(folder, "/stackedFiles/", tables[i], ".csv"), row.names = F)
      }
      writeLines(paste("Stacked ", tables[i]))
    }
  }
  writeLines(paste("Finished: All data is stacked into ", i, " tables!"))
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


