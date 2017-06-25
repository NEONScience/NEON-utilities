#' Get the file's size in megabytes
#'
#' @param filepath The path to the file
#' @return The size of the file in megabytes
#' @export
get.filesize <- function(filepath){
  fs <- humanReadable(file.size(filepath), units = "auto", standard = "SI")
  return(fs)
}


#' Given the top level zip file, return dataframe of all of the files within it without
#' unzipping the file
#'
#' @param zippath The path to a zip file
#' @return A list of filenames within the given zip file
#' @export
list.files.inZip <- function(zippath){
  unzip(zipfile = zippath, list = T)
}

#' Given the data frame of all the files within the top level zip file,
#' return just an array of just the zip file names (no pdfs, etc.)
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

#' Unzip a zip file either at just the top level or recursively through the file.
#'
#' @param zippath The filepath of the input file
#' @param outpath The name of the folder to save unpacked files to
#' @param level Whether the unzipping should occur only for the 'top' zip file, or unzip 'all' recursively
#' @return A folder structure of unzipped files
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
#'
#' @param filepath The filepath of the input file
#' @param fnames Full names - if true, then return the full file names including enclosing folders, if false, return only the file names
#' @return a data frame of file names
#' @export
find.datatables <- function(folder, fnames = T){
  fs <- list.dirs(folder)
  fs <- fs[-1]
  fls <- character()
  for(i in 1:length(fs)){
    fls <- c(fls, list.files(fs[i], full.names = fnames))
  }
  return(fls[which(substr(fls, nchar(fls)-3, nchar(fls)) == ".csv")])
}

#' Find the data tables that are present in the dataset (e.g., 2 minute vs 30 minute, or pinning vs identification data)
#'
#' @param datatables A list of data files
#' @return a list of unique tables
#' @export
find.tables.unique <- function(datatables){
  splitNames <- strsplit(x = datatables, split = "\\.")
  t <- character()
  for (i in 1:length(splitNames)){
    if(length(splitNames[[i]]) %in% c(8,11)){
      n <- splitNames[[i]][7]
      t <- c(t, n)
    }
    if(length(splitNames[[i]]) %in% c(12,14)){
      n <- splitNames[[i]][11]
      t <- c(t, n)
    }
  }
  return(unique(t))
}

fNameSlotNum <- function(myFile){
  a <- strsplit(myFile, "\\.")
  l <- length(a[][[1]])
  return(l)
}

#support way to force R to read assign correct data types to each column based on variables file
getVariables <- function(varFile){
  d <- read.csv(varFile, header = T, stringsAsFactors = F)                     # read in a variables file
  d$colClass <- rep("numeric", nrow(d))                                       # make a new colClass column defaulting to numeric
  d$colClass[which(d$dataType %in% c("string", "dateTime"))] <- "character"   # change to character if string or dateTime
  if("table" %in% names(d))                                                   # OS variables files have table, IS do not
    return(d[, c("table", "fieldName", "colClass")])
  return(d[, c("fieldName","colClass")])
}

#' Given a folder of unzipped files (unzipped NEON data file), do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.
#'
#' @param folder The location of the data
#' @return One file for each table type is created and written.
stackDataFiles <- function(folder){
  fnames <- find.datatables(folder, F)  #filenames without full path
  fpaths <- find.datatables(folder, T)  #filenames with full path
  filelist <- setNames(as.list(fpaths), fnames) #make a list, where filenames are the keys to the filepath values
  varfls <- grep(pattern = "variables", x = fnames) #find the positions in the filename list where there are variables files

  # Working on this piece
  if(length(varfls > 0)){
    varFile <- filelist[varfls[1]][[1]]
  }

  valfls <- grep(pattern = "validation", x = fnames) #find the positions in the filename list where there are validiation files
  nondatafls <- c(varfls, valfls) #these two lists together make up files that aren't data files
  datafls <- filelist

  if(length(nondatafls) > 0){
    datafls <- filelist[-nondatafls] #remove the non-data files from the list
  }

  if(length(datafls)==0) #if there are no datafiles, exit
    return()

  if(length(datafls)==1){ #if there is just one data file (and thus one table name)
    newdata <- read.csv(datafls[1][[1]], header = T, stringsAsFactors = F) #if there is at least one data file, read the first one
    table <- find.tables.unique(names(datafls)) #find the unique table names
    write.csv(newdata, paste0(folder, "/", table, ".csv"), row.names = F) #then write out the data file with a new name into the top level folder.
  }

  if(length(datafls)>1){ #if there is more than one data file
    tables <- find.tables.unique(names(datafls)) #find the unique table
    # Working on this piece
    variables <- getVariables(varFile)

    for(i in 1:length(tables)){
      f <- datafls[grep(pattern = tables[i], datafls)]

      # Working on this piece
#      if(!is.null(variables$table)){            # if variables data frame has a column named table
#        colcls <- as.vector(variables[which(variables$table == tables[i]), "colClass"])   # make a vector of colClasses for the table
#      } else {colcls = NA}

      d <- read.csv(f[1][[1]], header = T, stringsAsFactors = F)
      for(j in 1:ncol(d)){
        if(names(d)[j] %in% variables$fieldName){
          type <- variables$colClass[which(variables$fieldName==names(d)[j])[1]]
          if(type == "numeric")
            d[,j] <- as.numeric(d[,j])
          if(type == "character")
            d[,j] <- as.character(d[,j])
        }
      }

      #cols <- sapply(d, is.logical) # No  clear way to keep a character column being interpreted as logical on read, so convert.
      #d[,cols] <- lapply(d[,cols], as.character)
      if(length(f) > 1){
        for(j in 2:length(f)){
          d.next <- read.csv(f[j][[1]], header = T, stringsAsFactors = F)
          for(j in 1:ncol(d.next)){
            if(names(d.next)[j] %in% variables$fieldName){
              type <- variables$colClass[which(variables$fieldName==names(d.next)[j])[1]]
              if(type == "numeric")
                d.next[,j] <- as.numeric(d.next[,j])
              if(type == "character")
                d.next[,j] <- as.character(d.next[,j])
            }
          }
          #cols <- sapply(d.next, is.logical)
          #d.next[,cols] <- lapply(d.next[,cols], as.character)
          d <- dplyr::full_join(d, d.next)
        }
        write.csv(d, paste0(folder, "/", tables[i], ".csv"), row.names = F)
      }
      writeLines(paste("Stacked ", tables[i]))
    }
  }
  writeLines(paste("Finished: All data is stacked into ", i, " tables!"))
}

# The ultimate function...
#' Given a zipped data file, do a full join of all data files, grouped by table type.
#' This should result in a small number of large files.
#'
#' @param location.package The location of the zip file
#' @return All files are unzipped and one file for each table type is created and written.
#' @export
stackByTable <- function(location.package){
  location.data <- substr(location.package, 1, nchar(location.package)-4)
  unzip.zipfile(zippath = location.package, outpath = location.data, level = "all")
  stackDataFiles(location.data)
}


# CASES
# 1. TIS or AIS, old pub system, NEON.DOM.SITE.DPL.DPNUM.REV.TERMS.HOR.VER.TMI.TABLE.CSV, 12 slots. The variables
# files follow NEON.DOM.SITE.DPL.DPNUM.REV.variables.TIMESTAMP.CSV, 9 slots
# 2. TIS or AIS, new pub system, NEON.DOM.SITE.DPL.DPNUM.REV.TERMS.HOR.VER.TMI.TABLE.YEARMO.PACKAGE.TIMESTAMP.CSV, 14 slots. The variables
# and validation files follow NEON.DOM.SITE.DPL.DPNUM.REV.variables.TIMESTAMP.CSV, 9 slots
# 3. TOS, AIS, AOS, new pub system, NEON.DOM.SITE.DPL.DPNUM.REV.TABLE.YEARMO.PACKAGE.TIMESTAMP.CSV, 11 slots. The variables
# and validation files follow NEON.DOM.SITE.DPL.DPNUM.REV.variables.TIMESTAMP.CSV, 9 slots
# 4. AOP, EC, or externally hosted - do not stack
# 5. TIS, AIS, AOS, old pub system, NEON.DOM.SITE.DPL.DPNUM.REV.TABLE.CSV, 8 slots

# STEPS
# + If download contains no csv files (team = AOP), stop and do not merge anything
# + If data product contains .h5 files (team EC), stop and do not merge anything
# + If data product contains variables files, merge them, but add a new column that indicates year month in case there
# are differences from one month to the next
# + If data product contains validations files, merge them, but add a new column that indicates year month in case there
# are differences from one month to the next
# + If files end with .csv, do not have 'variables' or 'validation' in their name,
#    - If # slots = 8 or 11, then stack by table in order of site, month
#    - If # slots = 12 or 14, then stack by table in order of site, month, and add 1 column for HOR, 1 column for VER

