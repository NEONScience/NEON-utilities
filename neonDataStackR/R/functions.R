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
#' @param filepath The path to a zip file
#' @return A list of filenames within the given zip file
#' @export
list.files.inZip <- function(filepath){
  unzip(zipfile = filepath, list = T)
}

#' Given the data frame of all the files within the top level zip file,
#' return just an array of just the zip file names (no pdfs, etc.)
#'
#' @param filepath The path to a zip file
#' @return An array of all zip files contained within the focal zip file
#' @export
list.zipfiles <- function(filepath){
  df <- unzip(zipfile = filepath, list = T)
  ns <- df[,1]
  fn <- ns[which(substr(ns, nchar(ns)-3, nchar(ns)) == ".zip")]
  return(fn)
}

#' Unzip a zip file either at just the top level or recursively through the file.
#'
#' @param inpath The filepath of the input file
#' @param outpath The name of the folder to save unpacked files to
#' @param level Whether the unzipping should occur only for the 'top' zip file, or unzip 'all' recursively
#' @return A folder structure of unzipped files
unzip.zipfile <- function(inpath, outpath, level){
  if(level == "top"){
    unzip(zipfile = inpath, exdir=outpath)
  }
  if(level == "all"){
    unzip(zipfile = inpath, exdir=outpath)
    zps <- list.zipfiles(inpath)
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
find.datatables <- function(filepath, fnames=T){
  fs <- list.dirs(filepath)
  fs <- fs[-c(1,2)]
  fls <- character()
  for(i in 1:length(fs)){
    fls <- c(fls, list.files.inZip(fs[i], full.names=fnames))
  }
  return(fls[which(substr(fls, nchar(fls)-3, nchar(fls)) == ".csv")])
}

#' Find the data tables that are present in the dataset (e.g., 2 minute vs 30 minute, or pinning vs identification data)
#'
#' @param filelist A list of data files
#' @return a list of unique tables
#' @export
find.tables.unique <- function(filelist){
  splitNames <- strsplit(x = filelist, split = "\\.")
  t <- character()
  for (i in 1:length(splitNames)){
    n <- splitNames[[i]][lengths(splitNames[i])-1]
    t <- c(t, n)
  }
  return(unique(t))
}

#' Bind all of the data from all of the monthly packages for each of the dataset's tables
#'
#' @param tables Array of table names returned from find.tables.unique()
#' @param outpath The location that files have been unpacked to
#' @return A set of stacked files
stackfiles <- function(tables, outpath){
  fls <- find.datatables(outpath, T)
  for(i in 1:length(tables)){
    f <- fls[grep(pattern = tables[i], fls)]
    d <- read.csv(f[1], header = T, stringsAsFactors = F)
    if(length(f) > 1){
      for(j in 2:length(f)){
        d.next <- read.csv(f[j], header = T, stringsAsFactors = F)
        d <- rbind(d, d.next)
      }
      d_noDup <- unique(d)
      write.csv(d_noDup, paste0(outpath, "/", tables[i], ".csv"), row.names = F)
    }
    writeLines(paste("Stacked ", tables[i]))
  }
  writeLines(paste("Finished: All data is stacked into ", i, " tables!"))
}

#' Unzip and stack the data in all data files, grouped by table type. This should result in a small number of large files.
#'
#' @param filepath The location of the zip file
#' @return All files are unzipped and one file for each table type is created and written.
#' @export
stackByTable <- function(location.package){
  location.data <- substr(location.package, 1, nchar(location.package)-4)
  unzip.zipfile(inpath = location.package, outpath = location.data, level = "all")
  d_fnames_full <- find.datatables(location.data, T)
  d_fnames_notfull <- find.datatables(location.data, F)
  tables <- find.tables.unique(d_fnames_notfull)
  stackfiles(tables, location.data)
}
