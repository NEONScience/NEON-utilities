
#drafting in progress...



get.filesize <- function(filepath){
  fs <- humanReadable(file.size(filepath), standard="SI")
  return(fs)
}

# given the top level zip file,
# return dataframe of all of the files within it without unzipping the file
list.files.inZip <- function(inpath){
  unzip(zipfile = inpath, list = T)
}

# given the data frame of all the files within the top level zip file,
# return just an array of just the zip file names (no pdfs, etc.)
list.zipfiles <- function(inpath){
  df <- unzip(zipfile = inpath, list = T)
  ns <- df[,1]
  fn <- ns[which(substr(ns, nchar(ns)-3, nchar(ns)) == ".zip")]
  return(fn)
}

# given the top level zipfile and whether to unzip just the top level or all levels,
# unzip files
unzip.zipfile <- function(l1, inpath, outpath, zname, level){
  if(level == "top"){
    unzip(zipfile = inpath, exdir=outpath)
  }
  if(level == "all"){
    unzip(zipfile = inpath, exdir=outpath)
    zps <- list.zipfiles(inpath)
    for(i in 1:length(zps)){
      p <- paste0(l1, "/", zps[i])
      unzip(p, exdir=substr(p, 1, nchar(p)-4), overwrite = T)
      if (file.exists(p)) file.remove(p)
    }   
  }
}
  
# if the top level zipfile has been fully unzipped, 
# return the names of the data tables within each folder
# fnames = full names - if true, then return the full file names including enclosing folders
find.datatables <- function(inpath, fnames=T){
  fs <- list.dirs(inpath)
  fs <- fs[-c(1,2)]
  fls <- character()
  for(i in 1:length(fs)){
    fls <- c(fls, list.files(fs[i], full.names=fnames))
  }
  return(fls[which(substr(fls, nchar(fls)-3, nchar(fls)) == ".csv")])
}

# return the names of the data table types that are present in the dataset
find.tables.unique <- function(inpath){
  fs <- list.dirs(inpath)
  fs <- fs[-c(1,2)]
  fls <- character()
  for(i in 1:length(fs)){
    fls <- c(fls, list.files(fs[i], full.names=F))
  }
  fls <- fls[which(substr(fls, nchar(fls)-3, nchar(fls)) == ".csv")]
  ts <- character()
  for(i in 1:length(fls)){
    table <- substr(fls[i], 29, nchar(fls[i]))
    if(!(table %in% ts)){
      ts <- c(ts, table)
    }
  }
  return(ts)
}

# for each of the dataset's tables, bind all of the data from all of the monthly packages
write.tables.all <- function(inpath, outpath){
  ts <- find.tables.unique(inpath)
  fls <- find.datatables(inpath, T)
  for(i in 1:length(ts)){
    f <- fls[grep(pattern = ts[i], fls)]
    d <- read.csv(f[1], header = T, stringsAsFactors = F)
    if(length(f) > 1){
      for(j in 2:length(f)){
        d.next <- read.csv(f[j], header = T, stringsAsFactors = F)
        d <- rbind(d, d.next)
      }
      write.csv(d, paste0(outpath, "/", ts[i]), row.names = F)
    }
  }
}
