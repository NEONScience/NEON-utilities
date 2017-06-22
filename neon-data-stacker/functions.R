
#drafting in progress...

stackfiles <- function(filepath){
  f <- filepath
  m <- "Data package name:"
  m <- add.message(m, f)
  m <- add.message(m, "\nSize:")
  m <- add.message(m, get.filesize(f))
}

get.filesize <- function(filepath){
  fs <- humanReadable(file.size(filepath), units = "auto", standard = "SI")
  return(fs)
}

add.message <- function(message, component){
  message <- cat(message, component)
  return(message)
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
unzip.zipfile <- function(l1, inpath, outpath, level){
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
      writeLines(paste("Unpacked ", zps[i], "\n"))
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
find.tables.unique <- function(filelist){
  splitNames <- strsplit(x = filelist, split = "\\.")
  t <- character()
  for (i in 1:length(splitNames)){
    n <- splitNames[[i]][lengths(splitNames[i])-1]
    t <- c(t, n)
    }
  return(unique(t))
}

# for each of the dataset's tables, bind all of the data from all of the monthly packages
write.tables.all <- function(tables, datapath){
  fls <- find.datatables(datapath, T)
  for(i in 1:length(tables)){
    f <- fls[grep(pattern = tables[i], fls)]
    d <- read.csv(f[1], header = T, stringsAsFactors = F)
    if(length(f) > 1){
      for(j in 2:length(f)){
        d.next <- read.csv(f[j], header = T, stringsAsFactors = F)
        d <- rbind(d, d.next)
      }
      d_noDup <- unique(d)
      write.csv(d_noDup, paste0(datapath, "/", tables[i], ".csv"), row.names = F)
    }
    writeLines(paste("Unpacked ", tables[i], "\n"))
  }
  writeLines(paste("Finished: Unpacked ", i, " tables!"))
}

unzip.all <- function(location.package){
  location.data <- substr(location.package, 1, nchar(location.package)-4)
  unzip.zipfile(l1 = location.data, inpath = location.package, outpath = location.data, level = "all")
  d_fnames_full <- find.datatables(location.data, T)
  d_fnames_notfull <- find.datatables(location.data, F)
  tables <- find.tables.unique(d_fnames_notfull)
  write.tables.all(tables, location.data)
}

zip.size <- get.filesize(zip.filepath)
zip.files <- list.files.inZip(zip.filepath)
zip.names <- list.zipfiles(zip.filepath)