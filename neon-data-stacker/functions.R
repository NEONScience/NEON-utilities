
#drafting in progress...

library(gdata)

filepath <- "data/NEON_obs-phenology-plant.zip"

get.filesize <- function(filepath){
  fs <- humanReadable(file.size(filepath))
  return(fs)
}

unzip.primary <- function(filepath){
  unzip(filepath, exdir="data")
  f <- unzip(filepath, list = T)
  return(f)
}

unzip.internal <- function(filenames){
  for(i in 1:length(filenames)){
    f <- filenames[i]
    if(substr(f, nchar(f)-3, nchar(f)) == ".zip")
      unzip(f, exdir=substr(f, 1, nchar(f)-4))
  }
}

