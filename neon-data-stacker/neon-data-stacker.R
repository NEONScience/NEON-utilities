# stack-neon-data.R

require(gdata)
source("functions.R")

location.package <- "data/NEON_cond-groundwater.zip"
location.data <- substr(location.package, 1, nchar(location.package)-4)
unzip.zipfile(l1 = location.data, inpath = location.package, outpath = location.data, level = "all")

zip.size <- get.filesize(zip.filepath)
zip.files <- list.files.inZip(zip.filepath)
zip.names <- list.zipfiles(zip.filepath)

