# stack-neon-data.R

require(gdata)
source("functions.R")

location.data <- "data"
location.output <- "data"
zip.name <- "NEON_obs-phenology-plant.zip"
zip.filepath <- "data/NEON_obs-phenology-plant.zip"

zip.size <- get.filesize(zip.filepath)
zip.files <- list.files.inZip(zip.filepath)
zip.names <- list.zipfiles(zip.filepath)
unzip.zipfile(inpath = zip.filepath, outpath = location.output, zname = zip.name, level = "top")
d_fnames_full <- find.datatables(location.data, T)
d_fnames_notfull <- find.datatables(location.data, F)
tables <- find.tables.unique(location.data)
write.tables.all(location.data, location.output)
