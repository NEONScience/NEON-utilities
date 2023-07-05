##############################################################################################
#' @title Scrape the publication date from each ReadMe file

#' @author
#' Nathan Mietkiewicz \email{mietkiewicz@battelleecology.org}

#' @description
#' Given a directory, this will recursively list all of the ReadMe files that were unzipped.
#' This should result in a single text file with a list of all of the publication dates from the ReadMe file.

#' @param savepath The root folder directory where the ReadMe files are located.
#' @param out_filepath The output directory and filename.
#' @param dpID The data product identifier

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2019-10-08 (Nathan Mietkiewicz): Original creation

##############################################################################################
getReadmePublicationDate <- function(savepath, out_filepath, dpID) {

  dpnum <- substring(dpID, 5, 9)
  out_filepath_name <- paste0(out_filepath, '/readme_', dpnum, '.txt')

  if(file.exists(out_filepath_name)) {
    unlink(out_filepath_name)
  }
  #writeLines("Stacking ReadMe documentation")
  readme_list <- list.files(savepath, pattern = '.readme.',
                            recursive = TRUE, full.names = TRUE)
  if(length(readme_list)==0) {
    writeLines("No readme file found.\n")
  } else {

    readme_recent <- getRecentPublication(readme_list)[[1]]
    txt_file <- utils::read.delim(readme_recent, header=FALSE, quote="", stringsAsFactors=FALSE)
    txt_file <- txt_file$V1[grep("Date-Time", txt_file$V1, invert=TRUE)]

    tables <- table_types[which(table_types$productID==dpID),]
    if(nrow(tables)>0) {
      qInd <- grep('QUERY', txt_file)
      dPackInd <- grep('CONTENTS', txt_file)
      downPackInd <- grep('Basic download package', txt_file)
      
      txt_file[I(dPackInd+3)] <- paste('This data product contains up to', nrow(tables), 'data tables:')
      txt_file[I(dPackInd+5):I(dPackInd+4+nrow(tables))] <- paste(tables$tableName, tables$tableDesc, sep=' - ')
      txt_file[I(dPackInd+5+nrow(tables))] <- 'If data are unavailable for the particular sites and dates queried, some tables may be absent.'
      txt_file <- txt_file[-c(qInd:I(dPackInd-2), I(dPackInd+6+nrow(tables)):I(downPackInd-1))]
    }
    
    cat("###################################\n", file = out_filepath_name)
    cat("########### Disclaimer ############\n", file = out_filepath_name, append=TRUE)
    cat('This is the most recent readme publication based on all site-date combinations used during stackByTable.\nInformation specific to the query, including sites and dates, has been removed. The remaining content reflects general metadata for the data product.\n', file = out_filepath_name, append=TRUE)
    cat("##################################\n", file = out_filepath_name, append=TRUE)
    cat("\n", file = out_filepath_name, append=TRUE)
    utils::write.table(txt_file, out_filepath_name, append=TRUE, 
                       row.names=FALSE, col.names=FALSE, quote=FALSE)
  }
}
