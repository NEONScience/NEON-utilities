##############################################################################################
#' @title Get a genericized version of a readme file by removing info that is specific to a site-month or data query.

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Read in a complete readme file, and return a genericized file.

#' @param savepath A data frame containing the readme contents.
#' @param dpID The data product identifier
#' 
#' @keywords internal

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2025-06-12 (Claire Lunch): Adapted from getReadmePublicationDate()

##############################################################################################
formatReadme <- function(savepath, dpID) {

  dpnum <- substring(dpID, 5, 9)
  
  txt_file <- savepath
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
  
  disclaimer <- c("###################################\n",
                  "########### Disclaimer ############\n",
                  'This is the most recent readme publication based on all site-date combinations used during stackByTable.\nInformation specific to the query, including sites and dates, has been removed. The remaining content reflects general metadata for the data product.\n',
                  "##################################\n",
                  "\n")
  disclaimer <- data.frame(disclaimer)
  txt_file <- data.frame(txt_file)
  names(disclaimer) <- "V1"
  names(txt_file) <- "V1"
  txt_file <- rbind(disclaimer, txt_file)
  return(txt_file)
}
