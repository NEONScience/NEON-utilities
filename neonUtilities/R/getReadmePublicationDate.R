##############################################################################################
#' @title Scrape the publication date from each ReadMe file

#' @author
#' Nathan Mietkiewicz \email{mietkiewicz@battelleecology.org}

#' @description
#' Given a directory, this will recursively list all of the ReadMe files that were unzipped.
#' This should result in a single text file with a list of all of the publication dates from the ReadMe file.

#' @param savepath The root folder directory where the ReadMe files are located.
#' @param out_filepath_name The output directory and filename.  

#' @examples
#' \dontrun{
#' # To unzip and merge files downloaded from the NEON Data Portal
#' stackByTable("~/NEON_par.zip")
#' 
#' # To unzip and merge files downloaded using zipsByProduct()
#' stackByTable("~/filesToStack00024", folder=T)
#' }

#' @export

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2019-10-08 (Nathan Mietkiewicz): Original creation

##############################################################################################

getReadmePublicationDate <- function(savepath, out_filepath) {
  requireNamespace('dplyr')
  requireNamespace('magrittr')
  requireNamespace('stringr')
  
  out_filepath_name <- paste0(out_filepath, '/readme.txt')

    if(file.exists(out_filepath_name)) {
      unlink(out_filepath_name)
    }
    writeLines("Stacking ReadMe documentation")
    readme_list <- list.files(savepath, pattern = '.readme.',
                              recursive = TRUE, full.names = TRUE)
    
    pub_date_df <- do.call(rbind, pbapply::pblapply(readme_list, function(x) {
      split <- x %>%
        stringr::str_split(., '/') %>%
        unlist() %>%
        .[max(length(unlist(.)))] 
      splitter <- split %>%
        stringr::str_split(., '\\.') %>%
        unlist()
      
      pub_date_str <- suppressWarnings(
        suppressMessages(readr::read_csv(x, col_names=c('X1', 'X2')) %>%
                           dplyr::filter(stringr::str_detect(X1, 'Date-Time for Data Publication'))))

      tmp_pub_date_df <- pub_date_str %>%
        dplyr::mutate(publication_date = lubridate::ymd_hm(stringr::str_remove(X1, 'Date-Time for Data Publication: ')),
                      domain= as.factor(splitter[2]),
                      site = as.factor(splitter[3]),
                      dp_id = as.factor(paste(splitter[4:6], collapse = '.')), 
                      readme_filename = as.factor(split)) %>%
        dplyr::select(-X1, -X2)
      
      return(tmp_pub_date_df)
    }))
    txt_file <- readr::read_lines(readme_list[[max(length(readme_list))]])
    
    readr::write_lines(txt_file, out_filepath_name)
    cat("\n", file = out_filepath_name, append=TRUE)
    cat("\n", file = out_filepath_name, append=TRUE)
    cat("POST STACKING README DOCUMENTATION\n", file = out_filepath_name, append=TRUE)
    cat("----------------------------------\n", file = out_filepath_name, append=TRUE)
    cat("\n", file = out_filepath_name, append=TRUE)
    cat("Compiled list of the data publication record of the files stacked.\nEach row contains information specific to a layer in the stack.\n\nFrom left to right, the publication date, domain, site, data product ID, and original readme filenames are listed.\n\n", 
        file = out_filepath_name, append=TRUE)
    utils::write.table(pub_date_df, file=out_filepath_name, 
                sep=",", append=TRUE, row.names=FALSE, col.names=FALSE, quote = FALSE)
}
