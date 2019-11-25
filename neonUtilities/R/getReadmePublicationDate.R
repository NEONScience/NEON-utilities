##############################################################################################
#' @title Scrape the publication date from each ReadMe file

#' @author
#' Nathan Mietkiewicz \email{mietkiewicz@battelleecology.org}

#' @description
#' Given a directory, this will recursively list all of the ReadMe files that were unzipped.
#' This should result in a single text file with a list of all of the publication dates from the ReadMe file.

#' @param savepath The root folder directory where the ReadMe files are located.
#' @param out_filepath The output directory and filename.  

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   2019-10-08 (Nathan Mietkiewicz): Original creation

##############################################################################################

getReadmePublicationDate <- function(savepath, out_filepath) {
  requireNamespace('stringr', quietly = TRUE)
  requireNamespace('dplyr', quiet=TRUE)
  requireNamespace('magrittr', quiet=TRUE)
  requireNamespace('stringr', quiet=TRUE)
  
  out_filepath_name <- paste0(out_filepath, '/readme.txt')
  
  if(file.exists(out_filepath_name)) {
    unlink(out_filepath_name)
  }
  #writeLines("Stacking ReadMe documentation")
  readme_list <- list.files(savepath, pattern = '.readme.',
                            recursive = TRUE, full.names = TRUE)
  
  pub_date_df <- do.call(rbind, pbapply::pblapply(readme_list, function(x) {
    split <- x %>%
      stringr::str_split(., '/') %>%
      unlist() %>%
      .[max(length(unlist(.)))]

    pub_date_str <- suppressWarnings(
      suppressMessages(readr::read_csv(x, col_names=c('X1', 'X2')))) %>%
      dplyr::mutate(readme_filename = as.factor(split)) %>%
      dplyr::select(-c('X1', 'X2'))
    
    return(pub_date_str)
    }))
  
  txt_file <- readr::read_lines(readme_list[[max(length(readme_list))]]) %>%
    .[!stringr::str_detect(., pattern="Date-Time")]
  
  cat("##################################\n", file = out_filepath_name)
  cat("########### Attention ############\n", file = out_filepath_name, append=TRUE)
  cat('Disclaimer: this is a compiled version and may have some information from specific site-dates based on readme files used during stackByTable.\n', file = out_filepath_name, append=TRUE)
  cat("##################################\n", file = out_filepath_name, append=TRUE)
  cat("\n", file = out_filepath_name, append=TRUE)
  readr::write_lines(txt_file, out_filepath_name, append=TRUE)
  cat("\n", file = out_filepath_name, append=TRUE)
  cat("POST STACKING README DOCUMENTATION\n", file = out_filepath_name, append=TRUE)
  cat("----------------------------------\n", file = out_filepath_name, append=TRUE)
  cat("\n", file = out_filepath_name, append=TRUE)
  cat("Each row contains the readme filename used during stackByTable\n", file = out_filepath_name, append=TRUE)
  cat("\n", file = out_filepath_name, append=TRUE)
  utils::write.table(pub_date_df, file=out_filepath_name, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE, quote = FALSE)
}
