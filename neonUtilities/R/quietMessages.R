##############################################################################################
#' @title Will suppress all output messages, while retaining the output dataframe

#' @author
#' Nate Mietkiewicz \email{mietkiewicz@battelleecology.org}

#' @description Used to quiet all output messages
#'
#' @param toBeQuieted Input to be quieted

#' @return The expected output without associated messages/warnings.
#' @keywords internal

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Nate Mietkiewcz (2020-02-27): original creation

##############################################################################################

quietMessages <- function(toBeQuieted) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(toBeQuieted))
}
