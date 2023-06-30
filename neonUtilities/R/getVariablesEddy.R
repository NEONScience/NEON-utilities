##############################################################################################
#' @title Get variable names and units from SAE H5 files

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Extract variable names and units from SAE H5 files and return in user-friendly form. Used in stackEddy(), not intended for independent use.
#'
#' @keywords internal
#' @param tabList A list of SAE data tables
#' @return A table of variable names and units, aggregated from the input tables

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

# Changelog and author contributions / copyrights
#   Claire Lunch (2023-05-30)
##############################################################################################

getVariablesEddy <- function(tabList){

  variables <- character(5)
  for(p in 1:length(tabList[[1]])) {
    if(!is.null(attributes(tabList[[1]][[p]])$unit)) {
      var.nm <- strsplit(names(tabList[[1]])[p], 
                         split="/", fixed=T)[[1]][c(3,4,length(strsplit(names(tabList[[1]])[p], 
                                                                        split="/", fixed=T)[[1]]))]
      if(length(attributes(tabList[[1]][[p]])$unit)>1) {
        var.nm <- matrix(var.nm, ncol=3, nrow=length(attributes(tabList[[1]][[p]])$unit), byrow=T)
        if(length(attributes(tabList[[1]][[p]])$unit)==length(attributes(tabList[[1]][[p]])$names)) {
          var.nm <- cbind(var.nm, attributes(tabList[[1]][[p]])$names)
        } else {
          if("index" %in% attributes(tabList[[1]][[p]])$names) {
            var.nm <- cbind(var.nm, 
                            attributes(tabList[[1]][[p]])$names[-which(attributes(tabList[[1]][[p]])$names=="index")])
          } else {
            var.nm <- cbind(var.nm, 
                            attributes(tabList[[1]][[p]])$names[-which(attributes(tabList[[1]][[p]])$names 
                                                                         %in% c("timeBgn","timeEnd"))])
          }
        }
        var.nm <- cbind(var.nm, attributes(tabList[[1]][[p]])$unit)
        variables <- rbind(variables, var.nm)
      } else {
        variables <- rbind(variables, c(var.nm, "", attributes(tabList[[1]][[p]])$unit))
      }
    }
  }
  variables <- data.frame(unique(variables))

  if(nrow(variables)==1) {
    variables <- NA
  } else {
    variables <- variables[-1,]
    colnames(variables) <- c("category","system","variable","stat","units")
    rownames(variables) <- 1:nrow(variables)
  }
  
  return(variables)
}

