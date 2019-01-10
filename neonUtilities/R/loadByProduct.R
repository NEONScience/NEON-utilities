##############################################################################################
#' @title Get files from NEON API, stack tables, and load into the current environment

#' @author
#' Claire Lunch \email{clunch@battelleecology.org}

#' @description
#' Pull files from the NEON API, by data product, merge data for each table, and read into the current R environment
#'
#' @param dpID The identifier of the NEON data product to pull, in the form DPL.PRNUM.REV, e.g. DP1.10023.001
#' @param site Either the string 'all', or the four-letter code of a single NEON site, e.g. 'CLBJ'. Future versions may allow more options for subsetting than one or all sites. Defaults to all.
#' @param package Either 'basic' or 'expanded', indicating which data package to download. Defaults to basic.
#' @param avg Either the string 'all', or the averaging interval to download, in minutes. Only applicable to sensor (IS) data. Defaults to 'all'.
#' @param check.size T or F, should the user be told the total file size before downloading? Defaults to T. When working in batch mode, or other non-interactive workflow, use check.size=F.

#' @return A named list of all the data tables in the data product downloaded, plus a validation file and a variables file, as available.

#' @examples
#' \dontrun{
#' # To download plant foliar properties data from all sites, expanded data package:
#' cfc <- loadByProduct(dpID="DP1.10026.001", site="all", package="expanded")
#' }

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# changelog and author contributions / copyrights
#   Claire Lunch (2019-01-10)
#     original creation
##############################################################################################

loadByProduct <- function(dpID, site="all", package="basic", avg="all", 
                          check.size=TRUE) {
  
  # error message if package is not basic or expanded
  if(!package %in% c("basic", "expanded")) {
    stop(paste(package, "is not a valid package name. Package must be basic or expanded", sep=" "))
  }

  # error message if dpID isn't formatted as expected
  if(regexpr("DP[1-4]{1}.[0-9]{5}.001",dpID)!=1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", sep=" "))
  }
  
  # error message if dpID can't be downloaded by zipsByProduct()
  if(substring(dpID, 5, 5)==3) {
    stop(paste(dpID, "is a remote sensing data product and cannot be loaded directly to R with this function. Use the byFileAOP() function to download locally.", sep=" "))
  }

  if(dpID %in% c("DP1.00033.001", "DP1.00042.001")) {
    stop(paste(dpID, "is a phenological image product, data are hosted by Phenocam.", sep=" "))
  }
  
  # create a temporary directory to save to
  temppath <- file.path(tempdir(), paste("zips", format(Sys.time(), "%Y%m%d%H%M%S"), sep=""))
  dir.create(temppath)
  
  # pass the request to zipsByProduct() to download
  zipsByProduct(dpID=dpID, site=site, package=package, avg=avg, check.size=check.size, 
                savepath=temppath, load=T)
  
  # stack and load the downloaded files using stackByTable
  out <- stackByTable(filepath=paste(temppath, "/filesToStack", substr(dpID, 5, 9), sep=""), 
                      savepath="envt", folder=T)
  return(out)

}


