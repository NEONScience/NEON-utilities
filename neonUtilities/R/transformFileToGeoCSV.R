##############################################################################################
#' @title Transform NEON CSV file to GeoCSV
#'
#' @author
#' Christine Laney \email{claney@battelleecology.org}

#' @description
#' Read in a NEON monthly data zip file and parse the respective variables file to create a new GeoCSV file
#'
#' @param infile The path to the file that needs to be parsed
#' @param varfile The path to the variables file needed to parse the infile
#' @param outfile The path where the new GeoCSV file should be placed
#' @return The same data file with a GeoCSV header

#' @references
#' License: GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007

#' @export

# Changelog and author contributions / copyrights
#   2018-01-09 (Christine Laney): Created function
#   2018-04-04 (Christine Laney): Updated function documentation
#   2018-04-17 (Christine Laney): Added field descriptions under the keyword field_long_name
##############################################################################################

transformFileToGeoCSV <- function(infile, varfile, outfile){
  datafile <- utils::read.csv(infile, header = TRUE, stringsAsFactors = FALSE)
  vars <- utils::read.csv(varfile, header = TRUE, stringsAsFactors = FALSE)
  dfsplit <- strsplit(infile, "\\/")
  dfname <- dfsplit[[1]][length(dfsplit[[1]])]

  line_dataset <- "# dataset: GeoCSV 2.0\n"
  line_title <- paste0("# title: ", getTitle(dfname))
  line_institution <- "# institution: National Ecological Observatory Network (NEON)"
  line_attribution <- "# attribution: http://www.neonscience.org/data-resources/data-usage-citations"
  line_resource <- "# resource: http://data.neonscience.org"
  line_field_long_name <- "# field_long_name: "
  line_field_unit <- "# field_unit: "
  line_field_type <- "# field_type: "
  line_delimiter <- '# delimiter: ","'
  for(i in 1:ncol(datafile)){
    type <- vars$dataType[which(names(datafile)[i] == vars$fieldName)][1]
    if(is.na(type)){type = "NA"}
    if(type == "dateTime"){unit = "ISO_8601"}
    if(type %in% c("unsigned integer","signed integer")){type = "integer"}
    if(type == "real"){type = "float"}
    if(!(type %in% c("ISO_8601","integer","float"))){type = "string"}

    unit <- vars$units[which(names(datafile)[i] == vars$fieldName)][1]
    if(is.na(unit) || unit == "NA"){unit = "unitless"}

    long_name <- vars$description[which(names(datafile)[i] == vars$fieldName)][1]

    if(i == 1){
      line_field_long_name <- paste0(line_field_long_name, long_name)
      line_field_unit <- paste0(line_field_unit, unit)
      line_field_type <- paste0(line_field_type, type)
    }
    if(i > 1){
      line_field_long_name <- paste(line_field_long_name, long_name, sep = ", ")
      line_field_type <- paste(line_field_type, type, sep = ", ")
      line_field_unit <- paste(line_field_unit, unit, sep = ", ")
    }
  }

  addText <- function(object, outfile, sep = "\n", append = TRUE){
    cat(object, file = outfile, sep = sep, append = append)
  }
  addText(line_dataset, outfile, append = FALSE)
  addText(line_title, outfile)
  addText(line_institution, outfile)
  addText(line_attribution, outfile)
  addText(line_resource, outfile)
  addText(line_field_long_name, outfile)
  addText(line_field_type, outfile)
  addText(line_field_unit, outfile)
  utils::write.table(datafile, file = outfile, row.names = FALSE, append = TRUE, sep = ",",
              fileEncoding = "UTF-8", quote = FALSE)
}


