# Set the base url for the NEON API

nu.globals <- new.env()
nu.globals$baseurl <- "https://data.neonscience.org/api/v0/"

.onAttach <- function(libname, pkgname) {
  
  nu.globals$baseurl <- Sys.getenv("NEON_API_URL")
  if(nu.globals$baseurl=="") {
    nu.globals$baseurl <- "https://data.neonscience.org/api/v0/"
  }
  
}


# print the base url for the NEON API

printBaseUrl <- function() {
  
  print(nu.globals$baseurl)
  
}
