# Set the base url for the NEON API

nu.globals <- new.env()

.onLoad <- function(libname, pkgname) {
  
  nu.globals$baseurl <- Sys.getenv("NEON_API_URL")
  if(nu.globals$baseurl=="") {
    nu.globals$baseurl <- "https://data.neonscience.org/api/v0/"
  }
  
}

