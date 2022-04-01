#A function to read in the analysis data

get_jello_consumption <- function(dir, calibrations) {
  
  date_dirs <- list.files(dir, full.names = TRUE)
  
  map(date_dirs, dir_wrapper)
  
}