#A function to read in the analysis data

get_jello_consumption <- function(dir, calibrations) {
  
  date_dirs <- list.files(dir, full.names = TRUE)
  
  readings <- map(date_dirs, dir_wrapper)
  
  dir_names <- list.files(dir)
  
  names(readings) <- dir_names
  
  return(readings)
  
}