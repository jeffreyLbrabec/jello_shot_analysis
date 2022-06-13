#A function to read in the analysis data

get_jello_consumption <- function(dir, calibrations, results_dir) {
  
  date_dirs <- list.files(dir, full.names = TRUE)
  
  date_dir_names <- list.files(dir)
  
  dir.create(file.path(results_dir))
  
  walk(date_dir_names, ~ dir.create(file.path(results_dir, .)))
  
  readings <- map2(date_dirs, date_dir_names, possibly(dir_wrapper, NA_real_), calibrations, results_dir, binned = FALSE)

  dir_names <- list.files(dir)
  
  names(readings) <- dir_names
  
  return(readings)
  
}