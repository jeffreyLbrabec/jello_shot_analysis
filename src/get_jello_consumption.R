#A function to read in the analysis data

get_jello_consumption <- function(dir, calibrations) {
  
  date_dirs <- list.files(dir, full.names = TRUE)
  
  date_dir_names <- list.files(dir)
  
  walk(date_dir_names, ~ dir.create(file.path(here("results/pred_readings"), .)))
  
  readings <- map2(date_dirs, date_dir_names, possibly(dir_wrapper, NULL), calibrations)

  dir_names <- list.files(dir)
  
  names(readings) <- dir_names
  
  return(readings)
  
}