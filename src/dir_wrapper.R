#Function to get into data directories

dir_wrapper <- function(dir) {
  
  data_files <- list.files(dir, full.names = TRUE)
  
  readings <- map(data_files, read_wrapper)
  
  file_names <- map(list.files(dir), str_remove, ".csv")
  
  names(readings) <- file_names
  
  return(readings)
  
}