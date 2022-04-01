#Function to get into data directories

dir_wrapper <- function(dir) {
  
  data_files <- list.files(dir, full.names = TRUE)
  
  map(data_files, read_wrapper)
  
}