#Function to get into data directories

dir_wrapper <- function(dir, calibrations) {
  
  data_files <- tibble(files = list.files(dir, full.names = TRUE),
                       mouse_id = str_remove(list.files(dir), ".CSV")) %>% 
    inner_join(calibrations, by = "mouse_id") %>% 
    select(files, mouse_id, slope)
   
  readings <- pmap(data_files, possibly(read_wrapper, NULL))
  
  #names(readings) <- file_names
  
  return(readings)
  
}