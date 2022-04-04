#Function to get into data directories

dir_wrapper <- function(dir, dir_name, calibrations) {
  
  data_files <- tibble(files = list.files(dir, full.names = TRUE),
                       mouse_id = str_remove(list.files(dir), ".CSV")) %>% 
    inner_join(calibrations, by = "mouse_id") %>% 
    select(files, mouse_id, slope)
  
  plan(multisession)
  readings <- future_pmap(data_files, possibly(read_wrapper, NULL), dir_name)
  
  names(readings) <- data_files$mouse_id
  
  return(readings)
  
}