#Function to get into data directories

dir_wrapper <- function(dir, dir_name, calibrations = NULL, binned = FALSE, results_dir, bin = NULL, time_increment = NULL) {
  
  if(binned) {
    
    smoothed_files <- tibble(files = list.files(dir, full.names = TRUE)) %>% 
      filter(!str_detect(files, ".png")) %>% 
      mutate(mouse_id = str_remove(list.files(dir)[!str_detect(list.files(dir), ".png")], "_smoothed.csv"))
    
    binned_files <- map2(smoothed_files$files, 
                         smoothed_files$mouse_id, 
                         bin_wrapper, 
                         dir_name = dir_name,
                         bin = bin, 
                         time_increment = time_increment, 
                         results_dir = results_dir)
    
  }else {
    data_files <- tibble(files = list.files(dir, full.names = TRUE),
                         mouse_id = str_remove(list.files(dir), ".CSV")) %>% 
      inner_join(calibrations, by = "mouse_id") %>% 
      select(files, mouse_id, slope)
    
    plan(multisession)
    readings <- future_pmap(data_files, possibly(read_wrapper, NULL), dir_name)
    
    names(readings) <- data_files$mouse_id
    
    return(readings)
    
  }
  
}