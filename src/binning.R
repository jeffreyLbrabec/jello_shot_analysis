get_binned_data <- function(dir, bin, time_increment, results_dir) {
  
  date_dirs <- list.files(dir, full.names = TRUE)
  
  date_dir_names <- list.files(dir)
  
  dir.create(file.path(results_dir))
  
  walk(date_dir_names, ~ dir.create(file.path(results_dir, .)))
  
  date_bins <- map2(date_dirs, 
                    date_dir_names, 
                    dir_wrapper, 
                    bin = bin, 
                    time_increment = time_increment, 
                    results_dir = results_dir, 
                    binned = TRUE)
  
  
}

bin_wrapper <- function(file, mouse_id, dir_name, bin, time_increment, results_dir) {
  
  
  data <- read_csv(file, show_col_types = FALSE)
  
  
  # Set custom time bin and calculate weight as mean for that time window ------------------
  if(nrow(data) < 900) {
    
    print("This File is too short.")
    
  }else {
    
    bin_and_convert(data = data, mouse_id = mouse_id, dir_name = dir_name, bin = bin, time_increment = time_increment, results_dir = results_dir)
    
  }
}
  
  