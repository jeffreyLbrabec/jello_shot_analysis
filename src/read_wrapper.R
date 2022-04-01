read_wrapper <- function(files, mouse_id, slope) {
  
  reading_file <- read_csv(files, show_col_types = FALSE)
  
  processed_reading <- reading_file %>%
    janitor::clean_names() %>% 
    filter(reading != reading_file$reading[1]) %>% 
    mutate(time = parse_date_time(time, "ymd HMS"), #Pay attentino to year, month, day orientation
           time_s = row_number(time)) %>%
    mutate(across(reading, as.numeric)) %>% 
    drop_na()
  
  calculated_weights <- calculate_weights(data = processed_reading,
                                          slope_val = slope)
  
  smoothed_data <- smooth_data(data = calculated_weights, 
                               smooth_method = "loess", 
                               span_val = 0.6)
  
  return(smoothed_data)
  
}