read_wrapper <- function(files, mouse_id, slope, date_dir_name, results_dir) {
  
  reading_file <- read_csv(files, show_col_types = FALSE)
  
  processed_reading <- reading_file %>%
    janitor::clean_names() %>% 
    drop_na() %>% 
    #filter(reading != reading_file$reading[1]) %>% 
    mutate(time = parse_date_time(time, "ymd HMS"),
           time_s = row_number()) %>%
    mutate(across(reading, as.numeric))
  
  calculated_weights <- calculate_weights(data = processed_reading,
                                          slope_val = slope)
  
  smoothed_data <- smooth_data(data = calculated_weights,
                               span_val = 0.6)

  write_csv(smoothed_data$data, paste(results_dir, date_dir_name, paste0(mouse_id, "_smoothed.csv"), sep = "/"))
  ggsave(filename = paste(results_dir, date_dir_name, paste0(mouse_id, "_smoothed.png"), sep = "/"),
         plot = smoothed_data$plot)
  
  return(smoothed_data)
  
}