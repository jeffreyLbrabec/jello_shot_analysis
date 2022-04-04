read_wrapper <- function(files, mouse_id, slope, date_dir_name) {
  
  reading_file <- read_csv(files, show_col_types = FALSE)
  
  processed_reading <- reading_file %>%
    janitor::clean_names() %>% 
    filter(reading != reading_file$reading[1]) %>% 
    mutate(time = parse_date_time(time, "ymd HMS"), #Pay attention to year, month, day orientation
           time_s = row_number(time)) %>%
    mutate(across(reading, as.numeric)) %>% 
    drop_na()
  
  calculated_weights <- calculate_weights(data = processed_reading,
                                          slope_val = slope)
  
  smoothed_data <- smooth_data(data = calculated_weights,
                               smooth_method = "loess",
                               span_val = 0.6)

  write_csv(smoothed_data$data, paste(here("results/pred_readings"), date_dir_name, paste0(mouse_id, "_smoothed.csv"), sep = "/"))
  ggsave(filename = paste(here("results/pred_readings"), date_dir_name, paste0(mouse_id, "_smoothed.png"), sep = "/"),
         plot = smoothed_data$plot)
  
  return(smoothed_data)
  
}