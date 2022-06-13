#Function for binning mouse eating jello time-series data

#data should be of the format oct7_consumption_cleaned.csv
#bins should be picked in
#time increment will be used 

bin_and_convert <- function(data = NULL, 
                            mouse_id,
                            bin = c("5 secs", "10 secs", "30 secs", "60 secs"),
                            time_increment = c("time_h", "time_m", "time_s"),
                            results_dir,
                            dir_name) {
  
  
  # Set custom time bin and calculate weight as mean for that time window ------------------
  binned_data <- data %>% 
    select(time, pred_weight) %>% 
    mutate(time = parse_date_time(time, "ymd HMS")) %>% 
    mutate(binned_time = cut(data$time, breaks = {{ bin }})) %>% 
    group_by(binned_time) %>% 
    summarize(mean_weight = mean(pred_weight)) %>% 
    ungroup() %>% 
    as.data.frame() %>% #added to stop R from rounding when doing mean(weight)
    mutate(
      binned_time = parse_date_time(binned_time, "ymd HMS"),
      !!{{ time_increment }} := seq(from = 0, 
                                    to = (length(.$mean_weight)-1) * as.numeric(difftime(.$binned_time[2], 
                                                                                         .$binned_time[1])), 
                                    by = as.numeric(difftime(.$binned_time[2], 
                                                             .$binned_time[1])))
    )
  
  # Convert time to hours ---------------------------------------------------
  time_col <- colnames(binned_data)[3]
  time_unit <- str_sub(time_col, start = -1)
  
  if(time_unit == "s") {
    
    trimmed_data_to_h <- binned_data %>% 
      mutate(time_h = `$`(binned_data, !!time_increment) / 60 / 60)
    
  }else if(time_unit == "m") {
    
    trimmed_data_to_h <- binned_data %>% 
      mutate(time_h = `$`(binned_data, !!time_increment) / 60)
    
  }else if(time_unit == "h") {
    
    trimmed_data_to_h <- binned_data
    
  }
  
  
  final_data <- trimmed_data_to_h %>% mutate(cum_consump = .$mean_weight[1] - .$mean_weight)
  write_csv(final_data, paste(results_dir, dir_name, paste0(mouse_id, "_binned.csv"), sep = "/"))
  return(final_data)
  
}