#function for smoothing data before binning

smooth_data <- function(data = NULL,
                        smooth_method = c("loess", "spline"),
                        span_val = 0.25,
                        spar_val = 0.9) {
  
  # Smooth data -------------------------------------------------------------
  
  weight_filt_data <- data %>% 
    dplyr::filter(weight < 30)
  
  if(smooth_method == "loess") {
    
    loess_pred_weight_data <- list()
    
    lowpass_loess <- loess(weight_filt_data$weight ~ weight_filt_data$time_s, 
                           data = weight_filt_data, 
                           span = span_val)
    
    loess_pred_weight_data$data <- weight_filt_data %>% 
      mutate(pred_weight = predict(lowpass_loess, weight_filt_data$time_s))
    
    
    loess_pred_weight_data$plot <- ggplot(weight_filt_data, mapping = aes(x = time_s, y = weight)) +
      geom_point() +
      geom_line(data = bind_cols(weight_filt_data$time_s, 
                                 predict(lowpass_loess, weight_filt_data$time_s)) %>% 
                  rename(time_s = ...1, pred_weight = ...2), 
                mapping = aes(time_s, pred_weight), color = "indianred", size = 2) +
      ggtitle("Loess-Smoothed Jello Consumption Data")
    
    return(loess_pred_weight_data)
    
    
  }else if(smooth_method == "spline") {
    
    spline_pred_weight_data <- list()
    
    lowpass_spline <- smooth.spline(x = weight_filt_data$time_s,
                                    y = weight_filt_data$weight,
                                    spar = spar_val)
    
    spline_pred_weight_data$data <- weight_filt_data %>% 
      mutate(pred_weight = predict(lowpass_spline, weight_filt_data$time_s)$y)
    
    spline_pred_weight_data$plot <- ggplot(weight_filt_data, mapping = aes(x = time_s, y = weight)) +
      geom_point() +
      geom_line(data = bind_cols(weight_filt_data$time_s, 
                                 predict(lowpass_spline, weight_filt_data$time_s)$y) %>% 
                  rename(time_s = ...1, pred_weight = ...2), 
                mapping = aes(time_s, pred_weight), color = "indianred", size = 2) +
      ggtitle("Spline-Smoothed Jello Consumption Data")
    
    return(spline_pred_weight_data)
  }
  
  
}