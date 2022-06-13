#Function to calculate weights

calculate_weights <- function(data = NULL, slope_val = NULL) {
  
  calculated_weights <- data %>% 
    mutate(measured_weight = abs(reading)/slope_val)
  
  return(calculated_weights)
}