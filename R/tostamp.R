#' Set time as string
#' 
#' Author: Philipp Franikowski, restructuring by Lea Musiolek
#' 
#' @param x Input timestamp in seconds(?)
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' 
#' Used in layout_staytime_tables.R
#'
#' @export

tostamp <- function(x) {
  if (is.na(x)) {
    return("-")
  }
  
  sign_char <- ifelse(x < 0, "-", "")
  abs_x <- abs(x)
  
  # Convert absolute value to period
  p <- abs_x %>%
    round() %>%
    as.integer() %>%
    lubridate::seconds_to_period()
  
  # Format as MM:SS
  time_str <- sprintf("%02d:%02d", lubridate::minute(p), 
                      lubridate::second(p))
  
  # Combine sign and time
  paste0(sign_char, time_str)
}