#' Displaying quantiles as horizontal range charts
#' 
#' Author: Philipp Franikowski, restructuring by Lea Musiolek
#' 
#' @param data Dataset being used
#' @param design unclear
#'
#' @return Formatting for a range plot of relevant quantiles with dots
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Used in layout_staytime_tables.R
#'
#' @export

#' @importFrom shiny div
display_dotplot <- function(data, design = NULL) {
  function(value, index) {
    if (is.na(value)) {
      return(value)
    }
    print_value <- to_stamp(value)
    prior <- data[[index, "unit_estimated"]]
    
    if (is.null(design)) {
      q90 <- data[[index, "unit_q90"]]
      q95 <- data[[index, "unit_q95"]]
      # tdiff <- data[[index, "unit_diff"]]
    } else {
      q90 <- data[[index, stringr::str_glue("unit_q90_{design}")]]
      q95 <- data[[index, stringr::str_glue("unit_q95_{design}")]]
      # tdiff <- data[[index, stringr::str_glue("unit_diff_{design}")]]
    }
    
    # color <- case_when(
    #   tdiff > 0 ~ "#fb7185",
    #   tdiff < -60 ~ "#0ea5e9",
    #   .default = "#34d399")
    
    div(
      style = list(display = "flex"),
      
      div(print_value, style = list(flex = "0 0 40px")),
      
      eatWidget::range_chart(
        est = prior,
        est_min = q90,
        est_max = q95,
        global_est = value,
        global_est_min = value,
        global_est_max = q90,
        width = 350,
        height = 20,
        min = 0,
        max = 20 * 60,
        color_line = "#bae6fd",
        global_color_line = "#0ea5e9",
        global_color = "#0ea5e9",
        global_fill = "#0ea5e9",
        fill = "#e2e8f0"
      )
    )
  }
}