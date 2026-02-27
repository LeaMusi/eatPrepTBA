#' Use the generated JavaScript in the onchange attribute
#' 
#' Author: Philipp Franikowski, restructuring by Lea Musiolek
#'
#' @param label Label for checkbox
#' @param checked Should it be pre-checked?
#' @param id Which table
#' @param columns unclear
#' @param filter_column unclear
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Prepares checkbox for rendered tables.
#' Used in layout_staytime_tables.R
#'
#' @export

generate_checkbox <- function(label, checked = NULL, id = "item-table", columns, filter_column = NULL) {
  filter_code <- ""
  if (!is.null(filter_column)) {
    filter_code <- glue::glue("
    if (!show) {{
      Reactable.setAllFilters(id, {jsonlite::toJSON(filter_column, auto_unbox = TRUE)});
    }}else {{
      Reactable.setAllFilters(id, []);
    }}")
  }
  
  js_code <- glue::glue("((e) => {{
    const show = !e.target.checked;
    const id = '{id}';
    console.log(Reactable.getState(id));
    const cols = {jsonlite::toJSON(columns, auto_unbox = TRUE)};
    cols.map(col => Reactable.toggleHideColumn(id, col, show));
    {filter_code}
    }})(event)")
  
  tags$input(
    label,
    type = "checkbox",
    checked = if (is.null(checked) || !checked) NULL else checked,
    onChange = htmltools::HTML(js_code)
  )
}