#' Format columns in the tables produced by layout_staytime_tables.R 
#' 
#' Author: Philipp Franikowski, restructuring by Lea Musiolek
#'
#' @param data Dataset being layouted
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Used in layout_staytime_tables.R
#'
#' @export

colUnit <- function(data) {
  list(
    link = reactable::colDef(
      name = "Links",
      width = 100,
      filterable = FALSE,
      sortable = FALSE,
      cell = shiny::icon("box-archive", lib = "font-awesome")
    ),
    unit_key = reactable::colDef(
      name = "Kurzname",
      style = sort_function,
      cell = function(value) htmltools::tags$code(value)
    ),
    unit_label = reactable::colDef(
      name = "Aufgabenbezeichnung",
      width = 350,
      style = sort_function
    ),
    unit_estimated = reactable::colDef(
      name = "a-priori",
      cell = to_stamp(value),
      style = sort_function
    ),
    unit_diff = reactable::colDef(
      name = "Differenz Q90",
      cell = to_stamp(value),
      style = sort_function
    ),
    unit_diff95 = reactable::colDef(
      name = "Differenz Q95",
      cell = to_stamp(value),
      style = sort_function
    ),
    
    # Globale Werte
    unit_median = reactable::colDef(
      name = "Median",
      cell = display_dotplot(data),
      style = sort_function,
      width = 400
    ),
    unit_q90 = reactable::colDef(
      name = "Q90",
      cell = to_stamp(value),
      style = sort_function
    ),
    unit_q95 = reactable::colDef(
      name = "Q95",
      cell = to_stamp(value),
      style = sort_function
    ),
    
    # Regelschulwerte
    unit_diff_RS = reactable::colDef(
      name = "Differenz Q90",
      cell = to_stamp(value),
      show = FALSE,
      style = sort_function
    ),
    unit_diff95_RS = reactable::colDef(
      name = "Differenz Q95",
      cell = to_stamp(value),
      show = FALSE,
      style = sort_function
    ),
    unit_median_RS = reactable::colDef(
      name = "Median",
      cell = display_dotplot(data, design = "RS"),
      style = sort_function,
      show = FALSE,
      width = 400
    ),
    unit_q90_RS = reactable::colDef(
      name = "Q90",
      cell = to_stamp(value),
      show = FALSE,
      style = sort_function
    ),
    unit_q95_RS = reactable::colDef(
      name = "Q95",
      cell = to_stamp(value),
      show = FALSE,
      style = sort_function
    ),
    
    # Förderschulwerte
    unit_diff_FS = reactable::colDef(
      name = "Differenz Q90",
      cell = to_stamp(value),
      show = FALSE,
      style = sort_function
    ),
    unit_diff95_FS = reactable::colDef(
      name = "Differenz Q95",
      cell = to_stamp(value),
      show = FALSE,
      style = sort_function
    ),
    unit_median_FS = reactable::colDef(
      name = "Median",
      cell = display_dotplot(data, design = "FS"),
      style = sort_function,
      show = FALSE,
      width = 400
    ),
    unit_q90_FS = reactable::colDef(
      name = "Q90",
      cell = to_stamp(value),
      show = FALSE,
      style = sort_function
    ),
    unit_q95_FS = reactable::colDef(
      name = "Q95",
      cell = to_stamp(value),
      show = FALSE,
      style = sort_function
    )
  )
}