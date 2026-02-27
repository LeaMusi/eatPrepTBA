#' Uses JavaScript code to sort the rows in the table
#' 
#' Author: Philipp Franikowski, restructuring by Lea Musiolek
#' 
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Used in layout_staytime_tables.R
#'
#' @export

sort_function <- htmlwidgets::JS("function(rowInfo, column, state) {
  const {id} = column;
  const firstSorted = state.sorted[0]
  const validIds = ['domain', 'unit_key', 'unit_median', 'unit_label'];
  if (!firstSorted || validIds.includes(firstSorted.id)) {
    const prevRow = state.pageRows[rowInfo.viewIndex - 1]
    if (prevRow && rowInfo.values[id] === prevRow[id]) {
      return { visibility: 'hidden' }
    }
  }
}")