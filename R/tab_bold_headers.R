#' Bold column and/or spanning headers in a gt table
#'
#' This function applies bold styling to the column headers in a `gt` table, and
#' optionally also to spanning headers.
#'
#' @param data A `gt_tbl` object.
#' @param spanners Should spanning headers also be bold?
#'
#' @return A `gt_tbl` object with bold headers.
#'
#' @examples
#' library(gt)
#'
#' gt(head(iris)) |>
#'   tab_bold_headers()
#'
#' @export
tab_bold_headers <- function(data, spanners = TRUE) {
  out <- gt::tab_style(
    data,
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels(everything())
  )

  if (spanners) {
    out <- gt::tab_style(
      out,
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_spanners()
    )
  }

  out
}
