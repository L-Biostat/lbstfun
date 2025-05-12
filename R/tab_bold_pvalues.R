#' Bold significant p-values in a gt table
#'
#' `tab_bold_pvalues()` sets significant p-values as bold in a [`gt::gt`] table.
#' It is useful for highlighting statistically significant results in formatted
#' regression or summary tables.
#'
#' @param data A `gt_tbl` object.
#' @param column The unquoted name of the column containing the p-values. Can
#'   only be a single column.
#' @param threshold A numeric value indicating the p-value threshold for
#'   significance. Defaults to 0.05.
#'
#' @return A `gt_tbl` object.
#'
#' @examples
#' library(gt)
#'
#' df <- data.frame(
#'   Term = c("Intercept", "Treatment A", "Treatment B"),
#'   Estimate = c(2.3, -1.1, 0.5),
#'   p.value = c(0.001, 0.06, 0.03)
#' )
#'
#' df |>
#'   gt() |>
#'   tab_bold_pvalues(p.value)
#'
#' # Sometimes, a stricter threshold is needed
#' df |>
#'   gt() |>
#'   tab_bold_pvalues(column = p.value, threshold = 0.025)
#'
#' @export
tab_bold_pvalues <- function(data, column, threshold = 0.05) {
  gt::tab_style(
    data,
    locations = gt::cells_body(
      columns = {{column}},
      rows = {{column}} <= threshold
    ),
    style = list(
      gt::cell_text(weight = "bold")
    )
  )
}
