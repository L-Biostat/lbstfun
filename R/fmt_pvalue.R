#' Format p-values in a gt table
#'
#' `fmt_pvalue()` formats numeric columns containing p-values in a [`gt::gt`]
#' table. It uses [`scales::label_pvalue()`] to control formatting aspects like
#' decimal accuracy, prefix, or whether to add "p=" automatically.
#'
#' @inheritParams gt::fmt_chem
#' @inheritParams scales::label_pvalue
#'
#' @return A `gt_tbl` object with formatted p-values.
#'
#' @examples
#' library(gt)
#' library(dplyr)
#'
#' tibble(
#'   variable = c("A", "B", "C"),
#'   p_value = c(0.034, 0.0004, 0.56)
#' ) |>
#'   gt() |>
#'   fmt_pvalue(columns = "p_value")
#'
#' @export
fmt_pvalue <- function(
  data,
  columns = gt::everything(),
  rows = gt::everything(),
  accuracy = 0.001,
  prefix = NULL,
  add_p = FALSE
) {
  rows <- rlang::enquo(rows)
  gt::fmt(
    data,
    columns = columns,
    rows = !!rows,
    fns = scales::label_pvalue(
      accuracy = accuracy,
      prefix = prefix,
      add_p = add_p
    )
  )
}
