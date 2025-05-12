test_that("tab_bold_pvalues() correctly bolds significant p-values", {
  pval <-  c(0.001, 0.06, 0.03)
  tbl <- data.frame(
    Term = c("Intercept", "Treatment A", "Treatment B"),
    Estimate = c(2.3, -1.1, 0.5),
    p.value = pval
  ) |>
    gt::gt() |>
    tab_bold_pvalues(p.value)

  # Extract row number with bold style
  styled_cells <- tbl$`_styles`
  styled_cells$weight <- lapply(
    styled_cells$styles,
    function(x) x$cell_text$weight
  )
  bold_rows <- styled_cells[styled_cells$weight == "bold", "rownum"]

  # All columns should be bold
  expect_equal(
    bold_rows,
    which(pval < 0.05)
  )
})
