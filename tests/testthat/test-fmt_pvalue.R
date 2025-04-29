test_that("fmt_pvalue formats p-values correctly", {
  data <- data.frame(
    variable = c("A", "B", "C"),
    p_value = c(0.034, 0.0004, 0.56)
  ) |>
    gt::gt()

  # Apply formatting
  formatted_tbl <- fmt_pvalue(data, columns = "p_value")

  # Extract formatted values
  rendered <- gt::extract_body(formatted_tbl)$p_value

  expect_true(all(c("0.034", "<0.001", "0.560") %in% rendered))
})

test_that("fmt_pvalue can add prefix correctly", {
  data <- data.frame(
    p_value = c(0.034)
  ) |>
    gt::gt()

  formatted_tbl <- fmt_pvalue(data, columns = "p_value", add_p = TRUE)

  rendered <- gt::extract_body(formatted_tbl)$p_value

  expect_true(any(grepl("^p=", rendered)))
})

test_that("fmt_pvalue handles custom accuracy", {
  data <- data.frame(
    p_value = c(0.03456)
  ) |>
    gt::gt()

  formatted_tbl <- fmt_pvalue(data, columns = "p_value", accuracy = 0.0001)

  rendered <- gt::extract_body(formatted_tbl)$p_value

  expect_true(rendered == "0.0346")
})
