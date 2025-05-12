test_that("bold_headers applies style to column headers", {
  tbl <- gt::gt(head(mtcars)) %>%
    tab_bold_headers()

  # Extract header style
  header_styles <- tbl$`_styles`
  header_styles <- header_styles$styles[
    header_styles$locname == "columns_columns"
  ]

  # Isolate the text font weight
  weight <- lapply(tbl$`_styles`$styles, function(x) x$cell_text$weight) |>
    unlist()

  # All columns should be bold
  expect_equal(
    weight,
    rep("bold", ncol(mtcars))
  )
})

test_that(
  "tab_bold_headers applies bold style to column and spanning headers",
  {
    tbl <- gt::gt(head(mtcars)) %>%
      gt::tab_spanner(label = "Engine specs", columns = c("cyl", "disp")) %>%
      tab_bold_headers(spanners = TRUE)

    # Extract styles data frame
    styles_df <- tbl$`_styles`

    # Extract weights for column headers
    col_weights <- styles_df$styles[styles_df$locname == "columns_columns"] |>
      lapply(function(x) x$cell_text$weight) |>
      unlist()

    # Assert all column headers are bold
    expect_equal(
      col_weights,
      rep("bold", ncol(mtcars))
    )

    # Extract weights for spanning headers
    spanner_weights <- styles_df$styles[
      styles_df$locname == "columns_groups"
    ] |>
      lapply(function(x) x$cell_text$weight) |>
      unlist()

    # Assert all spanners (1 in this case) are bold
    expect_equal(
      spanner_weights,
      rep("bold", length(spanner_weights))
    )
  }
)
