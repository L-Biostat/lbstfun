# Bold column and/or spanning headers in a gt table

This function applies bold styling to the column headers in a \`gt\`
table, and optionally also to spanning headers.

## Usage

``` r
tab_bold_headers(data, spanners = TRUE)
```

## Arguments

- data:

  A \`gt_tbl\` object.

- spanners:

  Should spanning headers also be bold?

## Value

A \`gt_tbl\` object with bold headers.

## Examples

``` r
library(gt)

gt(head(iris)) |>
  tab_bold_headers()


  

Sepal.Length
```

Sepal.Width

Petal.Length

Petal.Width

Species

5.1

3.5

1.4

0.2

setosa

4.9

3.0

1.4

0.2

setosa

4.7

3.2

1.3

0.2

setosa

4.6

3.1

1.5

0.2

setosa

5.0

3.6

1.4

0.2

setosa

5.4

3.9

1.7

0.4

setosa
