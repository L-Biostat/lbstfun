# Bold significant p-values in a gt table

\`tab_bold_pvalues()\` sets significant p-values as bold in a
\[\`gt::gt\`\] table. It is useful for highlighting statistically
significant results in formatted regression or summary tables.

## Usage

``` r
tab_bold_pvalues(data, column, threshold = 0.05)
```

## Arguments

- data:

  A \`gt_tbl\` object.

- column:

  The unquoted name of the column containing the p-values. Can only be a
  single column.

- threshold:

  A numeric value indicating the p-value threshold for significance.
  Defaults to 0.05.

## Value

A \`gt_tbl\` object.

## Examples

``` r
library(gt)

df <- data.frame(
  Term = c("Intercept", "Treatment A", "Treatment B"),
  Estimate = c(2.3, -1.1, 0.5),
  p.value = c(0.001, 0.06, 0.03)
)

df |>
  gt() |>
  tab_bold_pvalues(p.value)


  

Term
```

Estimate

p.value

Intercept

2.3

0.001

Treatment A

-1.1

0.060

Treatment B

0.5

0.030

\# Sometimes, a stricter threshold is needed df \|\>
[gt](https://gt.rstudio.com/reference/gt.html)() \|\>
tab_bold_pvalues(column = p.value, threshold = 0.025)

| Term        | Estimate | p.value |
|:------------|---------:|--------:|
| Intercept   |      2.3 |   0.001 |
| Treatment A |     -1.1 |   0.060 |
| Treatment B |      0.5 |   0.030 |
