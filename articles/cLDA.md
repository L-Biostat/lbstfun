# Constrained Longitudinal Data Analysis (cLDA) in R

``` r
# Packages used in the analysis
library(nlme) # GLS + correlation / variance structures
library(marginaleffects) # avg_predictions(), hypotheses()
# library(ggplot2)         # plotting
library(tinyplot)
```

``` r
# Read the example data from the package
data(priority, package = "lbstfun")
```

## Custom design matrix

To fit a cLDA model, we need to create a custom design matrix that
includes the time variable and its interaction with the treatment group.
It should not contain the intercept, nor the interaction with the first
level of time (baseline). We also relabel the columns to avoid issues
with special characters (e.g., `:`).

``` r
# Create the design matrix (no ME for group)
design_matrix_full <- model.matrix(~ time + time:trt, data = priority)

# Remove the intercept and the interaction with the first level of time (baseline)
col_names <- colnames(design_matrix_full)
selected_cols <- !(col_names %in% c("(Intercept)", "time0:trt2"))
design_matrix <- design_matrix_full[, selected_cols]

# Relabel the columns to avoid issues with special characters (:)
relabelled_colnames <- gsub(":", "_", col_names[selected_cols])
colnames(design_matrix) <- relabelled_colnames

# Combine the design matrix with the outcome and id
model_data <- cbind(
  priority,
  as.data.frame(design_matrix)
)

# View the first rows of the custom design matrix
head(design_matrix)
#>   time1 time2 time1_trt2 time2_trt2
#> 1     0     0          0          0
#> 2     1     0          1          0
#> 3     0     1          0          1
#> 4     0     0          0          0
#> 5     1     0          1          0
#> 6     0     1          0          1
```

## Fit the model

Since the design matrix was created manually, we need to specify all the
components of the model ourselves in the formula. We also specify the
variance structure (one variance per time point) and the correlation
structure (unstructured correlation within subjects).

``` r
# Create the formula for the model
form <- formula(paste(
  "outcome ~",
  paste(colnames(design_matrix), collapse = " + ")
))

# Fit the constrained LDA model
fit <- gls(
  form,
  data = model_data,
  weights = varIdent(form = ~ 1 | time), # One variance per time point
  correlation = corSymm(form = ~ 1 | id) # Unstructured correlation within subjects
)

coef(summary(fit))
#>                  Value Std.Error   t-value       p-value
#> (Intercept) 1694.93708  35.70774 47.466932 3.924868e-235
#> time1        109.28083  18.64782  5.860247  6.746131e-09
#> time2         95.69335  16.87677  5.670123  1.991302e-08
#> time1_trt2   -52.60079  26.61114 -1.976645  4.842392e-02
#> time2_trt2   -55.76834  23.89942 -2.333460  1.987030e-02
```

## Estimated marginal means and contrasts

We first compute the predicted values at each time point for each
treatment group and plot them.

``` r
# Obtain average predictions at each time point for each treatment group
p <- avg_predictions(fit, by = c("time", "trt"))
p
#> 
#>  time trt Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
#>     0   1     1695       35.7 47.5   <0.001 Inf  1625   1765
#>     0   2     1695       35.7 47.5   <0.001 Inf  1625   1765
#>     1   1     1804       40.4 44.7   <0.001 Inf  1725   1883
#>     1   2     1752       40.5 43.2   <0.001 Inf  1672   1831
#>     2   1     1791       38.8 46.1   <0.001 Inf  1715   1867
#>     2   2     1735       38.8 44.7   <0.001 Inf  1659   1811
#> 
#> Type: response
```

``` r
# Plot these predictions
plt(
  estimate ~ time | trt,
  data = p,
  ymin = conf.low,
  ymax = conf.high,
  type = type_lines(dodge = 0.05),
  col = c("#0050ef", "#e51400"),
  legend = list(title = "Treatement"),
  xlab = "Time",
  ylab = "Estimate"
)
plt_add(type = type_errorbar(dodge = 0.05))
```

![](cLDA_files/figure-html/unnamed-chunk-6-1.png)

To test specific hypotheses, we can use the
[`hypotheses()`](https://marginaleffects.com/man/r/hypotheses.html)
function. We then specify which linear combination of the predicted
values in `p` we want to test. For example, `"b5 - b1"` tests the
difference between row 5 and row 1 of the predicted values (i.e., time 2
vs time 0 for treatment group 1).

``` r
# Compute the difference between time 2 and time 0 for each treatment group
hypotheses(p, hypothesis = "b5 - b1 = 0") # For the reference group (trt1)
#> 
#>  Hypothesis Estimate Std. Error    z Pr(>|z|)    S 2.5 % 97.5 %
#>     b5-b1=0     95.7       16.9 5.67   <0.001 26.1  62.6    129
hypotheses(p, hypothesis = "b6 - b2 = 0") # For the other group (trt2)
#> 
#>  Hypothesis Estimate Std. Error    z Pr(>|z|)   S 2.5 % 97.5 %
#>     b6-b2=0     39.9         17 2.35   0.0185 5.8  6.69   73.2
```

We can also test if the change over time is different between the two
treatment groups (i.e., difference in differences).

``` r
# Compute the difference in differences between groups at time 2 vs time 0
hypotheses(p, hypothesis = "(b6 - b2) = (b5 - b1)")
#> 
#>       Hypothesis Estimate Std. Error     z Pr(>|z|)   S 2.5 % 97.5 %
#>  (b6-b2)=(b5-b1)    -55.8       23.9 -2.33   0.0196 5.7  -103  -8.93
```

## Covariance matrix

``` r
# Extract correlation matrix (1 per subject )
cor_matrix <- corMatrix(fit$modelStruct$corStruct, corr = TRUE)

# Extract variance weights (relative to base level)
var_weights <- coef(
  fit$modelStruct$varStruct,
  unconstrained = FALSE,
  allCoef = TRUE
)

# Get base residual variance
base_var <- fit$sigma^2

# Compute actual variances per time level
var <- base_var * var_weights^2
sd <- sqrt(var)

# Build covariance matrices for first subject
cor <- cor_matrix[[1]]
cov <- diag(sd) %*% cor %*% diag(sd)

cov
#>          [,1]     [,2]     [,3]
#> [1,] 397825.1 399325.0 389564.9
#> [2,] 399325.0 446405.1 417442.6
#> [3,] 389564.9 417442.6 417117.7
```
