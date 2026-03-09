#' Example data set for cLDA
#'
#' An example dataset coming from a study called PRIORITY (hence the name) to
#' illustrate how to fit and analyze results from a constrained longitudinal data
#' analysis (cLDA) in R.
#'
#' @format `priority`
#' A data frame with 807 rows and 4 columns:
#' \describe{
#'   \item{id}{Patient id}
#'   \item{trt}{Treatement group, 1 or 2 for placebo or treatement}
#'   \item{time}{Time points of the study where data was collected (0, 1, or 2)}
#'   \item{outcome}{The outcome to analyze}
#' }
"priority"
