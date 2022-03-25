#' Empirical utilites from Survey Model
#'
#' SHAP values for environmental, social, budget and mileage
#'
#' @format A data frame with 25 rows and 4 variables:
#' \describe{
#'   \item{name}{survey question codes: q17a_2 = environmental concern, qev29 = number of EV owners known}
#'   \item{answercode}{feature value}
#'   \item{answer}{answer}
#'   \item{shap}{SHAP contribution to utility prediction}
#' }
#' @source <Survey Model>
"util_empirical"
