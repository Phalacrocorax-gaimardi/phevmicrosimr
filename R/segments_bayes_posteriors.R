#' naive bayes posterior agent car segment probabilities
#' 
#' car segment choice probabilites for each agent based on "common sense" liklihoods for 5 characteristics:
#' see ...
#' 
#' @format A data frame with 924 rows and 15 variables:
#' \describe{
#'   \item{ID}{agent ID}
#'   \item{A}{A segment ownership probability}
#'   \item{B}{B segment ownership probability}
#'   \item{C}{B segment ownership probability}
#'   \item{D}{B segment ownership probability}
#'   \item{E}{B segment ownership probability}
#'   \item{F}{B segment ownership probability}
#'   \item{S}{B segment ownership probability}
#'   \item{B-J}{B segment ownership probability}
#'   \item{C-J}{B segment ownership probability}
#'   \item{D-J}{B segment ownership probability}
#'   \item{B-M}{B segment ownership probability}
#'   \item{C-M}{B segment ownership probability}
#'   \item{D-M}{B segment ownership probability}
#'   \item{E-M}{B segment ownership probability}
#' }
#' @source \url{Data created from priors and likelihoods in ~/Policy/AgentBaseModels/PHEVs/survey_fleet.xlxs}
"segments_bayes_posteriors"
