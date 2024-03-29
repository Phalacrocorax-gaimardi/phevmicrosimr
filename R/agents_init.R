#' agents_init
#'
#' new vehicle models purchased 2014-2019
#'
#' @format A data frame with 225 rows and 5 variables:
#' \describe{
#'   \item{ID}{ID}
#'   \item{w_econ}{financial weight}
#'   \item{w_social}{social weight}
#'   \item{w_enviro}{environmental weight}
#'   \item{w_distance}{distance weight (not used)}
#'   \item{w_theta}{theta weight}
#'   \item{qev32}{budget}
#'   \item{qev29}{budget}
#'   \item{q17a_2}{environmental concern 1 = low 5=high}
#'   \item{qev34}{kilometerage}
#'   \item{qev31}{new or used (3=dont know)}
#'   \item{type0}{initial fuel type, assumed to either be diesel or petrol based on adapted survey reponses}
#'   \item{reg}{registration year of vehicle (based on 2018 survey data - 3 years)}
#'   
#' }
#' @source <survey>
"agents_init"
