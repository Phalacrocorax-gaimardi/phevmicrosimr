#' ICEV specific emissions
#'
#' Statistical Model for specific ICEV emissions depending on mileage. In 2015 fleet mean NEDC emissions sugested by motor tax data is 150gCO2/km.
#'
#' @param mileage real
#' @param e_upper real
#' @param e_lower real
#'
#' @return real
#' @export
#'
#' @examples
emissions_model <- function(mileage, e_upper = 180, e_lower = 80) {
  # assume a statistical relationship between mileage and gCO2/km (higher kilometerage => larger engine)
  # e_upper = 200 e_lower=80 is supposed to represent 2015
  # mean road tax paid in 2020 ~ 400
  # lower bound on specific emissions for ICEV/hybrid is 80
  return(wltp_nedc_ratio*max(80, ((e_upper + (e_lower - e_upper) * exp(-mileage / 10000) + 25 * rnorm(1)))))
}



#emissions_model <- Vectorize(emissions_model)


#' icevfleet_fun
#'
#' ICEV fleet upper emissions parameter
#'
#' @param sD scenario dataframe
#' @param yeartime real
#'
#' @return real
#' @export
#'
#' @examples
icev_fleet_fun <- function(sD,yeartime){

  e_mean_final <- dplyr::filter(sD,parameter=="e_mean_final")$value
  end_development_year <- dplyr::filter(sD,parameter=="end_development_year")$value
  #estimates of e_upper in 2015(210) and 2019 (190) from motor tax receipts
  return(stats::approx(x=c(year_zero+0.5,2019.5,end_development_year),y=c(185,175,e_mean_final),xout=yeartime,rule=2)$y)
}


#' icev_fleet_lowercut_fun
#'
#' lower bound on ICEV emission (i.e. non-plug-in hyrbids)
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time in years
#'
#' @return value of e_cut used in emissions_model1
#' @export
#'
#' @examples
icev_fleet_lowercut_fun <- function(sD,yeartime){

  e_cut_final <- dplyr::filter(sD,parameter=="e_cut_final")$value
  end_development_year <- dplyr::filter(sD,parameter=="end_development_year")$value
  #estimates of e_upper in 2015(210) and 2019 (190) from motor tax receipts
  return(stats::approx(x=c(year_zero+0.5,2019.5,end_development_year),y=c(80,80,e_cut_final),xout=yeartime,rule=2)$y)
}



