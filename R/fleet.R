#' 2021 passenger car models
#'
#' 2021 new passenger cars listed in Ireland with technology costs. 
#' The order follow CSO car model sales data 2014-2019 except where new models have been introduced.
#' The lowest cost trim is used except in cases where vehicle type is available in a more expensive trim only.
#' In some cases this comparison is not possible 
#' e.g mini cooper BEV has in basic trim while PHEV version exists as premium "Countryman" trim only.
#' Prices are from 2021 price list where possible, however e.g. Audi and BMW prices are adjusted 2020 prices.
#'
#' 
#' @format A data frame with 244 rows and 16 variables:
#' \describe{
#'   \item{make}{manufacturer}
#'   \item{model}{model}
#'   \item{segment}{car market segment A,B,C,D,E,F, crossovers (-J) and multi-purpose vehicles (-M). Sports cars are S.}
#'   \item{type}{powertrain or fuel type}
#'   \item{2021_rrp}{price excluding subsidies}
#'   \item{tech_cost_2021}{retail price before incentives (including VAT and VRT)}
#'   \item{WLTP}{combined WLTP emissions}
#'   \item{vrt}{vrt rate for 2021 based on WLTP}
#'   \item{motor}{road tax (euros)}
#'   \item{kWh}{Li-B capacity. Mild HEVs are assigned a value 0.4, HEVs are assigned 1 all others from manufacturer}
#'   \item{2021_rrp*}{list price (bevs and phevs only) including incentives}
#'   \item{comment}{additional info}
#'   \item{kWh/100km}{conversion efficiency if quoted}
#'   \item{AER}{quoted electric ranges}
#'   \item{model_start}{introduction model year}
#'   \item{model_end}{last model year if applicable}
#' }
#' @source \url{Multiples sources primarilty manufacturer price lists. Also https://www.rte.ie/brainstorm/2020/1215/1184581-electric-cars-buyers-guide-2021-ireland/}
"fleet"
