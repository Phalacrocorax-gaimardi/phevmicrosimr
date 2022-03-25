#' 2021 passenger car models
#'
#' New passenger cars listed in Ireland in 2021 with costs. 
#' The order follow CSO car model sales data 2014-2019 except where new models have been introduced.
#' The lowest cost trim is used except in cases where zev is available in a more expensive trim only. In this case a comparable icev trim is used.
#' In some cases this comparison is not possible 
#' e.g mini cooper BEV has in basic trim while PHEV version exists as premium "Countryman" trim only.
#' Prices are from 2021 price list where possible, however e.g. Audi and BMW prices were adjusted 2020 prices.
#'
#' 
#' @format A data frame with 244 rows and 17 variables:
#' \describe{
#'   \item{make}{manufacturer}
#'   \item{model}{model}
#'   \item{segment}{car market segment A,B,C,D,E,F, crossovers (-J) and multi-purpose vehicles (-M). Sports cars are S.}
#'   \item{type}{powertrain or fuel type}
#'   \item{p_1}{price excluding subsidies but including VRT and tax}
#'   \item{p_0}{retail price before incentives or taxes}
#'   \item{p_2}{retail price including subsidies and taxes}
#'   \item{wltp}{combined WLTP emissions in g/km}
#'   \item{vrt}{vrt rate for 2021 based on WLTP}
#'   \item{motor}{road tax (euros)}
#'   \item{kWh}{Li-B capacity. Mild HEVs are assigned a value 0.4, HEVs are assigned 1 all others from manufacturer}
#'   \item{AER}{quoted electric range (wltp)}
#'   \item{nox}{nox emissions in mg/km}
#'   \item{nox_flag}{"data" implies is based on certificates of conformity}
#'   \item{comment}{additional info}
#'   \item{model_start}{introduction model year}
#'   \item{model_end}{last model year if applicable}
#' }
#' @source <Multiples sources primarilty manufacturer price lists. Also https://www.rte.ie/brainstorm/2020/1215/1184581-electric-cars-buyers-guide-2021-ireland/>. nox data rom revenue.ie.
"fleet"
