#' 2022 passenger car models
#'
#' 2022 new passenger cars listed in Ireland with technology costs. Data from dealer websites. This dataset is used to construct future new car fleets.
#'
#' 
#' @format A data frame with 345 rows and 15 variables:
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
#' }
#' @source <Multiples sources primarilty manufacturer price lists. Also https://www.rte.ie/brainstorm/2020/1215/1184581-electric-cars-buyers-guide-2021-ireland/>. nox data rom revenue.ie.
"fleet_2022"
