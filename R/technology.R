#scenario_1 <- readxl::read_xlsx("~/Policy/AgentBasedModels/PHEVs/scenarioDesignPHEV.xlsx",sheet=2)
#scenario_2 <- readxl::read_xlsx("~/Policy/AgentBasedModels/PHEVs/scenarioDesignPHEV.xlsx",sheet=3)
#scenario_3 <- readxl::read_xlsx("~/Policy/AgentBasedModels/PHEVs/scenarioDesignPHEV.xlsx",sheet=4)
#sD <- readxl::read_xlsx("~/Policy/AgentBasedModels/PHEVs/scenarioDesignPHEV.xlsx",sheet="scenario_B")
#use_data(scenario_B,overwrite=T)
#use_data(scenario_2,overwrite=T)
#use_data(scenario_1,overwrite=T)
#fleet <- read_csv("~/Policy/AgentBasedModels/PHEVs/NOx/fleet_2021_nox_calculated.csv")
#fleet_2022 <- read_csv("~/Policy/AgentBasedModels/PHEVs/NOx/fleet_2022_nox_calculated.csv")
#bevs <- filter(fleet,type=="bev")
#use_data(fleet,overwrite = T)
#use_data(fleet_2022,overwrite=T)

#get_fleet_data <- function(fleet_dirty){
  
#  fleet <- fleet %>% dplyr::filter(!is.na(`2021_rrp`))
#  fleet$kWh <- as.numeric(fleet$kWh)
#  fleet <- fleet %>% dplyr::mutate(kWh=ifelse(!is.na(kWh),kWh,0))
#  fleet <- fleet %>% dplyr::mutate(model_start=replace(model_start,is.na(model_start),1990))
#  fleet <- fleet %>% dplyr::mutate(model_end= replace(model_end,is.na(model_end),3000))
#  ifelse(any(is.na(fleet$tech_cost_2021)),print("bad input"),print("good input"))
#  return(fleet)
#}

#internal parameters
#use_data(p.,beta.,wltp_nedc_ratio,lambda.,theta.,year_zero,end_year,internal=T,overwrite=T)

#' emissions_model1
#'
#' gamma distribution of emissions above a cutoff corresponding to input mean emissions and standard deviation parameters. Dependence on mileage is modelled by a tanh function.
#'
#' @param mileage km
#' @param mean_emissions gco2/km - corresponds to wltm emissions  e.g. 190gCO2/km in 2015
#' @param sd_emissions gco2/km -
#' @param e_cut lower bound on ICEV emissions, roughly set to Toyota Prius wltp emissions
#' @param zeta determines strength of correlation between mileage and emissions
#'
#' @return a randomly generated specific emissions in gCO2/km
#' @export
#'
#' @examples
#' 
emissions_model1 <- function (mileage, mean_emissions=190, sd_emissions=30, e_cut=80,zeta=0.75) {
  #
  m_emissions <- mean_emissions + zeta*sd_emissions*tanh((mileage-10000)/20000)
  shape_param <- ((m_emissions-e_cut)/sd_emissions)^2
  theta <- (m_emissions-e_cut)/shape_param
  return(stats::rgamma(1,scale=theta,shape=shape_param)+e_cut)

}


#' Oil price scenario
#'
#' continuous time oil price (euro/bbl) based on monthly opec basket price (ORB) and monthly eurusd settlements from ecb
#' this depends on assumed oil prices at of 2030 and 2050 from input scenario
#'
#'
#' @param sD scenario dataframe
#' @param yeartime real
#'
#' @return real
#' @export
#'
#' @examples 
#' oil_price_fun(scenario_B,2023)
#' 
oil_price_fun <- function(sD,yeartime){

  crudeoilprice_2020 <- dplyr::filter(sD, parameter=="crudeoilprice_2020")$value
  crudeoilprice_2030 <- dplyr::filter(sD, parameter=="crudeoilprice_2030")$value
  crudeoilprice_2050 <- dplyr::filter(sD, parameter=="crudeoilprice_2050")$value

  oil <- stats::approx(c(lubridate::decimal_date(rev(fuelprices$date)),2031,2051),c(rev(fuelprices$opec_price/fuelprices$eur),crudeoilprice_2030,crudeoilprice_2050),xout=yeartime,rule=2)$y
  #oil <- stats::approx(c(2015:2019+0.5,2030,2050),c(dplyr::filter(opec_oilprices, year %in% 2015:2019)$price_euro,crudeoilprice_2030,crudeoilprice_2050),xout=yeartime,rule=2)$y
  return(oil)

}


#' crackspread_fun
#'
#' refining margin for diesel or gasoline
#' includes historical estimate from fuelprices
#' assumes diesel and gasoline spreads are the same
#'
#' @param type diesel or gasoline
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return euros/barrel
#' @export
#'
#' @examples
#' crackspread_fun("diesel",scenario_B,2025)
#' 
crackspread_fun <- function(type,sD,yeartime){
  #
  #if(!(type %in% c("diesel","gasoline"))) stop("bad fuel type")
  
  crackspread_2030 <- dplyr::case_when(type=="diesel"~ dplyr::filter(sD, parameter=="crackspread_diesel_2030")$value,
                               type=="gasoline"~ dplyr::filter(sD, parameter=="crackspread_gasoline_2030")$value,
                               type=="petrol"~ dplyr::filter(sD, parameter=="crackspread_gasoline_2030")$value)
  
  crackspread_2050 <- dplyr::case_when(type=="diesel"~ dplyr::filter(sD, parameter=="crackspread_diesel_2050")$value,
                                type=="gasoline"~ dplyr::filter(sD, parameter=="crackspread_gasoline_2050")$value,
                                type=="petrol"~ dplyr::filter(sD, parameter=="crackspread_gasoline_2050")$value)
  
  spread <- stats::approx(c(lubridate::decimal_date(rev(fuelprices$date)),2031,2051),c(rev(fuelprices$spread/fuelprices$eur),crackspread_2030,crackspread_2050),xout=yeartime,rule=2)$y
  return(spread)
  
}


#' diesel_price
#'
#' Retail fuel price formula. Does not distinguish between petrol and diesel.
#'
#' @param crudeoilprice euros/barrel
#' @param crackspread euros
#' @param margin euros
#' @param carbon_tax euro/tCO2
#' @param excise_duty euros
#' @param NORA levy euros
#' @param bio levy euros
#' @param VAT percent
#'
#' @return euros per litre
#' @export
#'
#' @examples
#' diesel_price(50,10,0.012,100,0.54,0.02,0.014,0.23)
#' 
diesel_price <- function(crudeoilprice=50,crackspread = 10, margin=0.12,carbon_tax=33.5, excise_duty=0.54, NORA=0.02, bio = 0.014, VAT=0.23){
   #euros per litre retail
  ((crudeoilprice+crackspread)/159+margin + excise_duty + carbon_tax/1000*2.640 + NORA)*(1+VAT)

}


#' fuel_price_fun
#'
#' Retail diesel or gasoline price from scenario
#'
#' @param type fuel type
#' @param crudeoilprice euros/barrel
#' @param crackspread euros
#' @param margin euros
#' @param carbon_tax euro/tCO2
#' @param excise_duty euros
#' @param NORA levy euros
#' @param bio levy euros
#' @param VAT percent
#'
#' @return euros per litre
#' @export
#'
#' @examples
#' 
fuel_price_fun <- function(type="diesel",crudeoilprice=50,crackspread = 10, margin=0.12,carbon_tax=33.5, excise_duty=0.54, NORA=0.02, bio = 0.014, VAT=0.23){
  #euros per litre retail
  if(type=="diesel")
  return(((crudeoilprice+crackspread)/159+margin + excise_duty + carbon_tax/1000*2.640 + NORA + bio)*(1+VAT))
  if(type=="gasoline" || type=="petrol")
  return(((crudeoilprice+crackspread)/159+margin + excise_duty + carbon_tax/1000*2.392 + NORA + bio)*(1+VAT))
  
}


#' fuel_price_fun1
#' 
#' cost of motor fuels at yeartime
#'
#' @param type fuel type ptrol or diesel
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return euros per litre
#' @export
#'
#' @examples
#' fuel_price_fun1("gasoline",scenario_B,2025)
#' 
fuel_price_fun1 <- function(type="diesel", sD, yeartime){
  
               crudeoilprice <- oil_price_fun(sD,yeartime)
               crackspread <- crackspread_fun(type=type,sD,yeartime)
               margin <- 0.12
               carbon_tax <- carbon_tax_fun(sD,yeartime)
               excise_duty <- 0.54
               NORA <- 0.02 
               bio <-  0.014
               VAT <- 0.23
               
               price <- dplyr::case_when(type=="diesel"~((crudeoilprice+crackspread)/159+margin + excise_duty_fun(type,sD,yeartime) + carbon_tax/1000*2.640 + NORA + bio)*(1+VAT),
                                         type=="gasoline"~((crudeoilprice+crackspread)/159+margin + excise_duty_fun(type,sD,yeartime) + carbon_tax/1000*2.392 + NORA + bio)*(1+VAT),
                                         type=="petrol"~((crudeoilprice+crackspread)/159+margin + excise_duty_fun(type,sD,yeartime) + carbon_tax/1000*2.392 + NORA + bio)*(1+VAT)
                                         )
               
               return(price)
}



#' bev_fuelcost
#' 
#' energy cost of bev in euros/km
#'
#' @param sD scenario dataframe
#' @param yeartime  real
#'
#' @return real euros per km
#' @export
#'
#' @examples
#' bev_fuelcost(scenario_B,2030)
#' 
bev_fuelcost <- function(sD,yeartime){

  electricity_price_fun(sD,yeartime)*conversion_fun(sD,yeartime)

}

#' conversion_fun
#'
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return value of interpolated conversion efficiency in kWh/km at yeartime
#' @export
#'
#' @examples
#' conversion_fun(scenario_B,2026.5)
#' 
conversion_fun <- function(sD,yeartime){
  
  conv_2020 <- dplyr::filter(sD,parameter=="conversion_eff_2020")$value
  conv_2030 <- dplyr::filter(sD,parameter=="conversion_eff_2030")$value
  conv_2040 <- dplyr::filter(sD,parameter=="conversion_eff_2040")$value
  
  conversion <- stats::approx(x=c(2020,2030,2040), y=c(conv_2020,conv_2030,conv_2040)/100,xout=yeartime,rule=2)$y
  return(conversion)
  
}

#' fuelcost_fun
#'
#' ICEV fuel cost in euros/km
#'
#' @param type diesel,petrol,hev,phev,bev
#' @param wltp emissions g/km
#' @param kWh battery capacity
#' @param AER all electric range km
#' @param params current value of scenario params from scenario_params()
#' @param xi charging behaviour
#'
#' @return real euros/km
#' @export
#'
#' @examples
#' 
fuelcost_fun <- function(type,wltp=0,kWh, AER, params,xi=1){
  #
  dplyr::recode(type,
   diesel = wltp*params$diesel/2640,
   petrol = wltp*params$gasoline/2392,
   gasoline = wltp*params$gasoline/2392,
   hev = wltp*params$gasoline/2392,
   bev = params$e_price*kWh/AER,
   phev = uf(AER)*params$e_price*kWh/AER + wltp*params$gasoline/2392)
}

#' BEV price premium (euros)
#' NOT USED
#'
#' @param sD scenario dataframe
#' @param yeartime real
#'
#' @return real
#' @export
#'
#' @examples
#' 
bev_premium_fun <- function(sD,yeartime){
  #BEV premium over gasoline vehicle
  premium_yearzero <- dplyr::filter(sD,parameter=="premium_yearzero")$value
  zero_premiumyear <- dplyr::filter(sD,parameter=="zero_premiumyear")$value
  premium_fall_year <- dplyr::filter(sD,parameter=="premium_fall_year")$value

  return(stats::approx(x=c(year_zero,premium_fall_year,zero_premiumyear),y=c(premium_yearzero,premium_yearzero,0),xout=yeartime,rule=2)$y)
}

#' Electricity price euros/kWh
#'
#' @param sD scenario dataframe
#' @param yeartime real
#'
#' @return real
#' @export
#'
#' @examples
#' electricity_price_fun(scenario_B,2029)
#' 
electricity_price_fun <- function(sD,yeartime){

  price_2020<-dplyr::filter(sD,parameter=="electricity_price_2020")$value
  price_2030<-dplyr::filter(sD,parameter=="electricity_price_2030")$value
  price_2050<-dplyr::filter(sD,parameter=="electricity_price_2050")$value
  price <- stats::approx(c(2020,2030,2050),c(price_2020,price_2030,price_2050),xout=yeartime,rule=2)$y
  return(price)

}

#' Range anxiety
#'
#' @param sD scenario dataframe
#' @param yeartime real
#'
#' @return real anxiety parameter for bevs. 1 is rational range anxiety, > 1 is irrational
#' @export
#'
#' @examples
#' anxiety_fun(scenario_B,2023)
anxiety_fun <- function(sD,yeartime){

  initial_range_anxiety <- dplyr::filter(sD,parameter=="initial_range_anxiety")$value
  anxiety_fall_year <- dplyr::filter(sD,parameter=="anxiety_fall_year")$value
  longterm_range_anxiety <- dplyr::filter(sD,parameter=="longterm_range_anxiety")$value
  longterm_range_anxiety_year <- dplyr::filter(sD,parameter=="longterm_range_anxiety_year")$value
  
  return(stats::approx(x=c(anxiety_fall_year,longterm_range_anxiety_year),y=c(initial_range_anxiety,longterm_range_anxiety),xout=yeartime,rule=2)$y)
}

#' u_anxiety
#' 
#' Range anxiety is assumed to reflect the probability that a daily trip exceeds the electric vehicle AER, requiring an intraday charge.
#' the distribution of daily trips is gamma distributed with shape parameter 2.5. subjectivity via "anxiety" parameter anxiety
#'
#' @param mileage annual mileage in km
#' @param AER range in km
#' @param shape gamma distribution 
#' @param anxiety range anxiety. 1 is non-anxious case, > 1 is anxious case
#' @param a_prefactor const of order 1
#'
#' @return probabilty that a daily trip exceeds AER
#' @export
#'
#' @examples
#' u_anxiety(20000,50,2.5,1.5)
u_anxiety <- function(mileage,AER,shape=2.5,anxiety,a_prefactor = 1){
  #probability that a trip exceeds AER
  mean_daily <- mileage/300 #assume vehicle is driven 300 days per year
  #adjust scale to reflect driver anxiety
  return(a_prefactor*(1-pgamma(AER,shape=shape,scale=anxiety*mean_daily/shape)))
}


#' tech_deflate
#' 
#' helper function describing for ev price factors pre-2021 
#'
#' @param rate price deflation rate
#' @param yeartime decimal time
#'
#' @return real value (1 in 2021)
#' @export
#'
#' @examples
tech_deflate <- function(rate,yeartime){
  
  ifelse(yeartime >= 2021,1,(1+ rate)^(2021-yeartime))
  
}
  

#' ICEV fleet emissions upper parameter
#'
#' @param sD scenario dataframe
#' @param yeartime real
#'
#' @return real
#' @export
#'
#' @examples
icevfleet_fun <- function(sD,yeartime){

  e_upper_yearzero<-dplyr::filter(sD,parameter=="e_upper_yearzero")$value
  end_development_year<- dplyr::filter(sD,parameter=="end_development_year")$value
  e_upper_final <- dplyr::filter(sD,parameter=="e_upper_final")$value
  return(stats::approx(x=c(year_zero,end_development_year),y=c(e_upper_yearzero,e_upper_final),xout=yeartime,rule=2)$y)
}

#' Battery cost function
#'
#' Learning curve model based on historical BNEF survey prices with a production lag and a learning curve
#'  i.e. early 2019 production models reflect mid 2016 BNEF battery prices
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time 2015-2050
#' @param nlag lag in years between bnef battery price and production price
#'
#' @return real (euros)
#' @export
#'
#' @examples
battery_cost_function <- function(sD,yeartime,nlag = 4.5){
  #battery pack price model tuned to Kona and Leaf prices
  #2020 premium
  alpha<- dplyr::filter(sD,parameter=="learning_exponent")$value
  b1 <-dplyr::filter(sD,parameter=="cost_floor")$value
  b0 <- dplyr::filter(bnefprices, year==2020)$pack_ekWh-b1
  #build learning curve from 
  if(yeartime >= 2020.5 + nlag) cost <- max(b1,b0*exp(-alpha*(yeartime-(2020.5+nlag))) + b1 ) 
  if(yeartime <= 2020.5 + nlag) cost <- stats::approx(x=bnefprices$year+0.5,y=bnefprices$pack_ekWh, xout=yeartime-nlag)$y
  #cap battery price as bat_max = 265
  #bat_max <- stats::approx(x=bnefprices$year+0.5,y=bnefprices$pack_ekWh, xout=2021-4.5)$y
  return(min(cost,265))
  #if(yeartime < 2021 & yeartime >= 2018) return( 264.7*(1+0.05)^(2021-yeartime))
  #if(yeartime < 2018) return( 264.7*(1+0.15)^(2021-yeartime))
  
}




#' tech_cost_fun
#' 
#' technology costs based on 2021 or 2022 fleets (pre-tax and incentive list prices). 
#' The cost model is based on battery pack price changes post 2022. 
#' Pre-2021 technology costs is based on deflation rate inputs
#'
#' @param type powertrain/fuel type
#' @param tech_cost 2021 technology cost
#' @param kWh battery
#' @param params scenario params e.g. scenario_B
#'
#' @return cost in euros
#' @export
#'
#' @examples
tech_cost_fun <- function(type,tech_cost,kWh=NA,params){
  
  dplyr::recode(type,
                
                diesel = tech_cost,
                petrol= tech_cost,
                hev = params$hev_deflate_factor*tech_cost,
                phev = ifelse(params$yeartime >=2021, tech_cost +kWh*(params$battery_cost- params$battery_cost_2021), params$phev_deflate_factor*tech_cost),
                #flat 2021 historic prices
                bev = ifelse(params$yeartime >= 2021, tech_cost + kWh*(params$battery_cost- params$battery_cost_2021), params$bev_deflate_factor*tech_cost)
  )
  
}



#' fleet_factor_fun
#' 
#' ICEV fleet emissions technology improvement based on % reduction in WLTP values.
#' This is applied to each ICEV model to project it forward and backwards in time.
#' Backward projection from 2021, forward from 2022 i.e. fleet_factor = 1 for 2021 and 2022
#' 
#' 
#'
#' @param sD  scenario
#' @param yeartime decimal time
#'
#' @return emissions relative to 2021 value
#' @export
#'
#' @examples
fleet_factor_fun <- function(sD,yeartime){
  
  func <- function(sD,yeartime){
    f1 <- dplyr::filter(sD,parameter=="fleet_factor_pre2008")$value
    f2 <- dplyr::filter(sD,parameter=="fleet_factor_2008_2015")$value
    f3 <- dplyr::filter(sD,parameter=="fleet_factor_2015_2021")$value
    f4 <- dplyr::filter(sD,parameter=="fleet_factor_2021")$value
    y_end <- dplyr::filter(sD,parameter=="end_development_year")$value
    #g1 <- 1+18*f1 #length(1990:2007)
    g1 <- (1+f1)^18
    #g2 <- g1*(1+7*f2)
    g2 <- g1*(1+f2)^7
    #g3 <- g2*(1+6*f3)
    g3 <- g2*(1+f3)^6
    #g4 <- g3*(1+(y_end-2020)*f4)
    g4 <- g3*(1+f4)^(y_end-2020)
    fact <- stats::approx(x=c(1990,2008,2015,2020,y_end),y=c(1,g1,g2,g3,g4),xout=yeartime,rule=2)$y
  return(fact)}
  ifelse(yeartime < 2022,return(func(sD,yeartime)/func(sD,2021)), return(func(sD,yeartime)/func(sD,2022)))
  
  
}


#' range_factor_fun
#'
#' BEV range function factor models efficiency improvements over time (i.e. km/kWh)
#'
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return unitless
#' @export
#'
#' @examples
range_factor_fun <- function(sD,yeartime){
  
  y_end <- dplyr::filter(sD,parameter=="bat_end_development_year")$value
  res <- 1 + (min(yeartime,y_end)-2021)*dplyr::filter(sD,parameter=="range_factor")$value
  return(res)
}



#' ev_depreciation_fun
#' 
#' spread between zevs & hev diesel/petrol
#'
#' @param type fuel/powertrain type 
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return percentage spread as fraction
#' @export
#'
#' @examples
depreciation_spread_fun <- function(type,sD,yeartime){
  
  spread_initial <- dplyr::recode(type,
                petrol=0,
                diesel= dplyr::filter(sD, parameter=="depreciation_diesel_spread")$value,
                hev = dplyr::filter(sD, parameter=="depreciation_hev_spread")$value,
                phev = dplyr::filter(sD, parameter=="depreciation_phev_spread")$value,
                bev = dplyr::filter(sD, parameter=="depreciation_bev_spread")$value
               )
  
  spread_fall_year <- dplyr::recode(type,
                                     petrol=2900,
                                     diesel= 2900,
                                     hev = dplyr::filter(sD, parameter=="hev_spread_fall_year")$value,
                                     phev = dplyr::filter(sD, parameter=="phev_spread_fall_year")$value,
                                     bev = dplyr::filter(sD, parameter=="bev_spread_fall_year")$value
  )
    
  spread_zero_year <- dplyr::recode(type,
                                    petrol=3000,
                                    diesel= 3000,
                                    hev = dplyr::filter(sD, parameter=="hev_spread_zero_year")$value,
                                    phev = dplyr::filter(sD, parameter=="phev_spread_zero_year")$value,
                                    bev = dplyr::filter(sD, parameter=="bev_spread_zero_year")$value
  )

  return(stats::approx(x=c(spread_fall_year,spread_zero_year), y=c(spread_initial,0),xout=yeartime,rule = 2)$y)
  
}


#' initialise_segments
#' 
#' extends agents_init with vehicle model & segment choice based on naive bayes classifier
#' A car model is the assigned by on segment choice, observed Irish make preferences 2014-2019 & 2021, and 2021 new fleet dataframe
#'
#'
#' @param sD scenario (needed for historical fleet parameters)
#'
#' @return initial agents dataframe
#' @export
#'
#' @examples
initialise_segments <- function(sD){
  #2015 car fleet
  choose_initial_car <- function(seg,fuel){
    #chooses a car from fleet weighted according to cso data initial_fleet
    #car segments and makes from cso 2014-2019
    init_car <- initial_fleet %>% dplyr::filter(segment==seg)
    #2021 fleet cars fossil only
    #fleet1 <- fleet %>% dplyr::filter(segment==seg, type %in% c("petrol","diesel"))
    fleet1 <- fleet %>% dplyr::filter(segment==seg, type == fuel)
    if(dim(fleet1)[1]==0) return("segment fuel cominbination not present in fleet")
    fleet1 <- fleet1 %>% dplyr::select(segment,make,model,type,wltp)
    init_car <- dplyr::filter(init_car, make %in% fleet1$make)
    init_car <- init_car %>% dplyr::group_by(make) %>% dplyr::summarise(n=sum(number))
    #choose car weighted by cso probabilities
    car_models <- dplyr::filter(fleet1,make==dplyr::slice_sample(init_car,n=1, weight_by = init_car$n)$make)
    #pick one if there are multiple options exist
    car_models <- car_models %>% dplyr::slice_sample(n=1)
    return(car_models)
  }
  
  #problem: too many diesels in B & B-J new priors 15/03/2022
  
  bay <- segments_bayes_posteriors %>% tidyr::pivot_longer(-ID,names_to="segment",values_to="prob")
  car_segments <- unique(bay$segment)
  init <- bay %>% dplyr::rowwise() %>% dplyr::group_by(ID) %>% dplyr::summarise(segment=sample(car_segments,1,prob=prob))
  agents_in1 <- dplyr::inner_join(agents_init,init,by="ID")
  agents_in1 <- dplyr::bind_cols(dplyr::select(agents_in1,-type0,-segment),dplyr::select(agents_in1,segment,type0) %>% dplyr::rowwise() %>% dplyr::do(choose_initial_car(.$segment,.$type0)))
  #assume new car drivers are driving 3 year old cars
  #used car drivers are driving 10 year old cars
  agents_in1 <- agents_in1 %>% dplyr::mutate(wltp = fleet_factor_fun(sD,reg)*wltp)
  return(agents_in1)
}


#'scenario_params_df
#'
#' techno-economic parameter dataframe at yeartime
#' parameters derived from a scenario at a decimal time between 2015 and 2050. Used to create hash object for fastlookup of current parameter values.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return object of class "environment" for fast lookup e.g. params$battery_cost
#' @export
#'
#' @examples
scenario_params_df <- function(sD,yeartime){
  #fast params
  scen <- tibble::tibble(parameter="yeartime", value=  yeartime)
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="battery_cost", value=  battery_cost_function(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="battery_cost_2021", value =  battery_cost_function(sD,2021)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bev_grantpath", value =  grant_fun1("bev",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="phev_grantpath", value =  grant_fun1("phev",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="green_tax", value =  carbon_tax_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="car_vat", value =  car_vat_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="dS", value =  dplyr::filter(sD, parameter=="add_measures")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="max_rebate", value =  dplyr::filter(sD, parameter=="max_rebate")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="rebate_threshold_lower", value =  vrt_rebate_lower(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="rebate_threshold_upper", value =  vrt_rebate_upper(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="fleet_factor", value =  fleet_factor_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="range_factor", value =   range_factor_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="vkt_reduction", value =   vkt_reduction_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="hev_deflate_factor", value =   tech_deflate(dplyr::filter(sD,parameter=="hev_deflate")$value,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="phev_deflate_factor", value =   tech_deflate(dplyr::filter(sD,parameter=="phev_deflate")$value,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bev_deflate_factor", value =   tech_deflate(dplyr::filter(sD,parameter=="bev_deflate")$value,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_0", value =   dplyr::filter(sD, parameter=="depreciation_0")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_diesel", value =   dplyr::filter(sD, parameter=="depreciation_0")$value + depreciation_spread_fun("diesel",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_hev", value =   dplyr::filter(sD, parameter=="depreciation_0")$value + depreciation_spread_fun("hev",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_phev", value =   dplyr::filter(sD, parameter=="depreciation_0")$value + depreciation_spread_fun("phev",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_bev", value =   dplyr::filter(sD, parameter=="depreciation_0")$value + depreciation_spread_fun("bev",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_battery", value = dplyr::filter(sD, parameter=="depreciation_battery")$value ))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="battery_degrade", value = dplyr::filter(sD, parameter=="battery_degrade")$value ))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="anxiety", value =   anxiety_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="anxiety_prefactor", value =   dplyr::filter(sD, parameter=="anxiety_prefactor")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="phev_aversion", value =   dplyr::filter(sD, parameter=="phev_aversion")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="brand_loyalty_new", value =   dplyr::filter(sD, parameter=="brand_loyalty_new")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="brand_loyalty_used", value =   dplyr::filter(sD, parameter=="brand_loyalty_used")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="tco_term", value =   dplyr::filter(sD, parameter=="term.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="oil", value =  oil_price_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="excise_diesel", value =  excise_duty_fun("diesel",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="excise_gasoline", value =  excise_duty_fun("gasoline",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="e_price", value =  electricity_price_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="conversion", value =  conversion_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ev_runcost", value =  bev_fuelcost(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="diesel", value=fuel_price_fun(type="diesel",
                                                                                    crudeoilprice = oil_price_fun(sD,yeartime),
                                                                                    crackspread = crackspread_fun(type="diesel",sD,yeartime), 
                                                                                    margin=0.12,
                                                                                    carbon_tax=carbon_tax_fun(sD,yeartime),
                                                                                    excise_duty =excise_duty_fun("diesel",sD,yeartime), 
                                                                                    NORA=0.02, 
                                                                                    bio = 0.014, 
                                                                                    VAT=0.23)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="gasoline", value=fuel_price_fun(type="gasoline",
                                                                                    crudeoilprice=oil_price_fun(sD,yeartime),
                                                                                    crackspread = crackspread_fun(type="gasoline",sD,yeartime), 
                                                                                    margin=0.12,
                                                                                    carbon_tax=carbon_tax_fun(sD,yeartime),
                                                                                    excise_duty = excise_duty_fun("gasoline",sD,yeartime),
                                                                                    NORA=0.02,
                                                                                    bio = 0.014,
                                                                                    VAT=0.23)))
  
  return(scen)
}

#' scenario_params
#'
#' hashed for fast lookup
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return environment
#' @export
#'
#' @examples
scenario_params <- function(sD,yeartime){

  scen <- scenario_params_df(sD,yeartime)
  test <- as.list(scen$value)
  names(test) <- scen$parameter
  test <- list2env(test)
  return(test)
}


#' get_newtech_2021
#' 
#' past (pre 2022) new fleet parameters derived from adjusted 2021 fleet prices and tech. 
#' Adjustments made include falling battery pack prices for EVs, efficiency (AER adjustments), ICEVs specific emissions reductions etc
#' models with model_start > yeartime are excluded. separate deflation rates of prices for new tech (hev, phev and bevs) are allowed 
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return current fleet data frame
#' @export
#'
#' @examples
get_newtech_2021 <- function(sD, yeartime){
  #create new technology for 2021 and earlier
  params <- scenario_params(sD,yeartime)
  #flag0 <- ifelse(yeartime >= 2021, "new","old")
  flag0 <- flagr(yeartime)
   fleet1 <- fleet %>% dplyr::select(c(-motor,-vrt,-p_1,-p_2,-`comment`)) 
   fleet1 <- fleet1 %>% dplyr::filter(yeartime >= model_start & yeartime < (model_end+1))
   #update cost model (assumed to be battery costs only)
   tech <- fleet1 %>% dplyr::rowwise() %>% dplyr::mutate(p_0=tech_cost_fun(type=type,tech_cost=p_0,kWh=kWh,params) )
   #update wltp
   tech <- tech %>% dplyr::rowwise() %>% dplyr::mutate(wltp=params$fleet_factor*wltp,AER=params$range_factor*AER)
   tech <- tech %>% dplyr::mutate(p_1=p_0*(1+params$car_vat)/(1-0.9*vrt_rate(wltp,flag0)))
   tech <- tech %>% dplyr::mutate(p_2 = p_1-incentives_fun(type,p_1,params,wltp,new_used="new_car"))
   #adjust fleet emissions & vehicle range 
   #use UF for 
   return(dplyr::select(tech,make,model,segment,type,p_0,p_1,p_2,wltp,kWh,AER,model_start,model_end))
  
}


#' get_newtech_2022
#' 
#' future (post 2021) new fleet parameters derived by adjusting adjusting 2022 fleet prices and tech. 
#' Adjustments include falling battery pack prices for EVs, efficiency (AER adjustment), ICEV specific emissions reductions (described by fleet_factor_fun).
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return a new car fleet data frame
#' @export
#'
#' @examples
get_newtech_2022 <- function(sD, yeartime){
  #create new technology for 2021 and earlier
  params <- scenario_params(sD,yeartime)
  #flag0 <- ifelse(yeartime >= 2021, "new","old")
  flag0 <- flagr(yeartime)
  fleet1 <- fleet_2022 %>% dplyr::select(c(-motor,-vrt,-p_1,-p_2,-`comment`)) 
  #fleet1 <- fleet1 %>% dplyr::filter(yeartime >= model_start & yeartime < (model_end+1))
  #update cost model (assumed to be battery costs only)
  tech <- fleet1 %>% dplyr::rowwise() %>% dplyr::mutate(p_0=tech_cost_fun(type=type,tech_cost=p_0,kWh=kWh,params) )
  #update wltp
  tech <- tech %>% dplyr::rowwise() %>% dplyr::mutate(wltp=params$fleet_factor*wltp,AER=params$range_factor*AER)
  tech <- tech %>% dplyr::mutate(p_1=p_0*(1+params$car_vat)/(1-0.9*vrt_rate(wltp,flag0)))
  tech <- tech %>% dplyr::mutate(p_2 = p_1-incentives_fun(type,p_1,params,wltp,new_used="new_car"))
  #adjust fleet emissions & vehicle range 
  #use UF for 
  return(dplyr::select(tech,make,model,segment,type,p_0,p_1,p_2,wltp,kWh,AER))
  
}


#' get_newtech
#' 
#' New technology dataframe available at yeartime based on 2021 and 2022 new car fleets. 
#' The 2021 fleet is used to project backwards and the 2022 fleet is used to project forwards in time
#'
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return new car fleet at yeartime
#' @export
#'
#' @examples
get_newtech <- function(sD,yeartime){
  
  ifelse(yeartime >= 2022, return(get_newtech_2022(sD,yeartime)), return(get_newtech_2021(sD,yeartime)))
  
}


#' get_used_tech
#' 
#' available used fleet parameters at future or past times based on adjusted new fleet prices and tech. 
#' adjustments made include falling battery pack prices for EVs, efficiency (AER adjustments), ICEVs specific emissions reductions etc
#' only models with model_start < yeartime are included
#'
#' @param sD scenario dataframe
#' @param yeartime decimal purchase time of vehicle
#' @param vehicle_age age of vehicle (decimal time)
#'
#' @return current fleet data frame
#' @export
#'
#' @examples
get_used_tech <- function(sD, yeartime, vehicle_age=4){
  
  #used cars are assumed to be imported
  params <- scenario_params(sD,yeartime) #parameters taken at yeartime
  params_old <- scenario_params(sD,yeartime-vehicle_age) #technical parameters when vehicle was new
  #use 2021 or 2022 fleets
  if(yeartime-vehicle_age >= 2022) {
    tech <- get_newtech_2022(sD,yeartime-vehicle_age)
  }
  if(yeartime-vehicle_age < 2022) {
    tech <- get_newtech_2021(sD,yeartime-vehicle_age)
    tech <- tech %>% dplyr::filter(yeartime - vehicle_age >= model_start & yeartime - vehicle_age < (model_end+1))
  }
  #current technology and pricing 
  #bev depreciation includes additional depreciation due to falling battery price
  deprc <- (1-params$deprec_0)^vehicle_age #assume same depreciation for all vehicles including zevs (i.e. risk premium is absent)
  deprc_bat <- (1-params$deprec_battery)^vehicle_age
  #battery (AER) degradation for used car buyers
  degrade <- params$battery_degrade^vehicle_age
  flag0 <- flagr(yeartime) #old, 2021 or new tax regime  at time of vehicle import
 
  #depreciate technology cost at current battery market price
  #this includes battery price falls
  tech <- tech %>% dplyr::rowwise() %>% dplyr::mutate(p_0=replace(p_0,type %in% c("hev","diesel","petrol"),deprc*p_0))
  #battery value is depreciated at current cost not at orginal cost i.e. faster
  tech <- tech %>% dplyr::rowwise() %>% dplyr::mutate(p_0=replace(p_0,type %in% c("bev","phev"),deprc*(p_0 - kWh*params_old$battery_cost) + deprc_bat*kWh*params$battery_cost ))
  #degrade
  tech <- tech %>% dplyr::mutate(wltp = params$fleet_factor*wltp,kWh=degrade*kWh,AER=degrade*params_old$range_factor*AER)
  
  #treat as fresh import
  
  tech <- tech %>% dplyr::mutate(p_1=p_0*(1+params$car_vat)/(1-0.9*vrt_rate(wltp,flag0)))
  #"used_car" flag applied vrt rebate but not SEAI grant
  tech <- tech %>% dplyr::mutate(p_2 = p_1-incentives_fun(type,p_1,params,wltp,new_used="used_car"))
  return(dplyr::select(tech,make,model,segment,type,p_0,p_1,p_2,wltp,kWh,AER))
  
}


#' update_agents4
#'
#' Micro-simulation Updater
#'
#' The workhorse ABM function.Within a scenario, does a single month update of the agent characteristics. A random sample of agents evaluates their economic and social
#' utilities. If these exceed their individual threshold and EV is adopted.
#'
#'
#' @param sD  scenario dataframe
#' @param yeartime decimal time
#' @param agents_in input agent dataframe
#' @param social_network artifical social network
#'
#' @return new agent dataframe
#' @export
#' @importFrom magrittr %>%
#' @examples
update_agents4 <- function(sD,yeartime,agents_in, social_network){

  dU_enviro <- dplyr::filter(util_empirical,name=="q17a_2")$shap
  dU_social <- dplyr::filter(util_empirical,name=="qev29")$shap
  #lambda. <- 0.12
  a_s <- agents_in
  a_s$transaction <- F
  a_s <- dplyr::ungroup(a_s)

  #parameters from scenario corresponding to yeartime
  params <- scenario_params(sD,yeartime) #enviroment object
  tco_term <- params$tco_term
  #depreciation vector over 3-year (for TCO)
  deprc_vals <- list("petrol"=(1-params$deprec_0)^tco_term,
                      "diesel"=(1-params$deprec_diesel)^tco_term,
                      "hev"=(1-params$deprec_hev)^tco_term,
                      "phev"=(1-params$deprec_phev)^tco_term,
                      "bev" = (1-params$deprec_bev)^tco_term
                      )
  
  depr_ev <- (1-params$deprec_ev)^tco_term
  depr <- (1-params$deprec_0)^tco_term
  #modify mileage (VKT reduction measures not including price elasticity)
  mileage_vkt <- mileage_vals*(1-params$vkt_reduction)

  flag <- flagr(yeartime) #whether to use old, 2021 or new budget 2022 tax bands

  #current fleet parameters
  new_tech <- get_newtech(sD,yeartime)
  #4 year old vehicles
  used_car_age_fun <- function(){
    
    age_0 <- 2015-mean(agents_init$reg)
    #f_new <- dim(filter(agents_init,qev31==1))[1]/924 #fraction with 
    f_new <- 287/924
    # a_{t+1} = (a_{t}+1/12)(1-M_{rep}/N_f) + a M_{used}/N_f
    (age_0 - (age_0+1/12)*(1-15/924))/((1-f_new)*15/924)
  }
  
  used_car_age <- used_car_age_fun() - 0.75 #apply 9 month adjustment
  used_tech <- get_used_tech(sD,yeartime,vehicle_age = used_car_age)
  
  b_s <- dplyr::slice_sample(a_s,n=round(dim(a_s)[1]*p.))  #this subsample of agents decide to replace their car
  b_s <- b_s %>% dplyr::mutate(transaction=T)
  #
  model_choice <- function(tech,qev31_in,segment_in,make_in){
    #car model selection
    #agents do not look at the whole market => random utility
    #ntype (stochastic) is the max number of each type in selection
    #loyalty parameter
    ntype <- sample(c(2,3),size=1,prob =c(0.5,0.5))
    p <- ifelse(qev31_in==1, params$brand_loyalty_new,params$brand_loyalty_used)
    p <- 0.5*(p+1)
    t <- dplyr::filter(tech, segment == segment_in) %>% dplyr::mutate(wt=ifelse(make==make_in,p,1-p))
    t <- t %>% dplyr::group_by(type) %>% dplyr::slice_sample(n=ntype,replace=T,weight_by=wt) %>% dplyr::distinct()
    t <- t %>% dplyr::slice_sample(n=ntype,replace=T,weight_by=wt) %>% dplyr::distinct()
    return(t %>% dplyr::select(-wt))
    #return(t)
  }
  
  #make
  update_cars <- function(b_s,params){
    #compute full utilities
    flag <- flagr(params$yeartime)
    choices <- vector("list",dim(b_s)[1])
    b_s <- b_s %>% dplyr::rename(old_type=type)
    for(i in 1:dim(b_s)[1]){
      ag <- b_s[i,]
      ifelse(ag$qev31 != 1,
        models <- dplyr::bind_cols(dplyr::select(ag,ID,w_econ,w_social,w_enviro,w_theta,qev32,q17a_2,qev29,qev34,qev31,old_type,reg),model_choice(used_tech,ag$qev31,ag$segment, ag$make)),
        models <- dplyr::bind_cols(dplyr::select(ag,ID,w_econ,w_social,w_enviro,w_theta,qev32,q17a_2,qev29,qev34,qev31,old_type,reg),model_choice(new_tech,ag$qev31,ag$segment, ag$make))
       )
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(reg=ifelse(qev31 != 1,trunc(yeartime-used_car_age),trunc(yeartime)))
       #TCO discount cost: buyers assume they capture incentives but subsequent purchaser does not
       #distinct 3 year values for each type
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(u_econ=-w_econ*beta.*((p_2-deprc_vals[[type]]*p_1)/3 + fuelcost_fun(type,wltp,kWh,AER,params)*mileage_vkt[qev34] + motor_tax(wltp,flag))/budget_vals[qev32])
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(u_social=ifelse(type=="bev", dU_social[min(qev29,3)],0))
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(u_enviro=ifelse(type=="bev" & old_type != "bev", w_enviro*dU_enviro[q17a_2],0))
       #range anxiety term - tends to be zero or dominant
       models <- models %>% dplyr::mutate(u_anx=0)
       #range anxiety is calculated off original mileages (may give a better reflection of probability of long trips)
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(u_anx=replace(u_anx,type=="bev",-u_anxiety(mileage_vals[qev34],AER,anxiety=params$anxiety,a_prefactor = params$anxiety_prefactor)))
      #assume no barrier from PHEV to BEV (other than range anxiety)
       models <- models %>% dplyr::mutate(theta =0)
       #add rosk aversion barrier for bev or phev adoption.
      models <- models %>% dplyr::rowwise() %>% dplyr::mutate(theta = replace(theta,type=="bev" & old_type %in% c("petrol","diesel","hev"),-w_theta*(theta.) + lambda.))
      models <- models %>% dplyr::rowwise() %>% dplyr::mutate(theta = replace(theta,type=="phev" & old_type %in% c("petrol","diesel","hev"),params$phev_aversion*(lambda.-w_theta*(theta.))))
      models <- models %>% dplyr::select(-any_of(c("model_start","model_end")))
      models <- models[,12:27] %>% dplyr::rowwise() %>% dplyr::mutate(u=u_econ+u_social+u_enviro+u_anx+theta)
      #add hedonic term that penalises cars cheaper than budget?
      #add brand loyalty effect to prevent skodaisation
      #add range anxiety than penalised low range bevs for higher mileage drivers? battery size penalty term?
      #add lower threshold for phev -> bev switching 
      choices[[i]] <- models[which.max(models$u),]
    }
    choices <- dplyr::bind_rows(choices)
    return(dplyr::bind_cols(dplyr::select(b_s, ID,w_econ,w_social,w_enviro,w_distance,w_theta, qev32,qev29,q17a_2,qev34,qev31,transaction),choices[,1:11]) )                                        
  }
  
  b_s <- update_cars(b_s,params)
  
  a_s <- dplyr::filter(a_s, !(ID %in% b_s$ID))
  a_s <- dplyr::bind_rows(a_s,b_s) %>% dplyr::arrange(as.numeric(ID))
  #recompute social variable
  ma <- igraph::get.adjacency(social_network)
  g <- social_network %>% tidygraph::activate(nodes) %>% dplyr::left_join(a_s,by="ID")
  #social network conformity effect
  #
  #fossil_nodes  <- igraph::V(g)$fuel == "fossil"
  ev_nodes <- igraph::V(g)$type == "bev"
  a_s$qev29 <- as.numeric(ma %*% ev_nodes) #social reinforcement
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(qev29 = min(qev29+1,3))
  agents_out <- a_s
  print(paste("decimal time", round(yeartime,1),"BEV adopters",sum(agents_out$type=="bev")-sum(agents_in$type=="bev")))
  return(dplyr::ungroup(agents_out))
}

#' runABM
#'
#' Runs the simulation on artificial society of 924 agents. Each run is performed on an independently generated social network with random initial ICEV emissions
#'
#' @param sD scenario set-up dataframe, typically read with readxlxs(...,sheet=scenario)
#' @param Nrun integer, number runs
#' @param simulation_end the final year of simulation of early termination is required
#' @param resample_society if TRUE resample society with replacement to capture additional variability
#' @param n_unused_cores number of cores left unused in parallel/foreach. Recommended values 2 or 1.
#' @param use_parallel if TRUE uses multiple cores. Use FALSE for diagnostic runs
#'
#' @return a three component list - simulation output, scenario setup, meta-parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar%
#' 
runABM <- function(sD, Nrun=1,simulation_end=end_year,resample_society=F,n_unused_cores=2, use_parallel=T){
  #
  year_zero <- 2015
  Nt <- round((simulation_end-year_zero+1)*12)
  #comment out net line for parallel
  #abm <- tibble::tibble()
  #comment in next two lines for parallel
  if(use_parallel){
    
  number_of_cores <- parallel::detectCores() - n_unused_cores
  doParallel::registerDoParallel(number_of_cores)

  #comment out next line for parallel
  #for(j in 1:Nrun){
  #comment in next line for parallel
  abm <- foreach::foreach(j = 1:Nrun, .combine=dplyr::bind_rows,.export = c("initialise_segments","update_agents4")) %dopar% {
    #create a new artificial society for each run
    print(paste("Generating network for run",j,"...."))
    if(!resample_society) social <- createArtificalSociety(society,homophily,4.5)
    if(resample_society){
      agent_resample <- sample(1:924,replace=T)
      society_new <- society[agent_resample,]
      society_new$ID <- 1:924
      social <- createArtificalSociety(society_new,homophily,4.5)

    }
    #randomiise ICEV emissions assignment
    #choose segments
    agents_in <- initialise_segments(sD)
    #no transactions
    agents_in$transaction <- FALSE
    #fraction of drivers who are uncer
    r_new <- dplyr::filter(sD,parameter=="r_new")$value
    agents_in <- agents_in %>% dplyr::rowwise() %>% dplyr::mutate(qev31 = replace(qev31, qev31==3, sample(c(1,2),1,prob=c(r_new,1-r_new))))
    agents_in <- dplyr::ungroup(agents_in)
    agent_ts<- vector("list",Nt)
    agent_ts[[1]] <- agents_in #agent paraneters with regularized weights

    for(t in seq(2,Nt)){
      #
      yeartime <- year_zero+(t-1)/12
      agent_ts[[t]] <- update_agents4(sD,yeartime,agent_ts[[t-1]],social_network=social) #static social network, everything else static
      #agent_ts[[t]] <- tibble::tibble(t=t)
      }

    for(t in 1:Nt) agent_ts[[t]]$t <- t
    agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
    agent_ts$simulation <- j
    #add vertex degree
    degrees <- tibble::tibble(ID=1:924,degree=igraph::degree(social))
    agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
    agent_ts
  }
  meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","theta.","lambda.","p.","wltp_nedc_ratio"),value=c(Nrun,end_year,beta.,theta.,lambda.,p.,wltp_nedc_ratio))
  return(list("abm"=abm,"scenario"=sD,"system"=meta))
  }
  
  #abm <- tibble::tibble()
  #comment in next two lines for parallel
  if(!use_parallel){
    abm<- tibble::tibble()
    #number_of_cores <- parallel::detectCores() - n_unused_cores
    #doParallel::registerDoParallel(number_of_cores)
    
    #comment out next line for parallel
    for(j in 1:Nrun){
    #comment in next line for parallel
    #abm <- foreach::foreach(j = 1:Nrun, .combine=dplyr::bind_rows,.export = c("initialise_segments","update_agents4")) %dopar% {
      #create a new artificial society for each run
      print(paste("Generating network for run",j,"...."))
      if(!resample_society) social <- createArtificalSociety(society,homophily,4.5)
      if(resample_society){
        agent_resample <- sample(1:924,replace=T)
        society_new <- society[agent_resample,]
        society_new$ID <- 1:924
        social <- phevmicrosimr::createArtificalSociety(society_new,homophily,4.5)
        
      }
      #randomise ICEV emissions assignment
      #choose market segment for each agent
      agents_in <- initialise_segments(sD)
      #no transactions
      agents_in$transaction <- FALSE
      #fraction of agents who are uncertain who buy a new car
      r_new <- dplyr::filter(sD,parameter=="r_new")$value
      agents_in <- agents_in %>% dplyr::rowwise() %>% dplyr::mutate(qev31 = replace(qev31, qev31==3, sample(c(1,2),1,prob=c(r_new,1-r_new))))
      agents_in <- dplyr::ungroup(agents_in)
      agent_ts<- vector("list",Nt)
      agent_ts[[1]] <- agents_in #agent paraneters with regularized weights
      
      for(t in seq(2,Nt)){
        #
        yeartime <- year_zero+(t-1)/12
        agent_ts[[t]] <- update_agents4(sD,yeartime,agent_ts[[t-1]],social_network=social) #static social network, everything else static
        #agent_ts[[t]] <- tibble::tibble(t=t)
      }
      
      for(t in 1:Nt) agent_ts[[t]]$t <- t
      agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
      agent_ts$simulation <- j
      #network degree
      degrees <- tibble::tibble(ID=1:924,degree=igraph::degree(social))
      agent_ts <- agent_ts %>% dplyr::inner_join(degrees)
      abm <- dplyr::bind_rows(abm,agent_ts)
      #comment in next line for parallel
      #agent_ts
    }
    meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","lambda.","theta.","p.","wltp_nedc_ratio"),value=c(Nrun,end_year,beta.,theta.,lambda.,p.,wltp_nedc_ratio))
    return(list("abm"=abm,"scenario"=sD,"system"=meta))
  }
  
}


#test[[1]] %>% group_by(t,type) %>% summarise(n=n()/924) %>% ggplot(aes(2015+(t-1)/12,n,colour=type)) + geom_line() + scale_y_continuous(trans="sqrt",breaks=c(0.005,0.01,0.05,0.2,0.4,0.8))