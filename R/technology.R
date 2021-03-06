#scenario_1 <- readxl::read_xlsx("~/Policy/AgentBasedModels/PHEVs/scenarioDesignPHEV.xlsx",sheet=2)
#scenario_2 <- readxl::read_xlsx("~/Policy/AgentBasedModels/PHEVs/scenarioDesignPHEV.xlsx",sheet=3)
#scenario_3 <- readxl::read_xlsx("~/Policy/AgentBasedModels/PHEVs/scenarioDesignPHEV.xlsx",sheet=4)

#use_data(scenario_3,overwrite=T)
#use_data(scenario_2,overwrite=T)
#use_data(scenario_1,overwrite=T)
#fleet <- readxl::read_xlsx("~/Policy/AgentBasedModels/PHEVs/survey_fleet.xlsx",sheet=6, range="A1:P347")
#bevs <- filter(fleet,type=="bev")
#fleet <- fleet %>% filter(!is.na(`2021_rrp`))
#fleet$kWh <- as.numeric(fleet$kWh)
#fleet <- fleet %>% mutate(kWh=ifelse(!is.na(kWh),kWh,0))
#fleet <- fleet %>% mutate(model_start=replace(model_start,is.na(model_start),1990))
#fleet <- fleet %>% mutate(model_end= replace(model_end,is.na(model_end),3000))
#use_data(fleet,overwrite = T)


get_fleet_data <- function(fleet_dirty){
  
  fleet <- fleet %>% dplyr::filter(!is.na(`2021_rrp`))
  fleet$kWh <- as.numeric(fleet$kWh)
  fleet <- fleet %>% dplyr::mutate(kWh=ifelse(!is.na(kWh),kWh,0))
  fleet <- fleet %>% dplyr::mutate(model_start=replace(model_start,is.na(model_start),1990))
  fleet <- fleet %>% dplyr::mutate(model_end= replace(model_end,is.na(model_end),3000))
  ifelse(any(is.na(fleet$tech_cost_2021)),print("bad input"),print("good input"))
  return(fleet)
}


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
emissions_model1 <- function (mileage, mean_emissions=190, sd_emissions=30, e_cut=80,zeta=0.75) {
  #
  m_emissions <- mean_emissions + zeta*sd_emissions*tanh((mileage-10000)/20000)
  shape_param <- ((m_emissions-e_cut)/sd_emissions)^2
  theta <- (m_emissions-e_cut)/shape_param
  return(stats::rgamma(1,scale=theta,shape=shape_param)+e_cut)

}



#' Oil price scenario
#'
#' @param sD scenario dataframe
#' @param yeartime real
#'
#' @return real
#' @export
#'
#' @examples
oil_price_fun <- function(sD,yeartime){

  crudeoilprice_2020 <- dplyr::filter(sD, parameter=="crudeoilprice_2020")$value
  crudeoilprice_2030 <- dplyr::filter(sD, parameter=="crudeoilprice_2030")$value
  crudeoilprice_2050 <- dplyr::filter(sD, parameter=="crudeoilprice_2050")$value

  oil <- stats::approx(c(2015:2019+0.5,2030,2050),c(dplyr::filter(opec_oilprices, year %in% 2015:2019)$price_euro,crudeoilprice_2030,crudeoilprice_2050),xout=yeartime,rule=2)$y
  return(oil)

}



#' crackspread_fun
#'
#' refining margin for diesel or gasoline
#'
#' @param type diesel or gasoline
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return euros/barrel
#' @export
#'
#' @examples
crackspread_fun <- function(type,sD,yeartime){
  if(!(type %in% c("diesel","gasoline"))) stop("bad fuel type")
  if(type=="diesel") return( dplyr::filter(sD,parameter=="crackspread_diesel")$value)
  if(type=="gasoline") return( dplyr::filter(sD,parameter=="crackspread_gasoline")$value)
  
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
fuel_price_fun <- function(type="diesel",crudeoilprice=50,crackspread = 10, margin=0.12,carbon_tax=33.5, excise_duty=0.54, NORA=0.02, bio = 0.014, VAT=0.23){
  #euros per litre retail
  if(type=="diesel")
  return(((crudeoilprice+crackspread)/159+margin + excise_duty + carbon_tax/1000*2.640 + NORA)*(1+VAT))
  if(type=="gasoline")
  return(((crudeoilprice+crackspread)/159+margin + excise_duty + carbon_tax/1000*2.392 + NORA)*(1+VAT))
  
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
fuel_price_fun1 <- function(type="diesel", sD, yeartime){
  
               crudeoilprice <- oil_price_fun(sD,yeartime)
               crackspread <- crackspread_fun(type="diesel",sD,yeartime)
               margin <- 0.12
               carbon_tax <- carbon_tax_fun(sD,yeartime)
               excise_duty <- diesel_excise_duty_fun(sD,yeartime)
               NORA <- 0.02 
               bio <-  0.014
               VAT <- 0.23
               
               if(type=="diesel")
                 return(((crudeoilprice+crackspread)/159+margin + excise_duty + carbon_tax/1000*2.640 + NORA)*(1+VAT))
               if(type=="gasoline")
                 return(((crudeoilprice+crackspread)/159+margin + excise_duty + carbon_tax/1000*2.392 + NORA)*(1+VAT))
               

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
fuelcost_fun <- function(type,wltp=0,kWh, AER, params,xi=1){
  #
  dplyr::recode(type,
   diesel = wltp*params$diesel/2640,
   petrol = wltp*params$gasoline/2392,
   hev = wltp*params$gasoline/2392,
   bev = params$e_price*kWh/AER,
   phev = uf(AER)*params$e_price*kWh/AER + wltp*params$gasoline/2392)
}

#' BEV price premium (euros)
#'
#' @param sD scenario dataframe
#' @param yeartime real
#'
#' @return real
#' @export
#'
#' @examples
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
#' @return real anxiety parameter for bevs
#' @export
#'
#' @examples
anxiety_fun <- function(sD,yeartime){

  initial_range_anxiety <- dplyr::filter(sD,parameter=="initial_range_anxiety")$value
  anxiety_fall_year <- dplyr::filter(sD,parameter=="anxiety_fall_year")$value
  longterm_range_anxiety <- dplyr::filter(sD,parameter=="longterm_range_anxiety")$value
  longterm_range_anxiety_year <- dplyr::filter(sD,parameter=="longterm_range_anxiety_year")$value
  
  return(stats::approx(x=c(anxiety_fall_year,longterm_range_anxiety_year),y=c(initial_range_anxiety,longterm_range_anxiety),xout=yeartime,rule=2)$y)
}

#' u_anxiety
#' 
#' Range anxiety is assumed to reflect the probability that the daily trip exceeds the electric vehicle AER, requiring a charge.
#' the distribution of daily trips is gamma distributed with shape parameter 2.5. subjectivity via "anxiety" parameter anxiety
#'
#' @param mileage annual mileage in km
#' @param AER range in km
#' @param shape gamma distribution 
#' @param anxiety range anxiety. 1 is non-anxious case, > 1 is anxious case
#'
#' @return probabilty that a daily trip exceeds AER
#' @export
#'
#' @examples
u_anxiety <- function(mileage,AER,shape=2.5,anxiety){
  #probability that a trip exceeds AER
  mean_daily <- mileage/300 #assume vehicle is driven 300 days per year
  #adjust scale to reflect driver anxiety
  return(1-pgamma(AER,shape=shape,scale=anxiety*mean_daily/shape))
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
  if(yeartime >= 2020.5 + nlag) return(max(b1,b0*exp(-alpha*(yeartime-(2020.5+nlag))) + b1 ))  
  if(yeartime <= 2020.5 + nlag) return(stats::approx(x=bnefprices$year+0.5,y=bnefprices$pack_ekWh, xout=yeartime-nlag)$y)
  #if(yeartime < 2021 & yeartime >= 2018) return( 264.7*(1+0.05)^(2021-yeartime))
  #if(yeartime < 2018) return( 264.7*(1+0.15)^(2021-yeartime))
  
}


#' VKT reduction
#'
#' Reduction factor of vehicle kilometers travelled as a function of mileage
#'
#' @param sD scenario dataframe
#' @param mileage km
#' @param yeartime decimal time
#'
#' @return  dimensionless fraction
#' @export
#'
#' @examples
vkt_reduction_fun <- function(sD,mileage,yeartime){

  highmiles <- dplyr::filter(sD, parameter=="VKT_high_2030")$value
  lowmiles <- dplyr::filter(sD, parameter=="VKT_low_2030")$value

  return(1-stats::approx(x=c(0,max(mileage_vals)), y=c(lowmiles,highmiles),xout=mileage,rule = 2)$y)


}



#' tech_cost_fun
#' 
#' technology costs based on 2021 pre-tax and incentive list prices. 
#' At present the cost model is based on battery pack price changes only.
#'
#' @param type powertrain/fuel type
#' @param tech_cost 2021 technology cost
#' @param kWh battery
#' @param params scenario params e.g. scenario_1
#'
#' @return cost in euros
#' @export
#'
#' @examples
tech_cost_fun <- function(type,tech_cost,kWh=NA,params){
  
  dplyr::recode(type,
                
                diesel = tech_cost,
                petrol= tech_cost,
                hev = tech_cost +kWh*(params$battery_cost- params$battery_cost_2021),
                phev = tech_cost+kWh*(params$battery_cost- params$battery_cost_2021),
                bev = ifelse(params$yeartime >= 2021, tech_cost + kWh*(params$battery_cost- params$battery_cost_2021), tech_cost)
  )
  
}



#' fleet_factor_fun
#' 
#' ICEV fleet emissions normalises to 2021 fleet WLTP values.
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
    g1 <- 1+18*f1
    g2 <- g1*(1+7*f2)
    g3 <- g2*(1+6*f3)
    g4 <- g3*(1+(y_end-2020)*f4)
    fact <- stats::approx(x=c(1990,2008,2015,2020,y_end),y=c(1,g1,g2,g3,g4),xout=yeartime,rule=2)$y
  return(fact)}
  
  return(func(sD,yeartime)/func(sD,2021))
  
  
}


#' range_factor_fun
#'
#' BEV range function factor
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
#' a car model is the assigned by on segment choice, observed Irish make preferences 2014-2019 & 2021, and 2021 new fleet dataframe
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
    #car segments and makes from cso 2014-2019
    init_car <- initial_fleet %>% dplyr::filter(segment==seg)
    #2021 fleet cars fossil only
    #fleet1 <- fleet %>% dplyr::filter(segment==seg, type %in% c("petrol","diesel"))
    fleet1 <- fleet %>% dplyr::filter(segment==seg, type == fuel)
    if(dim(fleet1)[1]==0) return("segment fuel cominbination not present in fleet")
    fleet1 <- fleet1 %>% dplyr::select(segment,make,model,type,WLTP)
    init_car <- dplyr::filter(init_car, make %in% fleet1$make)
    #choose car weighted by cso probabilities
    car_models <- dplyr::filter(fleet1,make==dplyr::slice_sample(init_car,n=1, weight_by = init_car$number)$make)
    #pick one if there are multiple options exist
    car_models <- car_models %>% dplyr::slice_sample(n=1)
    return(car_models)
  }
  
  bay <- segments_bayes_posteriors %>% tidyr::pivot_longer(-ID,names_to="segment",values_to="prob")
  car_segments <- unique(bay$segment)
  init <- bay %>% dplyr::rowwise() %>% dplyr::group_by(ID) %>% dplyr::summarise(segment=sample(car_segments,1,prob=prob))
  agents_in1 <- dplyr::inner_join(agents_init,init,by="ID")
  agents_in1 <- dplyr::bind_cols(dplyr::select(agents_in1,-type0,-segment),dplyr::select(agents_in1,segment,type0) %>% dplyr::rowwise() %>% dplyr::do(choose_initial_car(.$segment,.$type0)))
  #assume new car drivers are driving 3 year old cars
  #used car drivers are driving 10 year old cars
  agents_in1 <- agents_in1 %>% dplyr::mutate(WLTP = fleet_factor_fun(sD,reg)*WLTP)
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
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_0", value =   dplyr::filter(sD, parameter=="depreciation_0")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_diesel", value =   dplyr::filter(sD, parameter=="depreciation_0")$value + depreciation_spread_fun("diesel",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_hev", value =   dplyr::filter(sD, parameter=="depreciation_0")$value + depreciation_spread_fun("hev",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_phev", value =   dplyr::filter(sD, parameter=="depreciation_0")$value + depreciation_spread_fun("phev",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="deprec_bev", value =   dplyr::filter(sD, parameter=="depreciation_0")$value + depreciation_spread_fun("bev",sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="anxiety", value =   anxiety_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="oil", value =  oil_price_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="excise_diesel", value =  diesel_excise_duty_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="excise_gasoline", value =  gasoline_excise_duty_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="e_price", value =  electricity_price_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="conversion", value =  conversion_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="ev_runcost", value =  bev_fuelcost(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="diesel", value=fuel_price_fun(type="diesel",
                                                                                    crudeoilprice = oil_price_fun(sD,yeartime),
                                                                                    crackspread = crackspread_fun(type="diesel",sD,yeartime), 
                                                                                    margin=0.12,
                                                                                    carbon_tax=carbon_tax_fun(sD,yeartime),
                                                                                    excise_duty =diesel_excise_duty_fun(sD,yeartime), 
                                                                                    NORA=0.02, 
                                                                                    bio = 0.014, 
                                                                                    VAT=0.23)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="gasoline", value=fuel_price_fun(type="gasoline",
                                                                                    crudeoilprice=oil_price_fun(sD,yeartime),
                                                                                    crackspread = crackspread_fun(type="gasoline",sD,yeartime), 
                                                                                    margin=0.12,
                                                                                    carbon_tax=carbon_tax_fun(sD,yeartime),
                                                                                    excise_duty = gasoline_excise_duty_fun(sD,yeartime),
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


#' get_current_tech
#' 
#' new fleet parameters at current (pre or post 2021) times derived from adjusted 2021 fleet prices and tech. 
#' Adjustments made include falling battery pack prices for EVs, efficiency (AER adjustments), ICEVs specific emissions reductions etc
#' only models with model_start < yeartime are included
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return current fleet data frame
#' @export
#'
#' @examples
get_current_tech <- function(sD, yeartime){
  
  params <- scenario_params(sD,yeartime)
  flag0 <- ifelse(yeartime >= 2021, "new","old")
  fleet1 <- fleet %>% dplyr::select(c(-motor,-vrt,-`2021_rrp`,-`2021_rrp*`,-`comment`,-`kWh/100km`)) 
  fleet1 <- fleet1 %>% dplyr::filter(yeartime >= model_start & yeartime < (model_end+1))
  tech <- fleet1 %>% dplyr::rowwise() %>% dplyr::mutate(p_0=tech_cost_fun(type=type,tech_cost=tech_cost_2021,kWh=kWh,params) )
  tech <- tech %>% dplyr::mutate(p_1=p_0*(1+params$car_vat)/(1-0.9*vrt_rate(WLTP,flag0)))
  tech <- tech %>% dplyr::mutate(p_2 = p_1-incentives_fun(type,p_1,params,flag0,WLTP,new_used="new_car"))
  #adjust fleet emissions & vehicle range 
  #use UF for 
  tech <- tech %>% dplyr::mutate(WLTP = params$fleet_factor*WLTP, AER=params$range_factor*AER)
  return(dplyr::select(tech,make,model,segment,type,WLTP,kWh,AER,p_0,p_1,p_2))
  
}


#' get_used_tech
#' 
#' old fleet parameters at future or past times derived from adjusted 2021 fleet prices and tech. 
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
  
  params <- scenario_params(sD,yeartime) #tax parameters taken at yeartime
  params_old <- scenario_params(sD,yeartime-vehicle_age) #technical parameters taken at yeartime-vehicle_age
  deprc <- (1-params$deprec_0)^vehicle_age #assume same depreciation for all vehicles including zevs (i.e. risk premium is absent)
  flag0 <- ifelse(yeartime >= 2021, "new","old") #old or new tax regime  at time of import
  fleet1 <- fleet %>% dplyr::select(c(-motor,-vrt,-`2021_rrp`,-`2021_rrp*`,-`comment`,-`kWh/100km`)) 
  fleet1 <- fleet1 %>% dplyr::filter(yeartime - vehicle_age >= model_start & yeartime - vehicle_age < (model_end+1))
  #depreciation based on current technology cost of vehicle
  tech <- fleet1 %>% dplyr::rowwise() %>% dplyr::mutate(p_0=deprc*tech_cost_fun(type=type,tech_cost=tech_cost_2021,kWh=kWh,params) )
  #treat an import
  tech <- tech %>% dplyr::mutate(p_1=p_0*(1+params$car_vat)/(1-0.9*vrt_rate(WLTP,flag0)))
  #apply vrt rebate incentives but not SEAI grant
  tech <- tech %>% dplyr::mutate(p_2 = p_1-incentives_fun(type,p_1,params,flag0,WLTP,new_used="used_car"))
  tech <- tech %>% dplyr::mutate(WLTP = params$fleet_factor*WLTP,AER=params_old$range_factor*AER)
  return(dplyr::select(tech,make,model,segment,type,WLTP,kWh,AER,p_0,p_1,p_2))
  
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
  #depreciation vector
  deprc_vals <- list("petrol"=(1-params$deprec_0)^3,
                      "diesel"=(1-params$deprec_diesel)^3,
                      "hev"=(1-params$deprec_hev)^3,
                      "phev"=(1-params$deprec_phev)^3,
                      "bev" = (1-params$deprec_bev)^3
                      )
  
  depr_ev <- (1-params$deprec_ev)^3
  depr <- (1-params$deprec_0)^3
  #modify mileage (VKT policy)
  #mileage_vkt <- mileage_vals * sapply(mileage_vals, function(m) vkt_reduction_fun(sD,m,yeartime))

  flag <- ifelse(yeartime < 2021,"old","new") #whether to use old or new tax bands

  #current fleet parameters
  new_tech <- get_current_tech(sD,yeartime)
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
  
  b_s <- dplyr::slice_sample(a_s,n=round(dim(a_s)[1]*p.))  #test a subsample for car purchase and potential switching
  b_s <- b_s %>% dplyr::mutate(transaction=T)
  #
  model_choice <- function(tech,seg,ntype=2){
    #car selection
    #ntype is the max number of each type in selection
    dplyr::filter(tech, segment == seg)  %>% dplyr::group_by(type) %>% dplyr::slice_sample(n=ntype,replace=T) %>% dplyr::distinct()
  }
  
  #make
  update_cars <- function(b_s,params,flag){
    #compute full utilities
    choices <- vector("list",dim(b_s)[1])
    b_s <- b_s %>% dplyr::rename(old_type=type)
    for(i in 1:dim(b_s)[1]){
      ag <- b_s[i,]
      ifelse(ag$qev31 != 1,
        models <- dplyr::bind_cols(dplyr::select(ag,ID,w_econ,w_social,w_enviro,w_theta,qev32,q17a_2,qev29,qev34,qev31,old_type,reg),model_choice(used_tech,ag$segment)),
        models <- dplyr::bind_cols(dplyr::select(ag,ID,w_econ,w_social,w_enviro,w_theta,qev32,q17a_2,qev29,qev34,qev31,old_type,reg),model_choice(new_tech,ag$segment))
       )
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(reg=ifelse(qev31 != 1,trunc(yeartime-used_car_age),trunc(yeartime)))
       #TCO discount cost: buyers assume they capture incentives but subsequent purchaser does not
       #distinct 3 year values for each type
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(u_econ=-w_econ*beta.*((p_2-deprc_vals[[type]]*p_1)/3 + fuelcost_fun(type,WLTP,kWh,AER,params)*mileage_vals[qev34] + motor_tax(WLTP,flag))/budget_vals[qev32])
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(u_social=ifelse(type=="bev", dU_social[min(qev29,3)],0))
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(u_enviro=ifelse(type=="bev" & old_type != "bev", w_enviro*dU_enviro[q17a_2],0))
       #range anxiety term - tends to be zero or dominant
       models <- models %>% dplyr::mutate(u_anx=0)
       models <- models %>% dplyr::rowwise() %>% dplyr::mutate(u_anx=replace(u_anx,type=="bev",-u_anxiety(mileage_vals[qev34],AER,anxiety=params$anxiety)))
      #assume no barrier from PHEV to BEV (other than range anxiety)
       models <- models %>% dplyr::mutate(theta =0)
      models <- models %>% dplyr::rowwise() %>% dplyr::mutate(theta = replace(theta,type=="bev" & old_type %in% c("petrol","diesel","hev"),-w_theta*(theta.) + lambda.))
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
  
  b_s <- update_cars(b_s,params,flag)
  
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
#'
#' @return a three component list - simulation output, scenario setup, meta-parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar%
#' @examples
runABM <- function(sD, Nrun=1,simulation_end=end_year,resample_society=F,n_unused_cores=2){
  #
  Nt <- round((simulation_end-year_zero+1)*12)
  #comment out net line for parallel
  #abm <- tibble::tibble()
  #comment in next two lines for parallel
  number_of_cores <- parallel::detectCores() - n_unused_cores
  doParallel::registerDoParallel(number_of_cores)

  #comment out next line for parallel
  #for(j in 1:Nrun){
  #comment in next line for parallel
  abm <- foreach::foreach(j = 1:Nrun, .combine=dplyr::bind_rows,.export = c("initialise_segments","update_agents4")) %dopar% {
    #create a new artificial society for each run
    print(paste("Generating network for run",j,"...."))
    if(!resample_society) social <- microsim::createArtificalSociety(microsim::society,microsim::homophily,4.5)
    if(resample_society){
      agent_resample <- sample(1:924,replace=T)
      society_new <- microsim::society[agent_resample,]
      society_new$ID <- 1:924
      social <- microsim::createArtificalSociety(society_new,microsim::homophily,4.5)

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
    #comment out next line for parallel
    #abm <- dplyr::bind_rows(abm,agent_ts)
    #comment in next line for parallel
    agent_ts
  }
  meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","lambda.","A_bat","B_bat","M_bat","p.","wltp_nedc_ratio"),value=c(Nrun,end_year,beta.,lambda.,A_bat,B_bat,M_bat,p.,wltp_nedc_ratio))
  return(list("abm"=abm,"scenario"=sD,"system"=meta))
}


#test[[1]] %>% group_by(t,type) %>% summarise(n=n()/924) %>% ggplot(aes(2015+(t-1)/12,n,colour=type)) + geom_line() + scale_y_continuous(trans="sqrt",breaks=c(0.005,0.01,0.05,0.2,0.4,0.8))