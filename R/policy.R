#' VAT on cars
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return real vat rate
#' @export
#'
#' @examples
car_vat_fun <- function(sD, yeartime) {
  return(dplyr::filter(sD, parameter == "vat_car")$value)
}

#' Carbon tax
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return real
#' @export
#'
#' @examples
carbon_tax_fun <- function(sD, yeartime) {
  # impact of carbon tax in euros/km for a 5l/100km vehicle
  # return( (carbon_tax_2030-26)/100*0.015*max((year-2020)/10,0))
  carbon_tax_2030 <- dplyr::filter(sD, parameter == "carbon_tax_2030")$value
  carbon_tax_2050 <- dplyr::filter(sD, parameter == "carbon_tax_2050")$value
  vec <- seq(2000 + 5 / 12, 2050 + 5 / 12, by = 1)
  ctax <- c(rep(0,14), 10, 20, 20, 20, 20, 20, 20, seq(26, carbon_tax_2030, by = (carbon_tax_2030 - 26) / 10), seq(carbon_tax_2030, carbon_tax_2050, by = (carbon_tax_2050 - carbon_tax_2030) / 20)[-1])
  return(ctax[findInterval(yeartime, vec)])
}

#' Grant (linear phaseout)
#'
#' @param type car type bev, phev, hev, diesel, petrol
#' @param sD scenarion dataframe
#' @param yeartime decimal time
#'
#' @return grant level at yeartime. 1 is current grant level, 0 is no grant
#' @export
#'
#' @examples
grant_fun <- function(type,sD,yeartime) {
  # linear interp decay for SEAI grant
  if(type=="bev"){
  grant_phaseout_start <- dplyr::filter(sD, parameter == "bev_grant_phaseout_start")$value
  grant_phaseout_end <- dplyr::filter(sD, parameter == "bev_grant_phaseout_end")$value
  return(stats::approx(x = c(grant_phaseout_start, grant_phaseout_end), y = c(1, 0), xout = yeartime, rule = 2)$y)
  }
  if(type=="phev"){
    grant_phaseout_start <- dplyr::filter(sD, parameter == "phev_grant_phaseout_start")$value
    grant_phaseout_end <- dplyr::filter(sD, parameter == "phev_grant_phaseout_end")$value
    return(stats::approx(x = c(grant_phaseout_start, grant_phaseout_end), y = c(1, 0), xout = yeartime, rule = 2)$y)
  }
}


#' flagr
#'
#' @param yeartime decimal time
#'
#' @return a character flag identifying the tax rules to be applied
#' @export
#'
#' @examples
flagr <- function(yeartime){
  
  if(yeartime >= 2022) return("new")
  if(yeartime < 2021)  return("old")
  if(yeartime >=2021 & yeartime < 2022) return("2021")
}


#' grant_fun1
#' 
#' grant phaseout scenario for phevs and bevs
#'
#' @param type phev or bev
#' @param sD scenario
#' @param yeartime decimal time
#'
#' @return grant phaseout path usually in 0,1
#' @export
#'
#' @examples
grant_fun1 <- function(type,sD,yeartime) {
  # linear interp decay for SEAI grant
  if(type=="bev"){
    #steo from 1 to 0.5
    grant_stepdown_1 <- dplyr::filter(sD, parameter == "bev_grant_phaseout_start")$value
    #step from 0.5 to 0
    grant_stepdown_2 <- dplyr::filter(sD, parameter == "bev_grant_phaseout_end")$value
    f <- stats::stepfun(x=c(grant_stepdown_1, grant_stepdown_2), y = c(1, 0.5,0),f=0)
    return(f(yeartime))  }
  if(type=="phev"){
    #step from 1 to 0.5
    grant_stepdown_1 <- dplyr::filter(sD, parameter == "phev_grant_phaseout_start")$value
    #step from 0.5 to 0
    grant_stepdown_2 <- dplyr::filter(sD, parameter == "phev_grant_phaseout_end")$value
    f <- stats::stepfun(x=c(grant_stepdown_1, grant_stepdown_2), y = c(1, 0.5,0),f=0)
    return(f(yeartime))
  }
}





#' Excise Duty 
#' 
#' historical and future motor fuel excise (in euros)
#'
#' @param type diesel or gasoline
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return excise euros/litre at yeartime
#' @export
#'
#' @examples 
#' excise_duty_fun("gasoline",scenario_B,2030)
excise_duty_fun <- function(type,sD, yeartime) {
  #
  excise_diesel_2030 <-  dplyr::filter(sD, parameter=="excise_diesel_2030")$value
  excise_gasoline_2030 <- dplyr::filter(sD, parameter=="excise_gasoline_2030")$value
  excise_diesel_2050 <- dplyr::filter(sD, parameter=="excise_diesel_2050")$value
  excise_gasoline_2050 <- dplyr::filter(sD, parameter=="excise_gasoline_2050")$value
  
  excise <- dplyr::case_when(type=="diesel"~stats::approx(c(lubridate::decimal_date(rev(fuelprices$date)),2031,2051),c(rev(fuelprices$dieselexcise)/100,excise_diesel_2030,excise_diesel_2050),xout=yeartime,rule=2)$y,
                             type=="gasoline"~stats::approx(c(lubridate::decimal_date(rev(fuelprices$date)),2031,2051),c(rev(fuelprices$petrolexcise)/100,excise_gasoline_2030,excise_gasoline_2050),xout=yeartime,rule=2)$y)
  
  return(excise)

}



#' vrt_rebate_lower
#'
#' The upper rebate window, set to 40,000 euros in 2021.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return euros
#'
#' @export
#'
#' @examples
vrt_rebate_lower <- function(sD,yeartime) {
  #linear phaseout model for grants
  current_rebate_lower <- dplyr::filter(sD, parameter=="lower_rebate_threshold")$value
  lower_phaseout <- dplyr::filter(sD, parameter=="lower_rebate_phaseout_start")$value
  lower_phaseout_end <-  dplyr::filter(sD, parameter=="lower_rebate_phaseout_end")$value
  ifelse(yeartime < lower_phaseout, return(current_rebate_lower),0)
  #return(stats::approx(x=c(lower_phaseout_start,lower_phaseout_end),y=c(current_rebate_lower,0),xout=yeartime,rule=2)$y)
}

#' vrt_rebate_upper
#'
#' The lower rebate window, set to 10,000 euros in 2021. VRT rebate is 50% between vrt_rebate_lower and vrt_rebate_lower + vrt_rebate_upper
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return euros
#'
#' @export
#'
#' @examples
vrt_rebate_upper <- function(sD,yeartime) {
  #linear phaseout model for grants
  current_rebate_upper <- dplyr::filter(sD, parameter=="upper_rebate_threshold")$value
  upper_phaseout <- dplyr::filter(sD, parameter=="upper_rebate_phaseout_start")$value
  upper_phaseout_end <-  dplyr::filter(sD, parameter=="upper_rebate_phaseout_end")$value
  ifelse(yeartime < upper_phaseout, return(current_rebate_upper),0)
  #return(stats::approx(x=c(upper_phaseout_start,upper_phaseout_end),y=c(current_rebate_upper,0),xout=yeartime,rule=2)$y)
}


#' Annual Motor Tax
#'
#' This function looks up tax band in motortaxbands depending on WLTP emissions
#'
#' @param emissions WLTP emissions gCO2/km
#'
#' @return euros
#' @export
#'
#' @examples
motor_tax_old <- function(emissions){

  if(emissions < 0) {warning('negative emissions. Adjusting to zero'); emissions <- 0}
  #ifelse(emission == 0, return(120),
  motortaxbands$motor[cut(emissions/wltp_nedc_ratio,motortaxbands$lower,labels=F)]


}

#' motor_tax
#'
#' WLTP emissions based motor tax post Jan 2021
#'
#' @param emissions WLTP gCO2/km
#' @param flag old or new vrt
#'
#' @return annual amount in euros
#' @export
#'
#' @examples
motor_tax <- function(emissions,flag){

  if(emissions < 0) {warning('negative emissions. Adjusting to zero'); emissions <- 0}
  #ifelse(emission == 0, return(120),
  if(flag !="old") return(new_motortax_bands$motor[cut(emissions,new_motortax_bands$lower,right=F,labels=F)])
  if(flag=="old") return(motortaxbands$motor[cut(emissions/wltp_nedc_ratio,motortaxbands$lower,labels=F)])


}


#' vrt_rate
#'
#' VRT rates based on old/new flag
#'
#' @param emissions WLTP gCO2/km
#' @param flag old or new vrt
#'
#' @return rate
#' @export
#'
#' @examples
vrt_rate <-  function(emissions,flag){
  #
  if(!(flag %in% c("old","2021","new"))) stop("bad flag in vrt")
  if(flag=="old") return(motortaxbands$rate_vrt[cut(emissions/wltp_nedc_ratio,motortaxbands$lower,labels=F)])
  if(flag == "2021") return(vrt_bands_2021$rate[cut(emissions,vrt_bands_2021$lower,right=F,labels=F)])
  if(flag == "new") return(vrt_bands_2022$rate[cut(emissions,vrt_bands_2022$lower,right=F,labels=F)])
}



#' vrt
#'
#' VRT amount paid. At the moment this assumes the old minimum VRT amounts
#'
#' @param price omsp euros
#' @param emissions WLTP emissions gCO2/km
#' @param flag old, 2021, or new vrt
#'
#' @return amount in euros
#' @export
#'
#' @examples
vrt <- function(price,emissions,flag){

  if(!(flag %in% c("new","2021","new"))) stop("bad flag in vrt")
  max(vrt_min(emissions), vrt_rate(emissions,flag)*price)
}



#' vrt_min
#'
#' Floor VRT amount irrespective of OMSP
#'
#' @param emissions gCO2/km (WLTP)
#'
#' @return amount in euros
#' @export
#'
#' @examples
vrt_min <-  function(emissions){
  #
  motortaxbands$min_vrt[cut(emissions/wltp_nedc_ratio,motortaxbands$lower,labels=F)]
}

#' SEAI ZEV grant
#'
#' @param price price inclusive of taxes
#'
#' @return amount in euros
#' @export
#'
#' @examples
zev_grant <- function(price){

  zevgrantbands$grant[cut(price,zevgrantbands$min,labels=F)]


}



#' vrt_rebate
#'
#' post 2020 VRT rebate amount for BEVs based on OMSP
#'
#' @param type vehicle type bev,phev, hev, diesel, petrol
#' @param omsp approximated by budget + dP_0
#' @param params hashed parameter object
#'
#' @return amount in euros
#' @export
#'
#' @examples
vrt_rebate <- function(type,omsp,params){
  
  flag <- flagr(params$yeartime)
  if(type %in% c("diesel","petrol")) return(0)
  if(flag == "2021" & type=="bev"){
  r_lower <- params$rebate_threshold_lower
  r_upper <- params$rebate_threshold_lower + params$rebate_threshold_upper
  if(omsp  <= r_lower) return(vrt_rate(0,flag)*omsp)
  if(omsp > r_lower & omsp <= r_upper) return(vrt_rate(0,flag)*(r_lower + 0.5*(omsp-r_lower)))
  if(omsp >= r_upper) return(vrt_rate(0,flag)*(r_lower + 0.5*(r_upper-r_lower)))
  }
  if(flag == "new" & type=="bev"){
    #reflects budget 2022 same parameters but different interpretation - no relief for vehicle with omsp >= 50000
    r_lower <- params$rebate_threshold_lower
    r_upper <- params$rebate_threshold_lower + params$rebate_threshold_upper
    if(omsp  <= r_lower) return(vrt_rate(0,flag)*omsp)
    if(omsp > r_lower & omsp <= r_upper) return(vrt_rate(0,flag)*r_lower*((r_upper-omsp)/(r_upper-r_lower)))
    if(omsp >= r_upper) return(0)
  }
  if(flag !="old" & type %in% c("hev","phev")) return(0)
  if(flag=="old" & type=="bev"){
    return(min(5000, vrt_rate(0,"old")*omsp))
  }
  if(flag=="old" & type=="phev"){
    #assume all phevs have wltp emissions below 100
    return(min(2500, vrt_rate(0,"old")*omsp))
  }

}

#' vrt_rebate
#' 
#' vrt relief function
#'
#' @param type fuel/powertain
#' @param omsp open maket sale price in euros 
#' @param params current scenario parameters generated from scenario_params 
#' @param wltp gCO2/km
#' @param new_used new or used vehicle "new_car" or "used_car"
#'
#' @return
#' @export
#'
#' @examples
vrt_rebate1 <- function(type,omsp,params,wltp,new_used="new_car"){
  
  if(!(new_used %in% c("used_car","new_car"))) stop("bad new_used variable")
  flag <- flagr(params$yeartime)
  #used cars pre-2021
  if(flag=="old" & new_used=="used_car"){
   res <- dplyr::recode(type,
                                    "diesel"=0,
                                    "petrol"=0,
                                    "hev" = min(1050,vrt_rate(wltp,"old")*omsp),
                                    "phev" = min(1750,vrt_rate(wltp,"old")*omsp),#all phevs have wltp < 100
                                    "bev"= min(3500,vrt_rate(0,"old")*omsp))
  }
  #new cars pre-2021
  if(flag=="old" & new_used=="new_car"){
    res <- dplyr::recode(type,
                         "diesel"=0,
                         "petrol"=0,
                         "hev" = min(1500,vrt_rate(wltp,"old")*omsp),
                         "phev" = min(2500,vrt_rate(wltp/1.21,"old")*omsp),
                         "bev"= min(5000,vrt_rate(0,"old")*omsp))
  }
  #post 2020 cars
  get_bev_rebate <- function(omsp,flag){
    r_lower <- params$rebate_threshold_lower
    r_upper <- params$rebate_threshold_lower + params$rebate_threshold_upper
    if(omsp  <= r_lower) return(vrt_rate(0,flag)*omsp)
    if(flag == "2021"){
      if(omsp > r_lower & omsp <= r_upper) return(vrt_rate(0,"new")*(r_lower + 0.5*(omsp-r_lower)))
      if(omsp >= r_upper) return(vrt_rate(0,flag)*(r_lower + 0.5*(r_upper-r_lower)))
    }
    if(flag == "new"){
      if(omsp > r_lower & omsp <= r_upper) return(vrt_rate(0,flag)*r_lower*((r_upper-omsp)/(r_upper-r_lower)))
      if(omsp >= r_upper) return(0)
    }
    
  }
  
  if(flag != "old"){
    res <- dplyr::recode(type,
                         "diesel"=0,
                         "petrol"=0,
                         "hev" = 0,
                         "phev" = 0,
                         "bev"= get_bev_rebate(omsp,flag)
    )
  }
  
  return(res)
}


#' incentives_fun
#'
#' total subsidy (SEAI grant + VRT rebate) 
#'
#' @param type fuel/powertrain
#' @param p_1 unsubsidised price (including VAT and VRT)
#' @param params hashed parameter values
#' @param wltp gCO2/km
#' @param new_used new or used vehicle, values "new_car" or "used_car"
#'
#' @return incentive in euros
#' @export
#'
#' @examples
incentives_fun <- function(type,p_1,params,wltp,new_used){
  #incentive
  dplyr::recode(type,
                diesel = 0,
                petrol= 0,
                hev = vrt_rebate1("hev",0.9*p_1,params,wltp,new_used),
                phev = ifelse(new_used=="new_car",params$phev_grantpath*zev_grant(p_1),0) + vrt_rebate1("phev",0.9*p_1,params,wltp,new_used),
                bev = ifelse(new_used=="new_car",params$bev_grantpath*zev_grant(p_1),0) + vrt_rebate1("bev",0.9*p_1,params,wltp,new_used)
  )
  
}


#' rrp_full
#' 
#' full list price of a bev before incentives
#'
#' @param rrp_incentive list price (euros) of bev including incentive (grant + vrt rebate)
#' @param params hashed parameters
#'
#' @return price (euros)
#' @export
#'
#' @examples
bev_rrp_full <- function(rrp_incentive,params){
  
  f <- function(r) {r -zev_grant(r) -vrt_rebate("bev",0.9*r,params)-rrp_incentive}
  return(uniroot(f, interval=c(rrp_incentive*0.8,2*rrp_incentive))$root)
  
}


