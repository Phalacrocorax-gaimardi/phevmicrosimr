#' BEV Adoption Rates
#'
#' Converts ABM output to EV adoption rates by simulation run
#'
#' @param abm agent dataframe by simulation run and date
#'
#' @return dataframe columns simulation (run ID) - date - predicted uptake fraction
#' @export
#'
#' @examples
getEVAdoption1 <- function(abm){
  #returns the EV adoption rate across runs
  adoption <- abm[[1]] %>% dplyr::group_by(simulation,t,type) %>% dplyr::summarise(n=dplyr::n())
  Nrun <- dplyr::filter(abm[[3]],parameter=="Nrun")$value
  Nt <- length(unique(adoption$t))
  test <- expand.grid(1:Nrun,1:Nt,type=c("diesel","petrol","hev","phev","bev")) %>% tibble::as_tibble()
  names(test) <- c("simulation","t","type")
  test$type <- as.character(test$type)
  adoption <- dplyr::left_join(test,adoption)
  adoption <- adoption %>% dplyr::mutate(n=tidyr::replace_na(n,0))
  y_zero <- lubridate::ymd(paste(year_zero,1,1))
  adoption$date <- y_zero+months(adoption$t-1)

  #adoption <- adoption %>% group_by(simulation,fuel) %>% mutate(dn=n-lag(n))
  adoption <- adoption %>% group_by(type) %>% dplyr::mutate(predicted=n/924)

  return(adoption[,c(1,3,5,6)])
}

#' vkt_factor
#' 
#' a simple mileage reduction factor based on fuel price and demand reduction measures
#'
#' @param price_elasticity travel demand elasticity
#' @param price prevailing price future fuel price
#' @param demand_reduction prevailing demand_reduction demand reduction due to non-price measures - active travel, congestion charging etc e.g. 0.95
#'
#' @return vkt factor 0-1
#' @export
#'
#' @examples
vkt_factor_fun <- function(price_elasticity,price,demand_reduction){
  #fuelprices %>% group_by(year=year(date)) %>% summarise(price=mean(diesel+petrol)/2) 2018 mean price
  return((price/1.38)^price_elasticity*(1-demand_reduction))
  
}


#' getEmissions
#' 
#' Monthly and cumulative emissions for a set of model runs
#'
#' @param abm output of runABM
#' @param addvars vector of additional variables  
#' @param xi phev behavioural parameter xi=1 is optimal charging behaviour
#' @param epsilon kilometer fuel price elasticity for ICEV drivers (typically -0.15)
#' @param average_over_runs if FALSE return individual run emissions, otherwise average over runs
#' @param emissions_start_date start date for cumulative emissions calculation
#'
#' @return a dataframe 
#' @export
#'
#' @examples
getEmissions <- function(abm, addvars=NULL, xi=1,epsilon = 0,average_over_runs=F,emissions_start_date=NULL){
  #returns the EV adoption rate across runs

  year_zero <- 2015
  y_zero <- lubridate::ymd(paste(year_zero,1,1))
  end_year <-  dplyr::filter(abm[["system"]],parameter=="end_year")$value
  Nt <- (end_year+1-year_zero)*12
  date_tab <- tibble::tibble(t=1:Nt)
  date_tab <- date_tab %>% dplyr::mutate(date=y_zero+months(t-1))
  date_tab <- date_tab %>% dplyr::mutate(vkt_fact=1-vkt_reduction_fun(abm[[2]],lubridate::decimal_date(date)))
  date_tab <- date_tab %>% dplyr::mutate(diesel=fuel_price_fun1("diesel",abm[[2]],lubridate::decimal_date(date)),petrol=fuel_price_fun1("gasoline",abm[[2]],lubridate::decimal_date(date)))
  date_tab <- date_tab %>% dplyr::mutate(vkt_factor_diesel=vkt_factor_fun(epsilon,diesel,vkt_reduction_fun(abm[[2]],lubridate::decimal_date(date))))
  date_tab <- date_tab %>% dplyr::mutate(vkt_factor_petrol=vkt_factor_fun(epsilon,petrol,vkt_reduction_fun(abm[[2]],lubridate::decimal_date(date))))
  abm.e <- dplyr::inner_join(abm[[1]],date_tab,by="t") %>% dplyr::select(ID,date,simulation,type,AER,qev34,diesel,petrol,vkt_fact,vkt_factor_diesel,vkt_factor_petrol,wltp)
  #real-world emissions
  abm.e <- abm.e %>% dplyr::arrange(simulation,date,ID)
  abm.e <- abm.e %>% dplyr::mutate(mileage=mileage_vals[qev34]) 
  abm.e <- abm.e %>% dplyr::mutate(e=wltp)  #add adustment?
  abm.e <- abm.e %>% dplyr::filter(type != "bev") #no need to include bevs
  #if(epsilon != 0){
  #abm.e <- abm.e %>% dplyr::rowwise() %>% dplyr::mutate(mileage=replace(mileage,type %in% c("diesel"),mileage*vkt_factor_diesel))
  abm.d <- abm.e %>% dplyr::filter(type=="diesel") %>% dplyr::mutate(mileage=mileage*vkt_factor_diesel)
  abm.h <- abm.e %>% dplyr::filter(type %in% c("petrol","hev"))  %>% dplyr::mutate(mileage=mileage*vkt_factor_petrol)
  #abm.e <- abm.e %>% dplyr::rowwise() %>% dplyr::mutate(mileage=replace(mileage,type %in% c("petrol","hev"),mileage*vkt_factor_petrol))
  #assume that phev driver mileages reflect travel demand reduction measures but not price elasticity
    #}
  abm.p <- abm.e  %>% dplyr::filter(type=="phev") 
  #the order here is important
  abm.p <- abm.p %>% dplyr::rowwise() %>% dplyr::mutate(mileage=mileage*vkt_fact)
  abm.p <- abm.p  %>% dplyr::rowwise() %>% dplyr::mutate(e=e_phev(wltp,AER,mileage,xi))
  abm.e <- dplyr::bind_rows(abm.d,abm.h,abm.p) %>% dplyr::arrange(simulation,date,ID)
  #abm.e <- abm.e %>% rowwise() %>% dplyr::mutate(e=replace(e,type=="phev",e_phev(WLTP,AER,mileage_vals[qev34],xi)))
  abm.e <- abm.e %>% dplyr::group_by_at(c("date","simulation",addvars)) %>% dplyr::summarise(wltp_mean=mean(wltp) ,e_real=mean(e), tCO2=sum(e*mileage)/1e+6/924)
  abm.e <- dplyr::distinct(abm.e)
  #abm.e <- inner_join(abm.e,date_tab,by="") %>% ungroup() %>% select("simulation","date",addvars,"WLTP_mean","e_real","tCO2")
  abm.e <- abm.e %>% dplyr::ungroup() %>% dplyr::select("simulation","date",all_of(addvars),"wltp_mean","e_real","tCO2")

  #average over runs
  if(average_over_runs){
   abm.e <- abm.e %>% dplyr::group_by_at(c("date",addvars)) %>% dplyr:summarise(e_real=mean(e_real),tCO2=mean(tCO2))
   abm.e <- abm.e %>% dplyr::group_by_at(addvars) %>% dplyr::mutate(cumulative_tCO2=cumsum(tCO2))
   if(!is.null(emissions_start_date)){
    abm0 <- dplyr::filter(abm.e, date==emissions_start_date) %>% dplyr::select(type,cumulative_tCO2)
    names(abm0)[2] <- "offset"
    abm.e <- dplyr::inner_join(abm.e,abm0,by="type")
    abm.e <- abm.e %>% dplyr::filter(date >= emissions_start_date) %>% dplyr::group_by_at(addvars) %>% dplyr::mutate(cumulative_tCO2=cumulative_tCO2-offset)
    abm.e <- abm.e %>% dplyr::select(-offset)
    }
  }
  return(abm.e)
}



#' get fleet age
#'
#' @param abm output of runABM
#' @param addvars additional variable (type,segment, qev31 etc)
#'
#' @return
#' @export
#'
#' @examples
getFleetAge <- function(abm,addvars=NULL){
  #returns the fleet age 
  vars <- c("simulation","t",addvars)
  regs <- abm[[1]] %>% dplyr::group_by_at(.vars=vars) %>% dplyr::summarise(reg=mean(reg))
  Nrun <- length(unique(regs$simulation))
  Nt <- length(unique(regs$t))
  #test <- expand.grid(1:Nrun,1:Nt,type=c("diesel","petrol","hev","phev","bev")) %>% tibble::as_tibble()
  #names(test) <- c("simulation","t","type")
  #test$type <- as.character(test$type)
  #regs <- dplyr::left_join(test,regs)
  #adoption <- adoption %>% dplyr::mutate(reg=tidyr::replace_na(reg,0))
  y_zero <- lubridate::ymd(paste(year_zero,1,1))
  regs$date <- y_zero+months(regs$t-1)
  regs <- regs %>% ungroup() %>% group_by_at(.vars=c("date",addvars)) %>% dplyr::summarise(mean_age=decimal_date(date)-mean(reg))
  #adoption <- adoption %>% group_by(simulation,fuel) %>% mutate(dn=n-lag(n))
  #regs <- regs %>% group_by(date,type) %>% dplyr::mutate(mean_reg=mean(reg))
  return(regs %>% select(date,addvars,mean_age))
}




#' Graph Model Uptake
#'
#' Make a graph including historical data from bev_historical_uptake
#'
#' @param abm output from runABM
#' @param yscale y-scale "sqrt" or "linear"
#' @param gtitle graph title
#' @param galpha alpha transparency for graph
#' @param fleet_size assumed constant motor fleet number in thousands
#'
#' @return ggplot object
#' @export
#'
#' @examples
graph_adoption <- function(abm, yscale="sqrt", gtitle="EV number",galpha=0.4, fleet_size = 2.2e+3){
 #
  adoption1 <- getEVAdoption1(abm)
  adoption2 <- adoption1 %>% dplyr::group_by(type,date) %>% dplyr::summarise(predicted=mean(predicted))
  meanadopt <- dplyr::filter(adoption2,date=="2030-12-01",type=="bev")$predicted
  Nrun <- dplyr::filter(abm[[3]],parameter=="Nrun")$value
 g <- adoption1 %>% dplyr::filter(lubridate::year(date) <= 2050, lubridate::year(date) >= year_zero ) %>% ggplot2::ggplot()
 g <- g + ggplot2::geom_line(ggplot2::aes(date,predicted*fleet_size,colour=type,group=interaction(simulation,type)),size=0.1,alpha=galpha)
 if(yscale=="sqrt") g <- g + ggplot2::scale_y_continuous(limits=c(0,fleet_size), trans="sqrt",breaks=c(10,100,400,1000,2000))
 if(yscale=="linear") g <- g
 g <- g + ggthemes::theme_tufte()
 g <- g + ggplot2::labs(x="", y="Thousands of EVs",title=gtitle)
 g <- g + ggplot2::geom_hline(yintercept = dplyr::filter(adoption2, date=="2030-12-01",type=="bev")$predicted*fleet_size,linetype="dotted")
 g <- g + ggplot2::geom_vline(xintercept = lubridate::ymd("20301201"),linetype="dotted")
 g <- g + ggplot2::theme(legend.position = "None")
 g <- g + ggplot2::geom_point(data=bev_historical_uptake %>% dplyr::filter(lubridate::year(date) >= 2010),ggplot2::aes(date,fleet_size*penetration/100),shape=4,size=2,colour="grey40")
 #g <- g + ggplot2::geom_line(data=adoption2, ggplot2::aes(date,predicted*fleet_size), colour="grey50")
 g
}



#' scenario_fulldata
#'
#' scenario developed as a function of date for plotting
#'
#' @param sD scenario dataframe
#'
#' @return parameter set
#' @export
#'
#' @examples
scenario_fulldata <- function(sD){

  Nt <- (end_year-year_zero+1)*12
  y_zero <- lubridate::ymd(paste(year_zero,1,1))
  scen <- list("vector",Nt)
  for(t in 1:Nt){
    df <- scenario_params_df(sD,yeartime = year_zero+(t-1)/12)
    df$date <- y_zero+months(t-1)
    scen[[t]] <- df
  }
  scen <- tibble::as_tibble(data.table::rbindlist(scen))
  return(scen)

}

#' getAdoption
#'
#' @param abm output ofrunABM
#' @param addvars additional grouping variable (default is type only)
#'
#' @return uptake dataframe
#' @export
#'
#' @examples
getAdoption <- function(abm, addvars=NULL){
  #returns the EV adoption rate across runs
  
  vars <- c("simulation","t","type",addvars)
  adoption <- abm[[1]] %>% dplyr::group_by_at(.vars=vars) %>% dplyr::summarise(n=dplyr::n())
  Nrun <- length(unique(adoption$simulation))
  Nt <- length(unique(adoption$t))
  test <- expand.grid(1:Nrun,1:Nt,type=c("diesel","petrol","hev","phev","bev")) %>% tibble::as_tibble()
  names(test) <- c("simulation","t","type")
  test$type <- as.character(test$type)
  adoption <- dplyr::left_join(test,adoption)
  adoption <- adoption %>% dplyr::mutate(n=tidyr::replace_na(n,0))
  y_zero <- lubridate::ymd(paste(year_zero,1,1))
  adoption$date <- y_zero+months(adoption$t-1)
  
  #adoption <- adoption %>% group_by(simulation,fuel) %>% mutate(dn=n-lag(n))
  adoption <- adoption %>% group_by_at(c("type",addvars)) %>% dplyr::mutate(predicted=n/924)
  vars <- vars %>% replace(vars=="t","date")
  return(dplyr::select(adoption, c(vars,"predicted")))
}



#' getIncentiveCost
#'
#' @param abm simulation runs (output of runABM)
#'
#' @return incentive cost (difference between after tax price and incentivised price)
#' @export
#'
#' @examples
getIncentiveCost <- function(abm){
  
  y_zero <- lubridate::ymd(paste(year_zero,1,1))
  grants <- abm[[1]] %>% dplyr::filter(transaction, type %in% c("hev","phev","bev")) %>% dplyr::group_by(simulation, date=y_zero+months(t-1),type) %>% dplyr::summarise(`incentive cost`=sum(p_1-p_2))
  Nrun <- dplyr::filter(abm[[3]],parameter=="Nrun")$value
  Nt <- (dplyr::filter(abm[[3]],parameter=="end_year")$value + 1 - 2015)*12
  test <- expand.grid(1:Nrun,y_zero+months(1:Nt-1),type=c("hev","phev","bev")) %>% tibble::as_tibble()
  names(test) <- c("simulation","date","type")
  test$type <- as.character(test$type)
  grants <- dplyr::left_join(test,grants)
  grants <- tidyr::replace_na(grants,list(`incentive cost`=0))
  return(grants)
  
}


#' getZEVRange
#'
#' @param abm output of runABM
#'
#' @return datafame of date and annual mean AER 
#' @export
#'
#' @examples
getZEVRange <- function(abm){
  
  y_zero <- lubridate::ymd(paste(year_zero,1,1))
  range <- abm[[1]] %>% dplyr::filter(transaction,type %in% c("phev","bev")) %>% dplyr::group_by(simulation, year=lubridate::year(y_zero+months(t-1)),type) %>% dplyr::summarise(`mean range`=mean(AER))
  return(range)
  
}

#' GetPricesPaid
#' 
#' monthly transaction prices
#'
#' @param abm output of runABM
#' @param addvars grouping variables in addition to segment, date e.g. segment, type, qev31. If NULL all prices
#'
#' @return dataframe of prices paid
#' @export
#'
#' @examples
getPricesPaid <- function(abm, addvars=NULL){
  
  y_zero <- lubridate::ymd(paste(year_zero,1,1))
  prices <- abm[[1]] %>% dplyr::filter(transaction)
  prices <- prices %>% dplyr::mutate(date=y_zero+months(t-1))
  prices <- prices %>% dplyr::group_by_at(c("simulation","date",addvars)) %>% dplyr::summarise(mean_price=mean(p_2))
  #prices <- tidyr::replace_na(prices,list(mean_price=0))
  return(prices)
  
}



#' getMileages
#' 
#' mileage by fuel type and other variables if requires
#'
#' @param abm output of runABM
#'
#' @return datafame of date, type and mean annual mileage 
#' @export
#'
#' @examples
getMileages <- function(abm){
  
  y_zero <- lubridate::ymd(paste(year_zero,1,1))
  kms <- abm[[1]] %>% dplyr::group_by(year=lubridate::year(y_zero+months(t-1)),type) %>% dplyr::summarise(`mean mileage`=mean(mileage_vals[qev34]))
  return(kms)
  
}



#' getZEVSales
#' 
#' annual ZEV sales market share by
#'
#' @param abm output of runABM
#'
#' @return sales dataframe (year, sales fraction)
#' @export
#'
#' @examples
getZEVSales <- function(abm){
  #
  df <- tidyr::expand_grid(qev31=c("new","used"),type=c("bev","phev"),year=2015:2050)
  Nrun <- dplyr::filter(abm[[3]],parameter=="Nrun")$value
  sales <- abm[[1]] %>% dplyr::filter(transaction) %>% dplyr::mutate(qev31 = ifelse(qev31==1,"new","used"))
  sales0 <-  sales %>% dplyr::group_by(qev31,year=lubridate::year(y_zero+months(t-1))) %>% dplyr::summarise(total_sales=dplyr::n()/(Nrun))
  sales <- sales %>% dplyr::group_by(type,qev31,year=lubridate::year(y_zero+months(t-1))) %>% dplyr::summarise(sales=dplyr::n()/(Nrun)) #annual sales by type & new/used
  sales <- dplyr::inner_join(sales,sales0)
  sales <- sales %>% dplyr::rowwise() %>% dplyr::mutate(share=sales/total_sales) %>% dplyr::filter(type %in% c("bev","phev")) %>% dplyr::select(qev31,type,year,share)
  sales <- dplyr::left_join(df,sales) %>% dplyr::mutate(share=tidyr::replace_na(share,0))
  return(sales)
  
}



#' getActivity
#' 
#' annual activity (km) by powertrain type including impact of VKT policy and price elasticity
#'
#' @param abm output of runABM
#' @param epsilon price elasticity
#' @param xi0 phev charging behaviour (one of 0.5,1.0,1.5,2.0)
#'
#' @return dataframe type, year, activity
#' @export
#'
#' @examples
getActivity <- function(abm,epsilon=-0.15,xi0=1.0){

 year_zero <- 2015
 y_zero <- lubridate::ymd(paste(year_zero,1,1))
 Nrun <- dplyr::filter(abm[[3]],parameter=="Nrun")$value
 end_year <-  dplyr::filter(abm[["system"]],parameter=="end_year")$value
 Nt <- (end_year+1-year_zero)*12
 date_tab <- tibble::tibble(t=1:Nt)
 date_tab <- date_tab %>% dplyr::mutate(date=y_zero+months(t-1))
 date_tab <- date_tab %>% dplyr::mutate(vkt_fact=1-vkt_reduction_fun(abm[[2]],lubridate::decimal_date(date)))
 date_tab <- date_tab %>% dplyr::mutate(diesel=fuel_price_fun1("diesel",abm[[2]],lubridate::decimal_date(date)),petrol=fuel_price_fun1("gasoline",abm[[2]],lubridate::decimal_date(date)))
 date_tab <- date_tab %>% dplyr::mutate(vkt_factor_diesel=vkt_factor_fun(epsilon,diesel,vkt_reduction_fun(abm[[2]],lubridate::decimal_date(date))))
 date_tab <- date_tab %>% dplyr::mutate(vkt_factor_petrol=vkt_factor_fun(epsilon,petrol,vkt_reduction_fun(abm[[2]],lubridate::decimal_date(date))))
 abm.e <- dplyr::inner_join(abm[[1]],date_tab,by="t") %>% dplyr::select(ID,date,simulation,type,qev34,diesel,petrol,vkt_fact,vkt_factor_diesel,vkt_factor_petrol,AER)
 abm.e <- abm.e %>% mutate(mileage=dplyr::case_when(type == "bev"~mileage_vals[qev34]*vkt_fact,
                                                    type == "phev"~mileage_vals[qev34]*vkt_fact,
                                                    type == "diesel"~mileage_vals[qev34]*vkt_factor_diesel,
                                                    type == "petrol"~mileage_vals[qev34]*vkt_factor_petrol,
                                                    type == "hev"~mileage_vals[qev34]*vkt_factor_petrol))
 #separate PHEV activity into electric and icev
 abm.p <- abm.e  %>% dplyr::filter(type=="phev") 
 abm.p$type <- "phev_elec"
 #electric miles
 abm.p <- abm.p %>% dplyr::rowwise() %>% dplyr::mutate(mileage=uf_phev_interp(mileage,xi0,AER)*mileage)
 abm.e <- dplyr::bind_rows(abm.e,abm.p) %>% dplyr::arrange(simulation,date,ID)
 activity <- abm.e %>% dplyr::group_by(type,year=lubridate::year(date)) %>% dplyr::summarise(activity=sum(mileage/(12*Nrun)))
 return(activity) 
}




getNOx <- function(abm, CF_petrol=1,CF_diesel=4){
  #NOx emissions assuming Euro6 RTE
  
}

