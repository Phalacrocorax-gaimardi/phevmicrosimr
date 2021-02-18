current_tech <-
function(params, yeartime){
  
  flag0 <- ifelse(yeartime >= 2021, "new","old")
  fleet1 <- fleet %>% select(c(-motor,-vrt,-`2021_rrp`,-`2021_rrp*`,-`comment`,-`kWh/100km`)) 
  fleet1 <- fleet1 %>% filter(yeartime > model_start & yeartime < model_end)
  tech <- fleet1 %>% rowwise() %>% mutate(p_0=tech_cost_fun(type=type,tech_cost=tech_cost_2021,kWh=kWh,2030) )
  tech <- tech %>% mutate(p_1=p_0*(1+params$car_vat)/(1-0.9*vrt_rate(WLTP,flag0)))
  tech <- tech %>% mutate(p_2 = p_1-incentives(type,p_1,params,"new"))
  return(select(tech,make,model,segment,type,WLTP,kWh,AER,p_0,p_1,p_2))
  
}
