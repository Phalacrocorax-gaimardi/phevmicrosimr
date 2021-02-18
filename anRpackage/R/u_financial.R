u_financial <-
function(seg,budget,mileage,params,flag){
  #financial utilities for new car candidates in segment seg for driver of budget and mileage
  
  models <- model_choice(tech,seg)
  #financial utility
  models <- models %>% mutate(u_econ=beta.*(p_2 + fuelcost_fun(type,WLTP,kWh,AER,params)*mileage + motor_tax(WLTP,flag))/budget)
}
