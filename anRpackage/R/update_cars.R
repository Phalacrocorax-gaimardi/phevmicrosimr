update_cars <-
function(b_s,params,flag){
    #compute full utilities
    choices <- vector("list",dim(b_s)[1])
    for(i in 1:dim(b_s)[1]){
      ag <- b_s[i,]
      models <- bind_cols(select(ag,ID,w_econ,w_social,w_enviro,w_theta,qev32,q17a_2,qev29,qev34,qev31),model_choice(tech,ag$segment))
      models <- models %>% rowwise() %>% mutate(u_econ=-beta.*(0.4*p_2/3 + fuelcost_fun(type,WLTP,kWh,AER,params)*mileage_vals[qev34] + motor_tax(WLTP,flag))/budget_vals[qev32])
      models <- models %>% rowwise() %>% mutate(u_social=ifelse(type=="bev", dU_social[min(qev29,3)],0))
      models <- models %>% rowwise() %>% mutate(u_enviro=ifelse(type=="bev", w_enviro*dU_enviro[q17a_2],0))
      models <- models %>% rowwise() %>% mutate(theta = ifelse(type=="bev",-w_theta*(theta.-lambda.),0))
      models <- models[,11:24] %>% mutate(u=u_econ+u_social+u_enviro+theta)
      #add hedonic term that penalises cars cheaper than budget?
      #add range anxiety than penalised low range bevs for higher mileage drivers?
      choices[[i]] <- models[which.max(models$u),]
    }
    choices <- bind_rows(choices)
    return(bind_cols(select(b_s, ID,w_econ,w_social,w_enviro,w_distance,w_theta, qev32,qev29,q17a_2,qev34,qev31,transaction),choices[,1:10]) )                                        
  }
