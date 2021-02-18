battery_cost_function <-
function(sD,yeartime,nlag = 4.5){
  #cost of EV vs cost
  #2020 premium
  year_100<- dplyr::filter(sD,parameter=="year_100")$value
  alpha<- dplyr::filter(sD,parameter=="learning_exponent")$value
  b1 <-dplyr::filter(sD,parameter=="cost_floor")$value/2
  b0 <- filter(bnefprices, year==2019)$pack_ekWh-b1
  if(yeartime <= 2019.5 + nlag) return(stats::approx(x=bnefprices$year+0.5,y=bnefprices$pack_ekWh, xout=yeartime-nlag)$y)
  #build learning curve from 
  if(yeartime >= 2019.5 + nlag) return(max(b1,b0*exp(-0.2*(yeartime-(2019.5+nlag))) + b1 ))
}
