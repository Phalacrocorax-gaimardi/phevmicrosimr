incentives <-
function(type,p1,params,flag){
  #incentive
  recode(type,
         diesel = 0,
         petrol= 0,
         hev = 0,
         phev = params$phev_grantpath*zev_grant(p1) + vrt_rebate("phev",0.9*p1,params,flag),
         bev = params$bev_grantpath*zev_grant(p1) + vrt_rebate("bev",0.9*p1,params,flag)
  )
  
}
