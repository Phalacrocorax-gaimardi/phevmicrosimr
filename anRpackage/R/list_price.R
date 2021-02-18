list_price <-
function(type,p1,params,flag){
  #incentive
  recode(type,
         diesel = p1,
         petrol= p1,
         hev = p1,
         phev = p1 - params$phev_grantpath*zev_grant(p1) - vrt_rebate("phev",0.9*p1,params,flag),
         bev = p1 - params$bev_grantpath*zev_grant(p1)-vrt_rebate("bev",0.9*p1,params,flag)
  )
  
}
