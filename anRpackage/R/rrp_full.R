rrp_full <-
function(rrp_incentive,params){
  
  f <- function(r) {r -zevgrantbands(r) -vrt_new_rebate(0.9*r,params)-rrp_incentive}
  
}
