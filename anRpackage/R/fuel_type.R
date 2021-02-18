fuel_type <-
function(i){
  ifelse(!is.na(i),
  recode(i,
         `1`="diesel",
         `2`="petrol",
         `3`="diesel",
         `4`="petrol",
         `5`="petrol",
         `6`="diesel")
  ,return(sample(c("diesel","petrol"),1,prob=c(0.9,0.1))))
}
