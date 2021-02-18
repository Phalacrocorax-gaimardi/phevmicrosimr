car_choice <-
function(choice,type){
  
  car_choices <- filter(fleet, segment==choice)
  n_seg <- dim(car_choices)[1]
  samp <- sample(1:n_seg,1,prob=car_choices$number)
  return(paste(car_choices[samp,]$make,car_choices[samp,]$model))
}
