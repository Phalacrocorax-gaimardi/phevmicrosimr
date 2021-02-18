choose_initial_car <-
function(seg){
    
    test <- initial_fleet %>% filter(segment==seg)
    return(test[sample(1:dim(test)[1],1,prob = test$number),2:3])
    
  }
