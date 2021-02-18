model_choice <-
function(tech,seg,ntype=2){
  #car selection
  #ntype is the max number of each type in selection
  cars <- filter(tech, segment == seg)  %>% group_by(type) %>% sample_n(ntype,replace=T) %>% distinct()
  return(cars)
}
