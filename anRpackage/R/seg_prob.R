seg_prob <-
function(qc1,qk,qev26,qev25,qev28){
  
  #vector of probabilities for car_segments
  #rurality, household,use,daily mileage, parking
  p <- sapply(car_segments, function(seg) filter(p0,segment==seg)$prob*(filter(p1,segment==seg)[1,qc1+1]
                                                                        *filter(p2,segment==seg)[1,qk+1]
                                                                        *ifelse(!is.na(qev26),filter(p3,segment==seg)[1,qev26+1],1)
                                                                        *ifelse(!is.na(qev25),filter(p4,segment==seg)[1,qev25+1],1)
                                                                        *ifelse(!is.na(qev28),filter(p5,segment==seg)[1,qev28+1],1)
                                                                        ))
  p <- p/(1e-6+sum(p))
  p <- tibble(seg=car_segments,prob=p)  #return(pivot_wider(p,names_from="seg"))
  return(p)
}
