createTable2 <-
function(survey = survey_ev, q){
  #cross table for n variables
  q1 <- q[!(q %in% non_multiple_choice_question_codes)]
  q2 <- q[(q %in% non_multiple_choice_question_codes)]
  n <- length(q)
  ev <- survey %>% group_by_at(vars(one_of(q))) %>% summarise(n=n()) 
  for(qq in q1)
    ev <- ev %>% inner_join(filter(qanda,code==qq)[,3:4],by=setNames("answercode",qq))# %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  for(qq in q2)
    ev$qq <- "number"# %>% inner_join(filter(qanda,code==q2)[,3:4],by=c(q2="answercode"))
  names(ev)[1:n] <- q
  names(ev)[seq(n+2,2*n+1)] <- toupper(q)
  return(ev)
}
