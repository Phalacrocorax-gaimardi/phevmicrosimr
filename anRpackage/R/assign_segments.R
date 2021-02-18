assign_segments <-
function()



tech <- tech %>% mutate(p_2 = p_1-incentives(type,p_1,params,"new"))
