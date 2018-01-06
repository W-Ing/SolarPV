library(tidyverse)
myfunc <- function(x,y){return(x+y^2)}
myfunc(2,3)
mydata <- as_tibble(matrix(1:50, ncol=5))

new <- mydata %>% 
  mutate(erg = myfunc(V1,V2))
