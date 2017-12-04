library(tidyverse)
# library(tibbletime)
# library(lubridate)
# library(reshape2)
#require(stringr)

myfunc <- function(par){
   result <- ifelse(par=="JA", 1, 0 )
   return(result)
}
myfunc("N")

# get first observation for each Species in iris data -- base R
mini_iris <- iris[c(1, 51, 101), ]
# gather Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
my_iris <- gather(mini_iris, key = flower_att, value = measurement,
       Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
# same result but less verbose
gather(mini_iris, key = flower_att, value = measurement, -Species)