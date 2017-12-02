library(tidyverse)
library(tibbletime)
library(lubridate)
library(reshape2)
#require(stringr)

data <- tibble( x =1:26, y = letters[])

option = c("x")

data <- data %>% 
   mutate(z = one_of(option))
data
