test <- matrix(c(0,0,1,2,3,4,5,6), nrow = 4,  ncol = 2)
test <- as.tibble(test)
names(test)[1] <- "sp1"
names(test)[2] <- "sp2"
test <- test %>% 
        mutate(prod = sp1*0.33)
