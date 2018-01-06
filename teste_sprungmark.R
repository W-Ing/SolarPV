vec <- c(6,8,17)
class(vec)
vec[2]
testdata <- list(x=1:20,y=0)
testdata <- as_tibble(testdata)
for (i in 1:length(vec)){
  testdata <- testdata %>% 
    mutate( y= ifelse(vec[i] <= x, vec[i], y))
}
testdata
