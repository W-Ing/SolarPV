list_fu <- function(start, num, delta){
    x<-c(start)
    for (i in 1:(num-1 ) )
         {x<-append(x,delta)}
    return(cumsum(x))
}
x <- list_fu(0.85, 10, 0.01)         
y <- list_fu(0.65, 20, 0.01)   

prod<-c()
for (i in list_fu(0.85, 10, 0.01)){
   for (j in list_fu(0.65, 10, 0.01)){
      prod <- append(prod,c(i,j))
  }
}

M <- matrix(prod,nrow=2,ncol=length(prod)/2)
Mat_EtaPaare <- as.tibble(t(M))

Mat_EtaPaare <- Mat_EtaPaare %>% 
  mutate(fehler = 0  )