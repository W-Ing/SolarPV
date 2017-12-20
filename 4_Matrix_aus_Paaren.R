# 4_Matrix_aus_Paaren.R

list_fu <- function(start, ende, delta){
  x<-c(start)
  num <- (ende - start)/delta
  for (i in 1:(num-1))
  {x<-append(x,delta)}
  return(cumsum(x))
}

matrix_bilden <- function(start1,ende1,delta1,start2,ende2,delta2){
   x <- list_fu(start1,ende1,delta1) 
   y <- list_fu(start2,ende2,delta2)   
   prod<-c()
   for (i in x){
     for (j in y){
       prod <- append(prod,c(i,j))
     }
   }
   M <- t(matrix(prod,nrow=2,ncol=length(prod)/2))
   M <- as_tibble(M)
   colnames(M) <- c("eta_1","eta_2")
  
   return(M)
}

