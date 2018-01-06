testfu <- function(a,b,fun){
      return(fun(a,b))
}
x<- 2
y<- 5

testfu(x,y,meine_lin_fu)


meine_lin_fu <- function(c,d){
   return (c+3*d)
}
