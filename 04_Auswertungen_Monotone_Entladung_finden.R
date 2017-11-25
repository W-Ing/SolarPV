# Monotone Abnahme finden

monotonie_ladezustand <- function(xdata){
  xdata <- xdata %>%  
    mutate(gr      = (lag(ladezustand) >  ladezustand),
           ls      = (lag(ladezustand) <  ladezustand),
           mono    =  as.integer( ifelse(gr,  -1, 0)),
           mono    =  as.integer( ifelse(ls,   1, mono))) 
  xdata[is.na(xdata)] <- 0
  
  cnull <- nullen_zaehlen_kurz(xdata)
  
  temp2 <- xdata %>%
    mutate(tmp = abs(mono*ct)) %>% 
    filter(tmp!=0)
  erste_nicht_null <- min(temp2$tmp)
  rm(temp2)
  
  while(cnull > erste_nicht_null){
    xdata <- xdata %>%
      mutate(prev = lag(mono),
             change_to_zero = (prev != 0) & (mono == 0), 
             mono = ifelse(change_to_zero, prev, mono))
    xdata[is.na(xdata)] <- 0
    cnull <- nullen_zaehlen_kurz(xdata)}
    xdata    <- xdata %>% select(-one_of(c("prev","change_to_zero","gr","ls")))
  return(xdata)
}  

#----------------------------------------------------------------

nullen_zaehlen_kurz <- function(xdata){
  return(sum(xdata$mono == 0,na.rm = TRUE))
} 
