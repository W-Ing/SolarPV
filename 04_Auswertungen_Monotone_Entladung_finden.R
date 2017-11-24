# Monotone Abnahme finden

monoton_fall <- function(xdata){
      xdata <-   xdata %>%  mutate(sd    = ifelse(lead(ladezustand) <= ladezustand, 1, 0))
      xdata[is.na(xdata)] <- 0
      xdata <-   xdata %>%  mutate(sj    = ifelse(sd == 1 &lag(sd) == 0, 1, 0))  
      xdata[is.na(xdata)] <- 0
      xdata <-   xdata %>%  mutate(cpm   = cumsum(sd*sj)*sd)
      return(xdata)
}

