# Monotone Aenderung finden

monotonie_mark <- function(xdata){                         
    xdata <- xdata %>%  
      mutate(gr     = (lag(ladezustand) >  ladezustand),         # Abfall  gegenueber Vorgaenger
            ls      = (lag(ladezustand) <  ladezustand),          # Anstieg      ..        ..
            mono    =  as.integer( ifelse(gr,  -1, 0)),           # alle Abfaelle mit -1 markieren, sonst Null
            mono    =  as.integer( ifelse(ls,   1, mono)))        # alle Anstiege mit  1, Rest erhalten 
    return(xdata)                                                 # gibt in mono 1 für Anstieg und -1 für Abfall zurueck, sonst 0
}

# kurze_luecke_fuellen <- function(xdata,l){                        # Wirkt auaf Variable mono
#     laenge <- max(xdata$ct)
#     for (i in 1:laenge){
#              
#     }
# }
# 
# monotonie_ladezustand <- function(xdata){
#   
#   xdata <- xdata %>%
#     mutate(duo     =  mono) #für die andere Version aufheben
#   
#   xdata[is.na(xdata)] <- 0
#   
#   cnull <- nullen_zaehlen_kurz(xdata)
#   
#   temp2 <- xdata %>%
#       mutate(tmp = abs(mono*ct)) %>% 
#       filter(tmp!=0)
#   erste_nicht_null <- min(temp2$tmp)
#   letzte_nullen    <- max(xdata$ct) - max(temp2$tmp) - 
#   rm(temp2)
#   
#   # Nullen nach vorne ausfüllen -----------------------!!!!!!!!!!!!!!!!!!!!!!
#   while(cnull > erste_nicht_null){      # hier muss noch BRemse vom Ende her eingebaut werden
#     xdata <- xdata %>%
#       mutate(prev = lag(mono),
#              change_to_zero      = (prev != 0) & (mono == 0),    # is das nicht gerade irrefuehrend
#              mono = ifelse(change_to_zero, prev, mono)
#             )
#     xdata[is.na(xdata)] <- 0
#     cnull <- nullen_zaehlen_kurz(xdata)
#   }
#   # Nullen nach hinten ausfüllen -----------------------!!!!!!!!!!!!!!!!!!!!!!
#   cnull <- sum(xdata$duo == 0,na.rm = TRUE)  
#   while(cnull > erste_nicht_null){      # hier muss noch BRemse vom Ende her eingebaut werden
#       xdata <- xdata %>%
#          mutate(forw = lead(duo),
#                 change_to_zero_backw = (forw != 0) & (duo  == 0),
#                 duo  = ifelse(change_to_zero_backw, forw, duo),
#                )
#       xdata[is.na(xdata)] <- 0
#       cnull <- sum(xdata$duo == 0,na.rm = TRUE)
#   }
#   
#   xdata    <- xdata %>% select(-one_of(c("prev", "forw", "change_to_zero","change_to_zero_backw", "gr","ls")))
#   return(xdata)
# }  
# 
# #----------------------------------------------------------------
# 
# nullen_zaehlen_kurz <- function(xdata){
#   return(sum(xdata$mono == 0,na.rm = TRUE))
# } 
# 
# #-----------------------------------------------------------------


