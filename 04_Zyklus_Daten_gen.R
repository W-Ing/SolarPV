#
# Schnitt mit horiz Funktion
#
#  greift auf Zaehler ct zurueck

#cat("04_Auswertungen_gen_zu Zyklus mit Level " "\n")

zyklus_daten_gen <- function(xdata, l){     # xdata ist der - Datenfile Typ tibble
                                            # l der zu bearbeitende Level
  
  xdata <- xdata %>% 
    mutate(pe           = (ladezustand == l),       # e =equal d =down u = up
           pu           = (ladezustand  > l),
           pd           = (ladezustand  < l),
           se           = (lag(pe) != pe),
           su           = (lag(pu) != pu),
           sd           = (lag(pd) != pd),
           s            = as.integer (se | su | sd))
  
  xdata[is.na(xdata)] <- 0
  
  xdata <- xdata %>% 
    ungroup() %>% 
    mutate(zyklus      = cumsum(s))
  
  xdata <- xdata %>% 
    ungroup() %>% 
    group_by(zyklus) %>% 
    mutate(med = median(ct)) %>% 
    mutate(bd = (ct < med) & pe ) %>% 
    mutate(bu = (ct > med) & pe ) %>%
    mutate(be = (ct == med) & pe ) %>% 
    mutate(newzyklus = ifelse(bd,zyklus-1,zyklus)) %>% 
    mutate(verynewzyklus = ifelse(bu,newzyklus+1,newzyklus)) %>% 
    ungroup()
  
  loesche <- c("med", "bd", "bu", "be","newzyklus", "pe", "pu", "pd", "se", "su", "sd", "s")
  xdata <- xdata %>% 
    select(-one_of(loesche)) 
  
  xdata <- xdata %>% 
    ungroup() %>% 
    mutate(zyklus = verynewzyklus) %>% 
    select(-one_of("verynewzyklus")) %>% 
    ungroup()
  
  xdata <- xdata %>% 
    mutate(EINS = 1) %>% 
    ungroup() %>% 
    group_by(zyklus) %>% 
    mutate(len_zyklus = sum(EINS)) %>% 
    select(-one_of("EINS")) %>% 
    ungroup()
  
  return(xdata)
  
  
}




# Neues durchnummerieren der zyklus derzeit falsch
# data <- data %>% 
#   mutate( 
#      jump = as.integer(zyklus > lag(zyklus)))
# 
# data[is.na(data)] <- 0
     
# data <- data %>% 
#    mutate(newzyklus = cumsum(jump))
# data <- data %>% 
#   mutate(zyklus = newzyklus)  
# 
# data <- data %>% 
#   ungroup() %>% 
#   select(-one_of("newzyklus"))


# Bem Battladung löst auf 1W in 5min INtervallen also 1/12 Wh
# der BAtteriezzustand löst nur 100 Wh auf == 1% der Kap


