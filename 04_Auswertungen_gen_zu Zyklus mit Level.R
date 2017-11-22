# Schnitt mit horiz Funktion
#
# benotigt gewuenschte Schnitthoehe level
#
# greift auf Zaehler ct zurueck

cat("04_Auswertungen_gen_zu Zyklus mit Level ", level , "\n")

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
    mutate(lpd      = cumsum(s))
  
  xdata <- xdata %>% 
    ungroup() %>% 
    group_by(levelpd) %>% 
    mutate(med = median(ct)) %>% 
    mutate(bd = (ct < med) & pe ) %>% 
    mutate(bu = (ct > med) & pe ) %>%
    mutate(be = (ct == med) & pe ) %>% 
    mutate(newlevelpd = ifelse(bd,levelpd-1,levelpd)) %>% 
    mutate(verynewlevelpd = ifelse(bu,newlevelpd+1,newlevelpd)) %>% 
    ungroup()
  
  loesche <- c("med", "bd", "bu", "be","newlevelpd", "pe", "pu", "pd", "se", "su", "sd", "s")
  xdata <- xdata %>% 
    select(-one_of(loesche)) 
  
  xdata <- xdata %>% 
    ungroup() %>% 
    mutate(levelpd = verynewlevelpd) %>% 
    select(-one_of("verynewlevelpd")) %>% 
    ungroup()
  
  xdata <- xdata %>% 
    mutate(EINS = 1) %>% 
    ungroup() %>% 
    group_by(levelpd) %>% 
    mutate(len_levelpd = sum(EINS)) %>% 
    select(-one_of("EINS")) %>% 
    ungroup()
  
  return(xdata)
  
  
}




# Neues durchnummerieren der levelpd derzeit falsch
# data <- data %>% 
#   mutate( 
#      jump = as.integer(levelpd > lag(levelpd)))
# 
# data[is.na(data)] <- 0
     
# data <- data %>% 
#    mutate(newlevelpd = cumsum(jump))
# data <- data %>% 
#   mutate(levelpd = newlevelpd)  
# 
# data <- data %>% 
#   ungroup() %>% 
#   select(-one_of("newlevelpd"))


# Bem Battladung löst auf 1W in 5min INtervallen also 1/12 Wh
# der BAtteriezzustand löst nur 100 Wh auf == 1% der Kap


