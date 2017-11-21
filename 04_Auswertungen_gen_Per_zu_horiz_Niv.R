# Schnitt mit horiz Funktion
#
# benotigt gewuenschte Schnitthoehe level
#
# greift auf Zaehler ct zurueck



data <- data %>% 
  mutate(pe           = (ladezustand == level),       # e =equal d =down u = up
         pu           = (ladezustand  > level),
         pd           = (ladezustand  < level),
         se           = (lag(pe) != pe),
         su           = (lag(pu) != pu),
         sd           = (lag(pd) != pd),
         s            = as.integer (se | su | sd))

data[is.na(data)] <- 0

data <- data %>% 
  ungroup() %>% 
  mutate(levelpd      = cumsum(s))

data <- data %>% 
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
data <- data %>% 
  select(-one_of(loesche)) 

data <- data %>% 
  ungroup() %>% 
  mutate(levelpd = verynewlevelpd) %>% 
  select(-one_of("verynewlevelpd")) %>% 
ungroup()

data <- data %>% 
    mutate(EINS = 1) %>% 
    ungroup() %>% 
    group_by(levelpd) %>% 
    mutate(len_levelpd = sum(EINS)) %>% 
    select(-one_of("EINS")) %>% 
    ungroup()


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


