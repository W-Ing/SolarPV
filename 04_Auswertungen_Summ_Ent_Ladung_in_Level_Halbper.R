# #
# # Voraussetzungen daypd und levelpd schon gebildet
# # len_levelpd schon berechnet
# #
# 
# data <- data %>%
#   ungroup(data) %>% 
#   group_by(daypd) %>%
#   mutate(day_bat_in = sum(batt_ladung),      #Umrechnung W in 5 min auf Wh nicht mehr noetig
#          day_bat_out= sum(batt_entladung),
#          day_period_ladehub = sum(ladediff)
#  )
# Perioden zwischen Schnitten mit level-Wert auswerten

data <- data %>% 
   ungroup(data) %>%  # Ladewert umverteilen
   mutate(tmp_bat_in  = batt_ladung,
          tmp_bat_in  = ifelse(lead(len_levelpd) == 1, tmp_bat_in  + lead(batt_ladung)/2   ,tmp_bat_in), 
          tmp_bat_in  = ifelse(lag(len_levelpd)  == 1, tmp_bat_in  + lag(batt_ladung)/2    ,tmp_bat_in), 
          #testdiff    = batt_ladung - tmp_bat_in,
          tmp_bat_in  = ifelse(len_levelpd == 1, 0  ,tmp_bat_in)
          ) %>% 
   mutate(tmp_bat_out  = batt_entladung,
       tmp_bat_out  = ifelse(lead(len_levelpd) == 1, tmp_bat_out  + lead(batt_entladung)/2   ,tmp_bat_out), 
       tmp_bat_out  = ifelse(lag(len_levelpd)  == 1, tmp_bat_out  + lag(batt_entladung)/2    ,tmp_bat_out), 
       #testdiff    = batt_entladung - tmp_bat_out,
       tmp_bat_out  = ifelse(len_levelpd == 1, 0  ,tmp_bat_out) 
       ) %>% 
   ungroup()


data <- data %>% 
  ungroup(data) %>% 
  group_by(levelpd) %>% 
  mutate(lev_bat_in = sum(tmp_bat_in),      #Umrechnung W in 5 min auf Wh nicht mehr noetig
         lev_bat_out= sum(tmp_bat_out),
         eta = ifelse(lev_bat_in != 0, lev_bat_out/lev_bat_in*10000, 0)) %>%        # Dh 10000 entspricht 100%
  ungroup()
data <- data %>% 
    select(-one_of("tmp_bat_out", "tmp_bat_in"))

data <- data %>% 
  ungroup(data) %>% 
  group_by(levelpd) %>% 
  mutate(max_level = max(ladezustand),
         min_level = min(ladezustand),
         hub_level = max_level - min_level,
         mit_level = (min_level+max_level)/2,
         durchsatz = hub_level/len_levelpd*12,                             # Durchsatz = Wh zwischen Min und Max / Stunde
         signum    = as.character(ifelse(max_level > level, "UP","DOWN")),
         lev       = level/1000
         ) %>%     
  ungroup()




# Die in den Perioden kumulierte Batterieladung (In/Out) in Wh sowie der 
# periodenweise zugeordnete Wirkungsgrad werden allen Zeilen zugeordnet
# ------------------------------------------------------------------------------------