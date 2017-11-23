# #
# # Voraussetzungen zyklus     schon gebildet
# #                 len_zyklus schon berechnet
# #
# 
#   Perioden zwischen Schnitten mit level-Wert auswerten



zyklus_summen_gen <- function(xdata,l){   # xdata ist der - Datenfile Typ tibble
  #cat("04_Auswertungen_Summ_Ent_Ladung_in_Zyklus", level , "\n")                                           
xdata <- xdata %>% 
   ungroup() %>%  # 
   mutate(tmp_bat_in  = batt_ladung,
          tmp_bat_in  = ifelse(lead(len_zyklus)  == 1, tmp_bat_in  + lead(batt_ladung)/2   ,tmp_bat_in), 
          tmp_bat_in  = ifelse(lag( len_zyklus)  == 1, tmp_bat_in  + lag(batt_ladung)/2    ,tmp_bat_in), 
          tmp_bat_in  = ifelse(len_zyklus        == 1, 0                                   ,tmp_bat_in)
          ) %>% 
   mutate(tmp_bat_out  = batt_entladung,
          tmp_bat_out  = ifelse(lead(len_zyklus) == 1, tmp_bat_out  + lead(batt_entladung)/2   ,tmp_bat_out), 
          tmp_bat_out  = ifelse(lag(len_zyklus)  == 1, tmp_bat_out  + lag(batt_entladung)/2    ,tmp_bat_out), 
          tmp_bat_out  = ifelse(len_zyklus       == 1, 0                                       ,tmp_bat_out) 
        ) %>% 
   ungroup()


xdata <- xdata %>% 
         ungroup() %>% 
         group_by(zyklus) %>% 
   mutate(lev_bat_in = sum(tmp_bat_in),      
          lev_bat_out= sum(tmp_bat_out),
          eta = ifelse(lev_bat_in != 0, lev_bat_out/lev_bat_in*10000, 0)) %>%        # Dh 10000 entspricht 100%
   ungroup()

xdata <- xdata %>% 
    select(-one_of("tmp_bat_out", "tmp_bat_in"))

xdata <- xdata %>% 
   ungroup() %>% 
   group_by(zyklus) %>% 
   mutate(max_level = max(ladezustand),
         min_level = min(ladezustand),
         hub_level = max_level - min_level,
         mit_level = (min_level+max_level)/2,
         durchsatz = hub_level/len_zyklus*12,                             # Durchsatz = Wh zwischen Min und Max / Stunde
         signum    = as.character(ifelse(max_level > l, "UP","DOWN")),
         lev       = l/1000
         ) %>%     
  ungroup()

  return(xdata)
}

# Die in den Perioden kumulierte Batterieladung (In/Out) in Wh sowie der 
# periodenweise zugeordnete Wirkungsgrad werden allen Zeilen zugeordnet
# ------------------------------------------------------------------------------------