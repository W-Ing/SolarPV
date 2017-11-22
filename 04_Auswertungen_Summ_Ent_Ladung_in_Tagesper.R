cat("04_Auswertungen_Summ_Ent_Ladung_in_Tagesper.R \n")


data <- data %>%
  ungroup(data) %>%
  group_by(daypd) %>%
  mutate(day_bat_in = sum(batt_ladung),      #Umrechnung W in 5 min auf Wh nicht mehr noetig
         day_bat_out= sum(batt_entladung),
         day_period_ladehub = sum(ladediff)
  ) %>% 
  ungroup()