#   04_Auswertungen_Proj_1_wert_pro_levelpd.R

proj_level_new <- data %>%
  ungroup() %>% 
  group_by(levelpd) %>%
  slice(1) %>%
  ungroup()