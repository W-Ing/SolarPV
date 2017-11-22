# Auswertung der Halbperioden

cat ("05_Grafik_Vs_Durchsatz ", level," \n")

proj_level <- tibble()

proj_level_new <- data %>%
  ungroup() %>% 
  group_by(levelpd) %>%
  slice(1) %>%
  ungroup()

proj_level <- rbind(proj_level, proj_level_new)

proj_level <- proj_level %>% 
  filter(eta > 0 & eta <= 10000) %>% 
  filter(day >= "2017-10-01") 

proj_level %>% 
   ggplot(aes(x = durchsatz/1000, y = eta)) + 
   geom_point(aes(x = durchsatz/1000, y = eta, color=lev)) +
   geom_smooth(mapping = aes(x = durchsatz/1000, y = eta, linetype = signum )) +
   #geom_line(mapping = aes(x = durchsatz/1000, y = eta, linetype = signum )) +
   labs(
      x = "Durchsatz in kWh/h",
      y = "Wirkungsgrad * 1000",
      color = "level" )+
   facet_wrap(~ signum)
 
