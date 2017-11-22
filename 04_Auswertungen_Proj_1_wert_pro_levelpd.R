#   04_Auswertungen_Proj_1_wert_pro_levelpd.R

zyklus_summen_gen <- function(xdata){
   red_data <- data %>%
      ungroup() %>% 
      group_by(levelpd) %>%
      slice(1) %>%
      ungroup()
   
   return(red_data)
}