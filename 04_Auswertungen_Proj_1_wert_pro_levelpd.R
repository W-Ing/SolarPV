#   04_Auswertungen_Proj_1_wert_pro_levelpd.R

zyklus_reduzieren <- function(xdata){
   colnames(xdata)  
   red_data <- xdata %>%
      ungroup(xdata) %>% 
      group_by(levelpd) %>%
      slice(1) %>%
      ungroup(xdata)
   return(red_data)
}