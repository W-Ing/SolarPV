#   04_Auswertungen_Proj_1_wert_pro_zyklus.R

zyklus_reduzieren <- function(xdata){
   colnames(xdata)  
   red_data <- xdata %>%
      ungroup(xdata) %>% 
      group_by(zyklus) %>%
      slice(1) %>%
      ungroup(xdata)
   return(red_data)
}