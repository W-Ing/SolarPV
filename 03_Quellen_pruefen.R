# 03_Quellen_pruefen.R

monate <- data %>% 
      group_by(month) %>% 
      slice(1) %>% 
      select(month)



alle_zeiten <- data %>% 
    select(zeit)


