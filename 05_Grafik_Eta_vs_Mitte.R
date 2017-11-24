# Auswertung der Halbperioden


cat("level ",level,"\n")

zyklen_bilden(data,level)

proj_level %>% 
   filter(eta > 0 &eta <= 10000) %>% 
   ggplot(aes(x = mit_level, y = eta)) + 
   geom_point(aes(x = mit_level, y = eta, color=signum)) +
   geom_smooth(mapping = aes(x = mit_level, y = eta, linetype = signum ), method= lm) +
   #geom_line(mapping = aes(x = durchsatz, y = eta, linetype = signum )) +
   facet_wrap(~ signum)
 
