# Auswertung der Halbperioden

# proj_level_new <- data %>%
#   ungroup() %>% 
#   group_by(levelpd) %>%
#   slice(1) %>%
#   ungroup()
# 
# proj_level <- rbind(proj_level, proj_level_new)


zyklen_bilden(data,level)

proj_level %>% 
   filter(eta > 0 &eta <= 10000) %>% 
   ggplot(aes(x = mit_level, y = eta)) + 
   geom_point(aes(x = mit_level, y = eta, color=signum)) +
   geom_smooth(mapping = aes(x = mit_level, y = eta, linetype = signum )) +
   #geom_line(mapping = aes(x = durchsatz, y = eta, linetype = signum )) +
   facet_wrap(~ signum)
 
