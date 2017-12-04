# 04_Woechentliche_Batterie_Ladung

temp <- data %>% 
  group_by(week) %>% 
  select(month,week, day, batt_ladung, batt_entladung) %>% 
  summarise(Laden=sum(batt_ladung)/1000, Entladen = sum(batt_entladung)/1000) %>% 
  mutate(eta_week = round(100*Entladen/Laden,1))

tempsort <- temp %>% 
  gather(type, IN_OUT, Laden, Entladen)

nur_entl <- tempsort %>% 
  filter(type == "Entladen") 
g <- ggplot(tempsort,aes(x = week, y =IN_OUT, fill = type )) 
g +  geom_col(position=position_dodge(), colour="black") +
  labs(
    x = "Woche",
    y = "El. Arbeit in kWh",
    fill = "Richtung" ,
    title="Ladung und Entladung der Batterie woechentlich", 
    subtitle = "Entladung zusaetzlich in '%' der Ladung " ) +
  #ggtitle("Ladung und Entladung der Batterie") +
  scale_y_continuous(breaks = seq(0, 200, by = 10)) +
  geom_text(aes(label=eta_week), data = nur_entl,nudge_x = -0.25, nudge_y = 4)
#ggsave("WOchen_Wirkungsg.pdf")
rm(temp)
rm(tempsort)