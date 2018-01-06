# Funktion definieren

erzeuge_ent_lade_diagramm <- function(df, period, helptext,xtext){    # df Daten, period week month year
  qperiod <- enquo(period)                                            # helptext woechentlich, monatlich, jaehrlich
                                                                      # xtext Woche Monat Jahr
  Titeltext   = paste("Ladung und Entladung der Batterie", helptext)
  xAchsentext = xtext
  mnumber     = ifelse(xAchsentext == "Monat", 12, 52)
  
  df$month = as.numeric(df$month)
  
  temp <- df %>%                # Summen periodenspezifisch bilden
    group_by(!!qperiod) %>% 
    select(month,week, day, batt_ladung, batt_entladung) %>% 
    summarise(Laden=sum(batt_ladung)/1000, Entladen = sum(batt_entladung)/1000) %>% 
    mutate(eta_period = round(100*Entladen/Laden,1)) 
  
  tempsort <- temp %>%          # Aufbereiten für das Diagramm
    gather(type, IN_OUT, Laden, Entladen)
  
  nur_entl <- tempsort %>%      # Datensatz für Wirkungsgradeintrag praeparieren
    filter(type == "Entladen")
  
  g <- ggplot(tempsort,aes_(x = qperiod , y =quote(IN_OUT), fill = quote(type) ))
  g <- g +  geom_col(position=position_dodge(), colour="black") +
    labs(
      x = xAchsentext,
      y = "El. Arbeit in kWh",
      fill = "Richtung" ,
      title=  Titeltext ,
      subtitle = "Entladung zusaetzlich in '%' der Ladung " ) +
    scale_y_continuous(breaks = seq(0, 200, by = 10)) +
    scale_x_continuous(breaks = seq(1, mnumber,  by = 1)) +
    #scale_x_discrete(breaks   =  ???) +
    geom_text(aes(label=eta_period), data = nur_entl,nudge_x = -0.25, nudge_y = 4)
 
   return(g)
}
