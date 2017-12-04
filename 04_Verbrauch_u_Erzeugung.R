# Verbrauch_u_Erzeugung.R






plotte_erzeugung_u_verbrauch <- function(df, period, typ,  titel){     # Datenfile, Typ = Erzeugung/Verbrauch
  qperiod <- enquo(period)
  
  df <- df %>%
    group_by(!!qperiod) %>%
    summarise(batt_ladung     = sum(batt_ladung)/1000,
              batt_entladung  = sum(batt_entladung)/1000,
              leistung.pv     = sum(leistung.pv)/1000,
              netzeinspeisung = sum(netzeinspeisung)/1000,
              netzbezug       = sum(netzbezug)/1000) %>%
    mutate(   direktverbrauch = leistung.pv - netzeinspeisung - batt_ladung,
              eigenverbrauch  = batt_entladung + direktverbrauch)
  return(df)
}
  # if(typ == "Erzeugung"){
  #   df.long <- gather(df , id = !!qperiod,val, direktverbrauch,batt_ladung,netzeinspeisung)
  #   } else {
  #   df.long <- gather(df , id = !!qperiod, measure = c("netzbezug","batt_entladung", "direktverbrauch"))
  #   }
  
  # tempsort <- temp %>%          # Aufbereiten für das Diagramm
  #   gather(type, IN_OUT, Laden, Entladen)
  
  
 # return(df.long)
  # ggplot(df.long, aes(!!qperiod, value, fill = variable)) +     #
  #   geom_bar(stat = "identity", position = "stack") +
  #   labs(     x  = "Kalenderwoche",
  #             y  = "Von PV-Anlage in der Woche erzeugt (kWh)",
  #             fill  = " Art",
  #             title = "Test",
  #             subtitle = "differenziert nach Verwendung") +
  #   geom_text(aes(label=round(value,-0)), size= 3, data = df.long, position = position_stack(vjust=0.5))
#}

