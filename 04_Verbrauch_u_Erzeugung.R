# Verbrauch_u_Erzeugung.R
# 
# Parameter dauer     übergibt week oder month
#           v_oder_e           "Verbrauch" oder "Erzeugung"
#           Ztraum             "Woche" oder "Monat" 
verb_u_erzeugung <- function(verb.data, v_oder_e,Ztraum){ # Datentibble - week/month - Verbrauch/Erzeugung - Monat/Woche
#verb_u_erzeugung <- function(verb.data, dauer, v_oder_e,Ztraum){ # Datentibble - week/month - Verbrauch/Erzeugung - Monat/Woche
  # ich komme nicht ohne week month aus um später korrekt die Spalte mit dem gleichen Namen zu referenzieren
  if (Ztraum == "Monat"){
    #qdauer  <- enquo(dauer)              # enquo, wenn auf die übergebene Var bezogen, quo, wenn innerhal auf Text
    qdauer  <- quo(month) 
    # test1    <<- enquo(month) liefert ~function(expr) 
    # test2    <<- enquo("month")
    mnumber <- 12
    xText <- "Monat"
  } else {
    #qdauer  <- enquo(dauer)
    qdauer  <- quo(week)
    mnumber <- 52
    xText <- "Kalenderwoche"
  }
  
  
  if(v_oder_e == "Verbrauch") { 
    diff_key = c("netzbezug", "batt_entladung", "direktverbrauch")
    yText = "Verbrauch (kWh)"
  } else  {
    diff_key = c("netzeinspeisung", "batt_ladung", "direktverbrauch")
    yText = "Von PV-Anlage erzeugt (kWh)"
  }
  
  verb.data$month = as.numeric(verb.data$month)          # Besser bereits früh angleichen nach Prüfung schon in data
  
  V_E_daten <- verb.data %>%
    group_by(!!qdauer) %>%
    summarise(batt_ladung     = sum(batt_ladung)/1000,
              batt_entladung  = sum(batt_entladung)/1000,
              leistung.pv     = sum(leistung.pv)/1000,
              netzeinspeisung = sum(netzeinspeisung)/1000,
              netzbezug       = sum(netzbezug)/1000) %>%
    mutate(   direktverbrauch = leistung.pv - netzeinspeisung - batt_ladung,
              eigenverbrauch  = batt_entladung + direktverbrauch)
  
  V_E.long <<- V_E_daten %>% 
    gather(key = key, value = Wert,  diff_key ) %>%   # Benennt key mit key und value mit Wert, verknuepft damit die in diff_key genannten Variablen
    select(-leistung.pv)
  
  ggplot(V_E.long, aes_(qdauer, V_E.long$Wert, fill = V_E.long$key)) +      # fuer gather
    geom_bar(stat   = "identity", position = "stack") +                     # identity: stellt die Werte dar, stack stapelt
    labs(     x     = xText,
              y     = yText,
              fill  = " Art",
              title = v_oder_e,
              subtitle = "differenziert nach Verwendung") +
    scale_x_continuous(breaks = seq(1, mnumber,  by = 1)) +
    geom_text(aes(label=round(Wert,-0)), size= 3, data = V_E.long, position = position_stack(vjust=0.5))
}



