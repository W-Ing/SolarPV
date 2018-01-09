#
# Batterie In/Out in Wh umrechen
data <- data %>% 
  mutate(batt_ladung    = batt_ladung/12,
         batt_entladung = batt_entladung/12,
         leistung.pv    = leistung.pv/12,
         leistung.stp   = leistung.stp/12,
         netzeinspeisung= netzeinspeisung/12,
         netzbezug      = netzbezug/12    )  # Jetzt sind die Daten in Wh umgerechnet, die jeweils in 5 min erbracht werden

# abgeleitete Groessen bilden

data <- data %>%                            
  mutate(
    month    = (format(zeit,"%m")),   # gibt chr , besser Evtl Wieder as.numeric
    week     = week(zeit),
    day      = as_date(zeit),
    hour     = as.numeric(format(zeit,"%H")), # gibt num
    ladediff = ladezustand- lag(ladezustand)) # diff geht nicht wg laenge

data[is.na(data)] <- 0

source("03_Quellen_pruefen.R")

# Indexspalteanfuegen ------------------------------------------------
data <- data %>%                             
  mutate(ct = 1, 
         ct = cumsum(ct))
cat('Der Datensatz enthaelt jetzt ',max(data$ct), 'Zeilen.\n')
#
# -------------------------------------------------------------------
cat("\n")
cat('Erzeuge Tabelle verbrauch \n')

#
# LAdezustand auch in Wh speichern - verwende batt_kapazitaet

data <- data %>% 
  mutate(ladezustand_Wh = ladezustand * batt_kapazitaet)
#--------------------------------------------------------------------------
verbrauch <- data
verbrauch <- verbrauch %>% 
     select(-leistung.stp)
         

loesche <- c("leistung.pv","leistung.stp","netzeinspeisung","netzbezug")
data    <- data %>% select(-one_of(loesche))
cat('Loesche aus data Spalten: ', loesche, '.\n')


# -----------------------------------------------------------------------------
# # Ladezustand glaetten
#  data[is.na(data)] <- 0
#  data <- data %>% 
#           mutate(ladezustand = rollmed(lead(ladezustand,schieben))) 
#  data[is.na(data)] <- 0
# ----------------------------------------------------------------------------- 
