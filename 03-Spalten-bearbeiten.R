#
loesche <- c("leistung.pv","netzeinspeisung","netzbezug")
data    <- data %>% select(-one_of(loesche))
cat('Loesche Spalten ', loesche, '.\n')

# Indexspalteanfuegen ------------------------------------------------
data <- data %>%                             
  mutate(ct = 1, 
         ct = cumsum(ct))
cat('Der Datensatz enthaelt jetzt ',max(data$ct), 'Zeilen.\n')
#
# -------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Ladezustand glaetten
 data[is.na(data)] <- 0
 data <- data %>% 
          mutate(ladezustand = rollmed(lead(ladezustand,schieben)))
 data[is.na(data)] <- 0
# ----------------------------------------------------------------------------- 
# abgeleitete Groessen bilden
data <- data %>%
  mutate(day      = as_date(zeit),
         hour     = as.numeric(format(zeit,"%H")),
         ladediff = ladezustand- lag(ladezustand)) # diff geht nicht wg laenge
data[is.na(data)] <- 0

# Batterie In/Out in Wh umrechen
data <- data %>% 
  mutate(batt_ladung    = batt_ladung/12,
         batt_entladung = batt_entladung/12)  # Jetzt sind die Daten in Wh umgerechnet, die jeweils in 5 min erbracht werden