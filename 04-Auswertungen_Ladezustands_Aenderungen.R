# 04-Auswertungen_Ladezustands_Aenderungen.R

#colnames <- colnames(data)

# Spalten umsortieren
zust_data <- data[,c(1,5,6,7,8,10,2,3,4,11)]

# Änderungen des Zustands markieren

zust_data <- zust_data %>% 
  mutate(lade_diff = batt_ladung-batt_entladung) %>% 
  mutate(change = 0) %>% 
  mutate(change = ifelse(ladezustand > lag(ladezustand), 1, change)) %>%  # hier wird der Beginn einer Phase mit konstanten Ladezustand mark
  mutate(change = ifelse(ladezustand < lag(ladezustand),-1, change))

zust_data$change[1] <- 1                                               # Der Startwert wird als erste Periode markiert
                                                                       # Gleichzeitig wird der nicht definierte Wert an Stelle 1 gefüllt
zust_data <- zust_data %>% 
  mutate(pd = ct * abs(change))                                        # die Periodenbeginne werden mit ihrer Pos versehen

wert <- 1                                                              # Lücken auffüllen
for (i in 1:nrow(zust_data)){                                          # Jetzt haben die Per konstanten LAdezustands eine Markierung
   if (wert >= zust_data$pd[i]){zust_data$pd[i] <- wert}               # mit ihrer Startposition
   else {wert <- zust_data$pd[i]}
}

zust_verkuerzt <- zust_data %>%   # Reduktion auf (pseudo-)konstante Zustände
     group_by(pd) %>% 
     mutate(lade_bilanz = sum(lade_diff)) %>%                          # Summe der Ladediff während der PEriode wird berechnet
     select(-lade_diff) %>% 
     slice(c(n())) %>%                                                 # für jede Periode wird die letzte Pos aufgehoben
     ungroup() %>%                                                
     mutate(zustands_diff = lead(ladezustand)-ladezustand,             # und die LAdediff zur nächsten Periode berechnet
            sgn = sign(zustands_diff),                                 # sgn gibt an, ob der Ladezustand steigen/fallen wird
            eta = (zustands_diff*batt_kapazitaet/lade_bilanz)^sgn,     # eta ist der Wirkungsgrad beim LAden/Entladen (deswegen ggf Kehrwert!)
            len_pd = (lead(pd)-pd)*sgn    ) 
                                                                       # umrechnung auf Wh!
# ---------------------------------------------------------------------

mittlere_wkgr_laden <- zust_verkuerzt %>%                              
     filter(sgn >0) %>% 
     group_by(ladezustand) %>% 
     mutate(mit_eta = median(eta)) %>% 
     filter(mit_eta< 2) %>% 
     filter(mit_eta>-1) %>% 
     slice(1)

mittlere_wkgr_entladen <- zust_verkuerzt %>% 
  filter(sgn < 0) %>% 
  group_by(ladezustand) %>% 
  mutate(mit_eta = median(eta)) %>% 
  filter(mit_eta < 2) %>% 
  filter(mit_eta >-1) %>% 
  slice(1)

p<- ggplot(data = mittlere_wkgr_laden) +
  geom_point((mapping = aes(x = ladezustand, y = mit_eta, color=len_pd))) +
  scale_color_gradient2(low="darkblue", midpoint = 0, high="red", space="Lab") +
  geom_point(data=mittlere_wkgr_entladen,(mapping = aes(x = ladezustand, y = mit_eta, color=len_pd)) ) +
  geom_smooth(data=mittlere_wkgr_entladen,(mapping = aes(x = ladezustand, y = mit_eta)) ) +
  geom_smooth(data=mittlere_wkgr_laden,(mapping = aes(x = ladezustand, y = mit_eta)) ) +
  theme_dark()

# ------------------------------------------------------------------------
# anderer_vers <- zust_verkuerzt %>% 
#     group_by(sgn,ladezustand) %>% 
#     summarise(mit_eta = median(eta)) %>% 
#     filter(mit_eta < 1.5) %>% 
#     filter(mit_eta > 0.5)
# 
# ggplot(data = anderer_vers) +
#   geom_point((mapping = aes(x = ladezustand, y = mit_eta, color=as.character(sgn)))) 

#-------------------------------------------------------------------------

# darst <- zust_verkuerzt %>% 
#      filter(eta< 2) %>% 
#      filter(eta>-1) %>% 
#      filter(zustands_diff <= 300) %>% 
#      filter(zustands_diff >=-300) 
#      
#   ggplot(data = darst) +
#   geom_point((mapping = aes(x = ladezustand, y = eta, color = zustands_diff))) +
#   #labs(
#   #  x      = "Dauer des Ent/Ladevorgangs in h",
#   #  y      = "Wirkungsgrad",
#   #  fill   = "Zustandsdifferenz",
#   #  title  = "Wirkungsgrad waehrend eines kontinuierlichen Lade- oder Entladevorgangs" ,
#   #  subtitle = subtitle_text ) + 
#   #scale_color_hue(l=100) +
#   #scale_y_continuous(breaks = seq(0.0, 3,  by = 0.1)) +
#   #scale_x_continuous(breaks = seq(0.0, 24.0,  by = 1.0)) +
#   scale_color_gradient2(midpoint=0, low="darkblue", mid="white", high="red", space="Lab") 
# 
#   ggplot(data = mittlere_wkgr) +
#     geom_point((mapping = aes(x = ladezustand, y = mit_eta))) +
#     #labs(
#     #  x      = "Dauer des Ent/Ladevorgangs in h",
#     #  y      = "Wirkungsgrad",
#     #  fill   = "Zustandsdifferenz",
#     #  title  = "Wirkungsgrad waehrend eines kontinuierlichen Lade- oder Entladevorgangs" ,
#     #  subtitle = subtitle_text ) + 
#     #scale_color_hue(l=100) +
#     #scale_y_continuous(breaks = seq(0.0, 3,  by = 0.1)) +
#     #scale_x_continuous(breaks = seq(0.0, 24.0,  by = 1.0)) +
#     scale_color_gradient2(midpoint=0, low="darkblue", mid="white", high="red", space="Lab") 
#   

