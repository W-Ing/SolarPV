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
     #mutate(len_pd = mean(len_pd)) %>% 
     #filter(mit_eta< 2) %>% 
     #filter(mit_eta>-1) #%>% 
     filter(eta > 0) %>% 
     filter(eta < 2)   %>% 
     filter(abs(zustands_diff) <= 300) 
     #filter(len_pd > 4) %>% 
     #slice(1)

mittlere_wkgr_entladen <- zust_verkuerzt %>% 
  filter(sgn < 0) %>% 
  group_by(ladezustand) %>% 
  mutate(mit_eta = median(eta)) %>% 
  #mutate(len_pd = mean(len_pd)) %>% 
  # filter(mit_eta < 2) %>% 
  # filter(mit_eta >-1) #%>% 
  filter(eta > 0) %>% 
  filter(eta < 2) %>% 
  filter(abs(zustands_diff) <= 300) 
  #filter(len_pd < -4) #%>% 
  #slice(1)

p<- ggplot(data = mittlere_wkgr_laden) +
  scale_color_gradient2(low="darkblue", midpoint = 0, high="red", space="Lab") +
  geom_point((mapping = aes(x = ladezustand, y = eta, color=zustands_diff))) +
  geom_point(data=mittlere_wkgr_entladen,(mapping = aes(x = ladezustand, y = eta, color=zustands_diff)) ) +
  geom_smooth(data=mittlere_wkgr_entladen,(mapping = aes(x = ladezustand, y = eta)),color="blue" ) +
  geom_smooth(data=mittlere_wkgr_laden,(mapping = aes(x = ladezustand, y = eta)),color="red" ) +
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

