# 04-Auswertungen_Monotonie_mit_Ent_Ladung.R
# 
# LAdephasen werden an BAtt_ladung/entladung orientiert, Problem: der Erfolg = LAdezustand gibt dies nur grob wieder

stat_lade_date <- data %>% 
        ungroup() %>% 
        mutate(netto_lad = batt_ladung - batt_entladung,
               sig_lad   = sign(netto_lad),
               eins      = 1) %>% 
        group_by(sig_lad) %>% 
               summarize(pos = sum(eins) )
cat("Vorkontrolle:") 
cat("Es werden\n",stat_lade_date$pos[1],"5min-Intervalle negativer und\n", stat_lade_date$pos[3],"Intervalle positiver Bilanz registiert. \n")
cat("",stat_lade_date$pos[2], "Intervalle sind neutral.\n")

lade_strecken <- data %>% 
  ungroup() %>% 
  mutate(netto_lad  = batt_ladung - batt_entladung,
         sig_lad    = sign(netto_lad),
         eins       = 1,                          #     +++++00000------++++++
         change_fd  = lead(sig_lad)-sig_lad,      #     0000-0000-00000-000000
         change_bd  = sig_lad - lag(sig_lad),     #     00000-0000-00000-00000
         pos_fd     = ct*sign(change_fd),         # pos mit Vorz wenn folgendes      LAdevorzeichen anders
         pos_bd     = ct*sign(change_bd),         # pos mit Vorz wenn vorhergehendes LAdevorzeichen anders
         nr_period  = 0)                          # Periodennummer erzeugt

lade_strecken[is.na(lade_strecken)] <- 0         #  Anfang in Ordnung bringen wo lag versagt
lade_strecken$change_bd[1] <- lade_strecken$sig_lad[1]
lade_strecken$pos_bd[1]    <- 1*sign(lade_strecken$change_bd[1])
vec_pos_fd <- lade_strecken %>% 
       filter(pos_fd != 0) 
vec_pos_fd <- vec_pos_fd$pos_fd
vec_pos_bd <- lade_strecken %>% 
  filter(pos_bd != 0) 
vec_pos_bd <- vec_pos_bd$pos_bd

for (i in 1: length(vec_pos_bd)){
   lade_strecken <- lade_strecken %>% 
              mutate(nr_period = ifelse(abs(vec_pos_bd[i]) <= ct, vec_pos_bd[i],nr_period))
}
# Jetzt sind die Strecken konstanten Ladevorzeichens mit der Position ihres Beginns markiert 

lade_strecken_summ <- lade_strecken %>%                       # über die Perioden wird die Nettoladung summiert
     ungroup() %>% 
     group_by(abs(nr_period)) %>% 
         mutate (summe_nettoladung = sum(netto_lad)) %>% 
     slice(c(n())) %>%                                       # jeweils die letzte Zeile einer monotonieperiode wird ausgewählt
     select(-netto_lad) %>% 
     ungroup()

lade_strecken_summ <- lade_strecken_summ %>% 
     ungroup() %>% 
     mutate(newct = cumsum(eins),
            zustandsdiff    = ifelse(newct > 1, ladezustand - lag(ladezustand), ladezustand-5800),  # mit 5800 beginnt die Beoabachtung
            strecken_laenge = ifelse(newct > 1, ct - lag(ct), ct),
            eta             = (zustandsdiff*batt_kapazitaet/summe_nettoladung)^sign(zustandsdiff)) # Korr fuer tatsächlichen Ladezustand

lade_strecken_plot <- lade_strecken_summ %>% 
      filter(eta <= 3) %>% 
      filter(eta >= 0) %>% 
      filter(strecken_laenge > 3)

subtitle_text <- paste("Zugrundegelegt: Ladezustand aus Systemmeldung umgerechnet auf 100% = ", as.character(10*batt_kapazitaet)," kWh." )

p <- ggplot(data = lade_strecken_plot) +
   geom_point((mapping = aes(x = strecken_laenge/12, y = eta, color = zustandsdiff))) +
   labs(
     x      = "Dauer des Ent/Ladevorgangs in h",
     y      = "Wirkungsgrad",
     fill   = "Zustandsdifferenz",
     title  = "Wirkungsgrad waehrend eines kontinuierlichen Lade- oder Entladevorgangs" ,
     subtitle = subtitle_text ) + 
   scale_color_hue(l=100) +
   scale_y_continuous(breaks = seq(0.0, 3,  by = 0.1)) +
   scale_x_continuous(breaks = seq(0.0, 24.0,  by = 1.0)) +
   scale_color_gradient2(midpoint=0, low="darkblue", mid="white", high="red", space="Lab") 
   


