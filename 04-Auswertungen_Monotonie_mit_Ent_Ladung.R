# 04-Auswertungen_Monotonie_mit_Ent_Ladung.R

stat_lade_date <- data %>% 
        ungroup() %>% 
        mutate(netto_lad = batt_ladung - batt_entladung,
               sig_lad   = sign(netto_lad),
               eins      = 1) %>% 
        group_by(sig_lad) %>% 
               summarize(pos = sum(eins) )
 
cat("Es werden \n",stat_lade_date$pos[1]," 5min-Intervalle negativer und \n", stat_lade_date$pos[3]," Intervalle positiver Bilanz registiert. \n")
cat(" ",stat_lade_date$pos[2], "Intervalle sind neutral.")

lade_strecken <- data %>% 
  ungroup() %>% 
  mutate(netto_lad  = batt_ladung - batt_entladung,
         sig_lad    = sign(netto_lad),
         eins       = 1,                          #     +++++00000------++++++
         change_fd  = lead(sig_lad)-sig_lad,      #     0000-0000-00000-000000
         change_bd  = sig_lad - lag(sig_lad),     #     00000-0000-00000-00000
         pos_fd     = ct*sign(change_fd),
         pos_bd     = ct*sign(change_bd),
         nr_period  = 0)                      

lade_strecken[is.na(lade_strecken)] <- 0

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

lade_strecken_summ <- lade_strecken %>% 
     ungroup() %>% 
     group_by(abs(nr_period)) %>% 
         mutate (summe_nettoladung = sum(netto_lad)) %>% 
     slice(c(n())) %>% 
     ungroup()

lade_strecken_summ <- lade_strecken_summ %>% 
     mutate(zustandsdiff = ladezustand - lag(ladezustand),
            strecken_laenge = ct - lag(ct),
            eta             = zustandsdiff/summe_nettoladung)




# kuerze_ladestrecken_temp <- lade_strecken %>% 
#           select(-batt_ladung,-batt_entladung)

# anzahl_lade_strecken <- lade_strecken %>% 
#   ungroup() %>% 
#   group_by(change) %>% 
#   summarise(wechsel = sum(eins))

# -2 heisst entladung folgt auf ladung
# -1 heisst entladung folgt neutral oder neutral folgt auf ladung
# keine Aenderung
#  1 heisst ladung folgt auf neutral oder neutral auf entladung
#  2 heisst ladung folgt auf Enladung

# print.data.frame(anzahl_lade_strecken)
# 
# lade_strecken_short <- lade_strecken %>% 
#       mutate(sprung = abs(sign(change))*ct) %>% 
#       filter(sprung != 0) %>% 
#       mutate(laenge = lead(sprung) - sprung) %>% 
#       ungroup()
# 
# stat_lade_strecken <-  lade_strecken_short %>% 
#       group_by(laenge) %>% 
#       summarise(anzahl_laengen = sum(eins))
# 
# stat_lade_strecken$anzahl_laengen
# 
# 
# ggplot(data = lade_strecken_short) +
#   geom_histogram((mapping = aes(x = laenge)), binwidth= 3)



