# 04_Modell_Ent_Lade_Wirkungsgrad.R 

lade_data <- data %>% 
  select(zeit, month, week, day, hour, ct, batt_ladung, batt_entladung, ladezustand ) %>% 
  mutate(ladezustand_Wh = 0.98 * ladezustand) %>% 
  mutate(korr_ladung = batt_ladung) %>% 
  select(-batt_ladung)

lade_data$korr_ladung[1]<- lade_data$korr_ladung[1]+5800    # korrekt wäre, 5800/eta_1, letzteres ist aber noch unbekannt

# -------------------------------------------------------------------

fehler_func <- function(e1, e2,norm_func){           # verwende globale Daten lade_data
  temp_data <- lade_data %>%
    mutate(kombi_ent_ladung = e1*korr_ladung - batt_entladung/e2) %>%
    mutate(cum_ladung = cumsum(kombi_ent_ladung)) %>%
    mutate(abweichung = abs(cum_ladung - ladezustand_Wh)) %>%
    #summarize(f = max(abweichung))
    summarize(f = norm_func(abweichung)) 
  #summarize(f = sum(abweichung))
  return(as.numeric(temp_data$f[1]))
}

# fehler_func <- function(e1, e2){           # verwende globale Daten lade_data
#   temp_data <- lade_data %>%
#     mutate(kombi_ent_ladung = e1*korr_ladung - batt_entladung/e2) %>%
#     mutate(cum_ladung = cumsum(kombi_ent_ladung)) %>%
#     mutate(abweichung = abs(cum_ladung - ladezustand_Wh)) %>%
#     #summarize(f = max(abweichung))
#     summarize(f = sqrt(sum(abweichung^2))) 
#   #summarize(f = sum(abweichung))
#   return(as.numeric(temp_data$f[1]))
# }

Mat_EtaPaare <- matrix_bilden(0.90, 1.001, 0.001, 0.65, 0.75, 0.001)

Mat_EtaPaare <- Mat_EtaPaare %>% 
  mutate(fehler = 0  )

 # -----------------------------------------------------------------------

# for (i in 1:nrow(Mat_EtaPaare)){
#   e1 <- as.numeric(Mat_EtaPaare[i,1])     # Fuer jedes Wertepaar eta_1/eta_2 wird der Fehler zwischen ladezustand und
#   e2 <- as.numeric(Mat_EtaPaare[i,2])     # simuliertem LAdevorgang in kombi_entladung berechnet
#   Mat_EtaPaare$fehler[i] = fehler_func(e1,e2)
# }

for (i in 1:nrow(Mat_EtaPaare)){
  e1 <- as.numeric(Mat_EtaPaare[i,1])     # Fuer jedes Wertepaar eta_1/eta_2 wird der Fehler zwischen ladezustand und
  e2 <- as.numeric(Mat_EtaPaare[i,2])     # simuliertem LAdevorgang in kombi_entladung berechnet
  Mat_EtaPaare$fehler[i] = fehler_func(e1,e2,waehle_norm_fu)
}

# Mat_EtaPaare <- Mat_EtaPaare %>%   # funktioniert nicht verwende evtl vectorize
#      rowwise() %>%  
#      mutate(fehler = fehler_func(e1,e2))

# x <- eta1   <- Mat_EtaPaare$v1
# y <- eta2   <- Mat_EtaPaare$v2
# z <- fehler <- Mat_EtaPaare$fehler

Minfehler <- min(Mat_EtaPaare$fehler)

kurz <- Mat_EtaPaare %>% 
  filter(fehler == min(Mat_EtaPaare$fehler))

Mat_EtaPaare <- Mat_EtaPaare %>% 
  mutate(fehler = ifelse(fehler >= 1.2*Minfehler, 1.2*Minfehler, fehler))
  


eta_1_max <- as.numeric(kurz[1,1])
eta_2_max <- as.numeric(kurz[1,2])

lade_data <- lade_data %>%
  mutate(cum_ladung = cumsum(eta_1_max*korr_ladung - batt_entladung/eta_2_max))

cat("Fuer Ladung und Entladung ergeben sich die Wirkungsgrade", eta_1_max, " und", eta_2_max, " \n")

