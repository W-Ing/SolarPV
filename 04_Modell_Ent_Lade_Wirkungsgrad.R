# 04_Modell_Ent_Lade_Wirkungsgrad.R 

lade_data <- data %>% 
  select(zeit, month, week, day, hour, ct, batt_ladung, batt_entladung, ladezustand ) %>% 
  mutate(ladezustand_wH = 0.98 * ladezustand) %>% 
  mutate(korr_ladung = batt_ladung) %>% 
  select(-batt_ladung)

lade_data$korr_ladung[1]<- lade_data$korr_ladung[1]+5800    # korrekt wäre, 5800/eta_1, letzteres ist aber noch unbekannt

# -------------------------------------------------------------------
fehler_func <- function(e1, e2){           # verwende globale Daten lade_data
  temp_data <- lade_data %>%
    mutate(kombi_ent_ladung = e1*korr_ladung - batt_entladung/e2) %>%
    mutate(cum_ladung = cumsum(kombi_ent_ladung)) %>%
    mutate(abweichung = abs(cum_ladung - ladezustand_wH)) %>%
    #summarize(f = max(abweichung))
    summarize(f = sum(abweichung^2))      # Bem: Supremumsnorm liefert denselben Wert
  return(as.numeric(temp_data$f[1]))
}
# --------------------------------------------------------------
# Erezeugt Liste mit Startwert start, num-1 Elementen im Abstand delta
list_fu <- function(start, num, delta){
    x<-c(start)
    for (i in 1:(num-1 ) )
         {x<-append(x,delta)}
    return(cumsum(x))
}
# -------------------------------------------------------------------
# Cart. Produkt aus zwei Listen    # 
prod<-c()
for (i in list_fu(0.7, 30,0.01)){
   for (j in list_fu(0.65, 35, 0.01)){
      prod <- append(prod,c(i,j))
  }
}

# ----------------------------------------------------------------------
# Verteilt paarweise auf Matrix und ergaenzt dritte Spalte

M <- matrix(prod,nrow=2,ncol=length(prod)/2)

Mat_EtaPaare <- as.tibble(t(M))

Mat_EtaPaare <- Mat_EtaPaare %>% 
  mutate(fehler = 0  )
 # -----------------------------------------------------------------------


laenge_eta_paare <- nrow(Mat_EtaPaare)

for (i in 1:laenge_eta_paare){
  e1 <- as.numeric(Mat_EtaPaare[i,1])     # Fuer jedes Wertepaar eta_1/eta_2 wird der Fehler zwischen ladezustand und
  e2 <- as.numeric(Mat_EtaPaare[i,2])     # simuliertem LAdevorgang in kombi_entladung berechnet
  Mat_EtaPaare$fehler[i] = fehler_func(e1,e2)   
}

# x <- eta1   <- Mat_EtaPaare$v1
# y <- eta2   <- Mat_EtaPaare$v2
# z <- fehler <- Mat_EtaPaare$fehler

kurz <- Mat_EtaPaare %>% 
  filter(fehler == min(Mat_EtaPaare$fehler))

eta_1 <- as.numeric(kurz[1,1])
eta_2 <- as.numeric(kurz[1,2])
# eta_1 <- 0.94   # versuchesweise angenommen
# eta_2 <- 0.74
lade_data <- lade_data %>%
  mutate(cum_ladung = cumsum(eta_1*korr_ladung - batt_entladung/eta_2))