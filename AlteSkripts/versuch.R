lade_data <- data %>% 
  select(zeit, month, week, day, hour, ct, batt_ladung, batt_entladung, ladezustand ) %>% 
  mutate(korr_ladung = batt_ladung) %>% 
  select(-batt_ladung)

lade_data$korr_ladung[1]<- lade_data$korr_ladung[1]+5800    # korrekt wäre, 5800/eta_1, letzteres ist aber noch unbekannt

# -------------------------------------------------------------------
fehler_func <- function(e1, e2){           # verwende globale Daten lade_data
  temp_data <- lade_data %>%
    mutate(kombi_ent_ladung = e1*korr_ladung - batt_entladung/e2) %>%
    mutate(cum_ladung = cumsum(kombi_ent_ladung)) %>%
    mutate(abweichung = abs(cum_ladung - ladezustand)) %>%
    #summarize(f = max(abweichung))
    summarize(f = sum(abweichung^2))      # Bem: Supremumsnorm liefert denselben Wert
  return(as.numeric(temp_data$f[1]))
  
}
# --------------------------------------------------------------

list_fu <- function(start, num, delta){
    x<-c(start)
    for (i in 1:(num-1 ) )
         {x<-append(x,delta)}
    return(cumsum(x))
}

prod<-c()
for (i in list_fu(0.75, 25, 0.01)){
   for (j in list_fu(0.65, 30, 0.01)){
      prod <- append(prod,c(i,j))
  }
}

M <- matrix(prod,nrow=2,ncol=length(prod)/2)
Mat_EtaPaare <- as.tibble(t(M))

Mat_EtaPaare <- Mat_EtaPaare %>% 
  mutate(fehler = 0  )

laenge_eta_paare <- nrow(Mat_EtaPaare)

for (i in 1:laenge_eta_paare){
  e1 <- as.numeric(Mat_EtaPaare[i,1])     # Hier war der Hund begraben, jetzt muss ich wieder rueckbauen
  e2 <- as.numeric(Mat_EtaPaare[i,2])
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