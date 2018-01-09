# 04_NLS_fuer_Ent_Lade_Wirkungsgrad.R


lade_data <- data %>% 
  select(zeit, month, week, day, hour, ct, batt_ladung, batt_entladung, ladezustand, ladezustand_Wh) %>% 
  #mutate(ladezustand_Wh = batt_kapazitaet * ladezustand) %>% 
  mutate(korr_ladung = batt_ladung) %>% 
  select(-batt_ladung)

lade_data$korr_ladung[1]<- lade_data$korr_ladung[1]+lade_data$ladezustand_Wh[1]  # korrekt wäre, + ..../eta_1, letzteres ist aber noch unbekannt

xvalues <- lade_data$zeit
yvalues <- lade_data$ladezustand_Wh

# Funktion 

model_func <- function(time, e1, e2, data){           
  temp_data <- data %>%
    mutate(kombi_ent_ladung = e1*korr_ladung - batt_entladung/e2) %>%
    mutate(cum_ladung = cumsum(kombi_ent_ladung)) 
    return(temp_data$cum_ladung)
}

model <- nls(yvalues ~ model_func(xvalues,b1,b2,lade_data),start = list(b1 = 0.8,b2 = 0.7))
#model
#predict(model)
eta_1_max <- as.numeric(coef(model)[1])
eta_2_max <- as.numeric(coef(model)[2])

# #subtitel_char <- paste("grün: tatsächlich, blau: modelliert mit eta_1 = ", eta_1_max, "und eta_2 = ",eta_2_max," optimiert bzl.", merke_norm_fu )
# ggplot(lade_data) +
#   geom_point(mapping = aes(x=zeit,y=cum_ladung),  color = "blue")  +
#   geom_point(mapping = aes(x=zeit,y=ladezustand_Wh), color = "green") +
#   labs(x = "Zeit", 
#        y = "Ladezustand (Wh)",
#        title = "Modellierter und tatsächlicher Ladezustand",
#        subtitle = subtitel_char)

lade_data <- lade_data %>%
  mutate(cum_ladung = cumsum(eta_1_max*korr_ladung - batt_entladung/eta_2_max))

cat("Fuer Ladung und Entladung ergeben sich die Wirkungsgrade", eta_1_max, " und", eta_2_max, " \n")
