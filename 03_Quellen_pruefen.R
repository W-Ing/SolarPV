# 03_Quellen_pruefen.R

# source("03-Files-Kopfzeilen_pruefen.R") # Um sicherzustellen, dass die Reihenfolge stimmt, kann händisch falsch gemacht werden!



# # Pruefen auf doppelte Zeitangaben---------------------------------------
# n <- nrow(data)
# m <- nrow(distinct(data,zeit))
# 
# cat("Kontrolle auf redundante Zeilen: ")
# 
# if (n != m) {
#      cat("Warnung, es gibt doppelte Zeiten.\n")
#   } else {cat("Ok, es gibt keine doppelte Zeiten.\n\n")}
# 

# -----------------------------------------------------------------------
# Extrahiere Zeiten und Tage im Zeit/Date Format

tage_im_Jahr <- data %>% 
      mutate(mon = month(zeit, label = TRUE),
             Jahr = format(zeit,"%Y")) %>% 
      group_by(Jahr, mon, day) %>% 
      summarize(obs = n())

mon_im_Jahr <- tage_im_Jahr %>% 
      group_by(Jahr,mon) %>% 
      summarize(tage = n()) %>% 
      mutate( monJahr = paste(mon,Jahr))


# ------------------------------------------------------------------
# Ausgeben Tage in den vorkommenden Monaten

list_Monate <- pull(mon_im_Jahr, monJahr)

for (m in list_Monate){
     temp <- mon_im_Jahr %>% 
        filter(monJahr == m) 
     cat(m, "hat Daten von ", temp$tage, "Tagen \n")      
   }

# -------------------------------------------------------------------
# AUflisten Tage mit unvollständigen Daten
cat("\n")

temp <- tage_im_Jahr %>% 
  filter(obs < 288) %>% 
  mutate(day = as.character(day))

list_tage   <- pull(temp, day) # Vektor der Tage mit weniger als 12 * 24 = 288 Beobachtungen

if (length(list_tage) >0 ){
   cat("Warnung: An folgenden Tage liegen weniger als 288 Beobachtungen vor: \n")
   for (t in list_tage){
       mtemp <- temp %>% 
        filter(day==t)
       cat(t , "hat nur ", mtemp$obs, " Beobachtungen.\n" )
   }
   cat("\n")
}

# -------------------------------------------------------------------------
temp <- data %>% 
  filter(batt_ladung !=0 & batt_entladung !=0 ) 

cat("Info: In ", nrow(temp)," Zeilen der Originaldaten findet gleichzeitig Ladung und Entladung statt.\n\n")
rm(temp)


# -------------------------------------------------------------------






