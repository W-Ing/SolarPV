# rollend Minima/Maxima über 144 mal 5 min bilden
data <- data %>%
        mutate(roll_min = min_144(lead(ladezustand,72)),
               roll_max = max_144(lead(ladezustand,72)))

data[is.na(data)] <- 0 

data <- data %>%
  mutate(is_min = ifelse(ladezustand == roll_min, 1, 0),
         is_max = ifelse(ladezustand == roll_max, 1, 0))   # Schnittpunkte mit der ladezustandskurve

data %>%                                        # Darstellen der Minima und Maxima
  filter(day > "2017-09-30") %>%
  filter(day < "2017-10-05") %>%
  ggplot(aes(x = zeit, y = ladezustand)) + geom_line(aes(x = zeit, y = ladezustand),color="red") +
  geom_point(aes(x = zeit, y= roll_min, color = is_min)) +
  geom_point(aes(x = zeit, y= roll_max, color = is_max)) #

#--------------------------------------------------------------------------------------
# Einzelpunkt als Max/MIN  pro Tag auswaehlen und in is_min bzw. is_max festhalten

data <- data %>%                                #
  mutate(tmp = ct*is_max)   %>%    # die Maxima durchzaehlen
  ungroup() %>% 
  group_by(day,add = FALSE) %>%
  mutate(tgl_max_median = mymedian(subset(tmp,tmp!=0)),
         is_max = ifelse(tmp == tgl_max_median, 1, 0)) 

data <- data %>%
  ungroup() %>% 
  mutate(tmp = ct*is_min) %>% 
  group_by(day,add = FALSE) %>%
  mutate(tgl_min_median = mymedian(subset(tmp,tmp!=0)),
         is_min = ifelse(tmp == tgl_min_median, 1, 0)) 

data %>%                                        # Darstellen der Minima und Maxima
  filter(day > "2017-09-30") %>%
  filter(day < "2017-10-05") %>%
  ggplot(aes(x = zeit, y = ladezustand)) + geom_line(aes(x = zeit, y = ladezustand),color="red") +
     geom_point(aes(x = zeit, y= roll_min, color = is_min)) +
     geom_point(aes(x = zeit, y= roll_max, color = is_max)) #

# Perioden zwischen Minima
data <- data %>%
   ungroup() %>%
   mutate(daypd = cumsum(is_min)) %>%           # Periodenbeginn in den tägl Minima
   group_by(daypd) %>% 
   mutate(len_daypd = max(ct)-min(ct)+1)

data <- data %>% 
   select(-one_of("tmp","tgl_min_median","tgl_max_median","roll_min","roll_max"))

#
# Voraussetzungen daypd und levelpd schon gebildet
# len_levelpd schon berechnet
#

data <- data %>%
  ungroup(data) %>% 
  group_by(daypd) %>%
  mutate(day_bat_in = sum(batt_ladung),      #Umrechnung W in 5 min auf Wh nicht mehr noetig
         day_bat_out= sum(batt_entladung),
         day_period_ladehub = sum(ladediff)) %>% 
  ungroup(data) 



# Veraendert wird die Tabelle um is_min, is_max
# daypd bedeutet eine Nummerierung der Perioden die jeweils mit einem täglichen MInimum beginnen