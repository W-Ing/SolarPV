# R-Code fuer Input # library(zoo)
data <- tibble()
data0 <- tibble()
path_to_data <- "./Daten/"
files <- dir(path=path_to_data, pattern=glob2rx('^Daten_*_11_2017.csv|^Daten_*_10_2017.csv|^Daten_*_12_2017.csv'))  # ^Daten_*_09_2017.csv| |'^Daten_*_11_2017.csv'
files
for (file in files)
{    file=paste(path_to_data,file,sep='',collapse='')
                    data0 <- read_delim(file, ';',
                    skip= 2 ,    #  Ignoriere Ueberschrift und leere Datenzeile
                    col_names = c('zeit',
                                  'leistung.pv', 
                                  'leistung.stp', 
                                  'netzeinspeisung',
                                  'netzbezug',
                                  'batt_ladung',
                                  'batt_entladung',
                                  'ladezustand'),
                    col_types =   cols(zeit = col_datetime("%d.%m.%Y %H:%M"),
                                       leistung.pv = col_integer(),
                                       leistung.stp = col_integer(),                 
                                       netzeinspeisung = col_integer(),
                                       netzbezug = col_integer(),
                                       batt_ladung = col_integer(),
                                       batt_entladung = col_integer(), #ladezustand = col_number()
                                       ladezustand = col_character() )
                    #n_max=50                   
)
data <- rbind(data, data0)
}
rm(data0)

#cat('gelesen:\n',files, '\n')

# Umwandeln des mit Komma geschrieben Prozentwerts in eine numerische Variable und Umrechnung 100 % entsprechen 10000

data$ladezustand <- as.numeric(gsub(",",".",data$ladezustand))*100

#loesche <- c("leistung.stp")                # Duplikat entfernen  
#data    <- data %>% select(-one_of(loesche))
#-------------------------------------------------------------------
data <- data %>% 
    arrange(zeit)     # Um die Zeilen korrekt anzuordnen


#-------------------------------------------------------------------





  
