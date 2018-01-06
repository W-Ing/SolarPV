# 02-File_analysieren_umbenennen.R

#data <- tibble()
data0 <- tibble()
path_to_data <- "./Daten/"
files <- dir(path=path_to_data, pattern=glob2rx('^ChartTable.csv'))  # ^Daten_*_09_2017.csv| |'^Daten_*_11_2017.csv'
files
for (file in files){  
     file=paste(path_to_data,file,sep='',collapse='')
     data0 <- read_delim(file, ';',
                    skip= 1 ,    #  Ignoriere Ueberschrift und leere Datenzeile
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
     beginn_tag  <- min(data0$zeit)
     ende_tag    <- max(data0$zeit)
     beginn_hour <- hour(beginn_tag)
     beginn_min  <- minute(beginn_tag)
     ende_hour <- hour(ende_tag)
     ende_min  <- minute(ende_tag)
     zielname <- paste(path_to_data,as.character(beginn_tag),'SUNNY.csv',sep = "")
     if(file.rename(c(file), c(zielname)) ){
        cat("ChartTable.csv erfolgreich umbenannt.\n ")
     } else {
       cat("ChartTable.csv nicht gefunden oder umbenannt.\n ")
     }
     rm(data0)
#data <- rbind(data, data0)
}


