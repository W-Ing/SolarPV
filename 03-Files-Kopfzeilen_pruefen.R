# 03-Files-Kopfzeilen_pruefen.R

check_col_heads <- tibble()
check_col_heads_0 <- tibble()
path_to_data <- "./Daten/"
files <- dir(path=path_to_data, pattern=glob2rx('^Daten_*_11_2017.csv|^Daten_*_10_2017.csv|^Daten_*_12_2017.csv'))  # ^Daten_*_09_2017.csv| |'^Daten_*_11_2017.csv'
#files
for (file in files)
{    file=paste(path_to_data,file,sep='',collapse='')
check_col_heads_0 <- read_delim(file, ';',
                    col_names = c('zeit',
                                  'leistung.pv', 
                                  'leistung.stp', 
                                  'netzeinspeisung',
                                  'netzbezug',
                                  'batt_ladung',
                                  'batt_entladung',
                                  'ladezustand'),
                    col_types =   cols(zeit = col_character(),
                                       leistung.pv = col_character(),
                                       leistung.stp = col_character(),                 
                                       netzeinspeisung = col_character(),
                                       netzbezug = col_character(),
                                       batt_ladung = col_character(),
                                       batt_entladung = col_character(), #ladezustand = col_number()
                                       ladezustand = col_character() ),
                    n_max=1                   
)
check_col_heads <- rbind(check_col_heads, check_col_heads_0)
}
rm(check_col_heads_0)

check_col_heads <- distinct(check_col_heads)

cat("Kontrolle der Kopfzeilen auf Gleichheit. \n")

if (nrow(check_col_heads) == 1) {
  cat("Die Kopfzeilen aller Dateien stimmen ueberein. \n")
} else {
  cat("Warnung: Die Kopfzeilen aller Dateien stimmen nicht ueberein. \n")}
  
