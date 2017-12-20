
is_even <- function(x) x %% 2 == 0

mymedian <- function(x){ 
  l=length(x)
  #if (l==0) {cat("Fehler: leere Menge \n")}
  if (l>0){
      if (is_even(l)) {length(x) <- l-1 }
      median(x)}
  else {
      0}
}                                             # implementierter Median aus R ist bei geraden Anzahlen unbrauchbar

min_144      <- rollify(min,    window = 144,na_val= 0 )    # Min/Max ueber Fensterbreiten von 144 = 1/2 Tag bilden 
max_144      <- rollify(max,    window = 144,na_val= 0 )

glaettebreite<- 3
schieben     <- floor(glaettebreite/2)        # wird spaeter benötigt
rollmed      <- rollify(.f = ~median(.x,na_val= TRUE), window = glaettebreite  ) # bei Breite 3 keine Vorsicht bzl Median noetig


source("4_Matrix_aus_Paaren.R")

# Entladung und LAdung auf die zeitlichen Zwischenräume umverteilen
# dazu werden jeweils die halben Werte die recht und links von einem 5min Zeitraum 
# registriert sind addiert und
# als Konvention am ENDE der 5min Phase gespeichert
# EHER NICHT NUTZEN
in_out_pd_mitteln <- rollify(.f = ~sum(.x)/2, window = 2, na_value = NULL)

# Normen für die Approx des Ladezustands

norm_2wurzel_aus_2pot <- function(x){
  return(sqrt(sum(x^2)))
}
norm_max_norm <- function(x){
  return(max(x))
}
norm_3wurzel_aus_3pot <- function(x){
  return((sum(x^3))^(1/3))
}