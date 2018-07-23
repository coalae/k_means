
############################################################################################################
# ABSCHLUSSBEISPIEL:
# Cordula Eggerth
# Lusine Yeghiazaryan
# Dariga Ramazanova
############################################################################################################

## Sicherstellen, dass keine alten Objekte im File sind:
rm(list=ls())

## libraries
library (cluster)
install.packages("randomcoloR")
library(randomcoloR)

## default daten:
daten <- faithful


## VARIABLES FOR TESTING
#k <- 3
#x <- faithful
#trace <- FALSE 
#maxiter <- 10 
#change <- 0.01



##-----------------------------------------------------
## FUNCTION KMEANS:
##-----------------------------------------------------

# input-parameter:
#   x ... dataframe (dimension nx2, d.h. n rows, 2 cols)
#   k ... anzahl der zu bildenden teilgruppen
#   trace=FALSE ... falls TRUE, dann soll fuer jeden zwischenschritt eine grafik produziert werden
#   maxiter=10 ... maximale anzahl von iterationsschritten
#   change=0.001 ... abbruchswert fuer die relative aenderung

# output (list):
#   iter ... anzahl d er durchgefuehrten iterationsschritte
#   zentren ... matrix der dimension kx2 (enthält für jede teilmenge die mittelwerte der beiden variablen)
#   index ... vektor der laenge n (enthaelt fuer jeden datenpunkt info, zu welcher teilmenge er gehoert)
#   distanz ... vektor der laenge n (enthaelt fuer jeden datenpunkt die distanz zum zentrum seiner teilmenge)

KMEANS <- function(x=daten, k=3, trace=FALSE, maxiter=10, change=0.001){
  
  # OUTPUT INITIALISIERUNG:
  iter <- 0
  zentren <- NULL
  index <- rep(-1,nrow(x))
  distanz <- rep(-1,nrow(x))
  
  # weitere intialisierungen:
  distanzensumme <- 0
  relativeAenderung_DistanzenSumme <- change+1
  group_means <- data.frame(xvalue=rep(0,k), yvalue=rep(0,k))
  
  if (trace == TRUE) {
    x11(180, 80)
    par(mfrow = c(2, ceiling((maxiter + 1)/2)))
    par(bg = "gray97")
    farben <- vector(mode = "character", length = k)
    #farben <- distinctColorPalette(k)
    #farben <- c("black", "red", "green", "yellow", "gray", "pink", "purple",
    #           "antiquewhite4", "aquamarine", "azure3", "cadetblue", "chartreuse3",
    #          "coral3", "darkblue", "darkorange3")
    vek <- c(LETTERS[1:6], 0:9)
    for (i in 1:k) {
      farben[i] <- paste0("#", paste0(sample(vek, size = 6), collapse = ""))
    }
  }
  
  
  # SCHRITT 1: 
  # waehle zufaellig k punkte aus beobachtungen als startloesung fuer die gruppenmittelwerte
  # stelle sicher, dass keine identischen beobachtungspaare unter den k ausgewaehlten punkten
  
  if(k > nrow(x)){
    stop("k muss kleiner als anzahl der beobachtungen sein!")
  }
  
  x_unique <- x[!duplicated(x), ] # duplikate herausnehmen
  randomStartingRowIndices <- sample(nrow(x_unique), size=k, replace=FALSE)
  randomStartingPoints <- x_unique[randomStartingRowIndices, ]
  rownames(randomStartingPoints) <- 1:k
  
  # plot wenn trace ist TRUE
  if(trace == TRUE) {
    plot(x, xlab = '', ylab = '', main = 'Start', col = "lightcyan3")
    points(randomStartingPoints, pch = 4, col = farben[1:k], lwd = 3, cex = 1.5)
  }
  
  # SCHRITT 2:
  # bestimme faer jeden punkt die euklidschen distanzen zu den aktuellen gruppenmittelwerten
  while(iter < maxiter && relativeAenderung_DistanzenSumme >= change){ # check abbruchbedingungen (i.e. schritt 4)
    
    for(i in 1:nrow(x)){ # i ... anzahl der beobachtungen
      
      
      distanzenZuClustern_proBeobachtung <- rep(0,k) 
      
      for(j in 1:k){ # j ... anzahl der k zu bildenden gruppen
        
        ## berechne euklidsche distanzen
        if(iter==0){
          
          distanzenZuClustern_proBeobachtung[j] <- sqrt( (x[i,1]-randomStartingPoints[j,1])^2 +
                                                           (x[i,2]-randomStartingPoints[j,2])^2 )
        }
        else{
          
          distanzenZuClustern_proBeobachtung[j] <- sqrt( (x[i,1]-group_means[j,1])^2 +
                                                           (x[i,2]-group_means[j,2])^2 )
        }
        
      } 
      
      
      ## setze distanz (distanz zum gewaehlten cluster-mittelpunkt)
      distanz[i] <- min(distanzenZuClustern_proBeobachtung)
      
      ## setze index (clusterzuordnung gemaess minimaler distanz)
      index[i] <- which(distanzenZuClustern_proBeobachtung == distanz[i])[1]
      
    }
    
    
    ## distanzsumme und relative aenderung davon in laufender iteration
    if(iter!=0){
      relativeAenderung_DistanzenSumme <- abs(distanzensumme - sum(distanz)) / distanzensumme
    }
    
    distanzensumme <- sum(distanz)
    
    
    ## SCHRITT 3: bestimme aufgrund von aktueller gruppenzugehoerigkeit der datenpunkte fuer jede 
    ##            der k gruppen durch anwendung von "mean" neue gruppenmittelwerte
    
    for(a in 1:k){
      rowIndex_groupK <- which(index==a)    
      group_means[a,1] <- mean(x[rowIndex_groupK,1]) # xvalue means
      group_means[a,2] <- mean(x[rowIndex_groupK,2]) # yvalue means
    }
    
    if (trace == TRUE) {
      plot(x, xlab = '', ylab = '', main = paste("Iteration", iter + 1), col = farben[index])
      points(group_means, col = farben[1:k], pch = 4, lwd = 4, cex = 1.5)
    }
    
    ## iterationsschritte-anzahl erhoehen
    iter <- iter + 1
    
  }
  
  
  ## RETURN liste mit iter, zentren, index, distanz
  list(iter=iter, zentren=group_means, index=index, distanz=distanz)
  
}



# TEST AUFRUF
ergebnis <- KMEANS(daten, 10, FALSE, 10, 0.001)
ergebnis

ergebnis <- KMEANS(daten, 5, TRUE, 10, 0.001)
ergebnis


# SIMULIERTE DATENSATZ .
# generieren Sie 4 Stichproben mit je 25 Beobachtungen (insgesamt n=100), und folgenden
# Mittelwerten (-1,1), (-1,-1), (1,1), (1,-1), die Werte für die beiden Variablen sollen jeweils um
# den Mittelwert normalverteilt mit Standardabweichung 1 sein.

set.seed(100)

stichprobe1 <- data.frame(xvalue = rnorm(25, mean = -1, sd = 1),yvalue= rnorm(25, mean = 1, sd =1))
KMEANS(stichprobe1, 3,TRUE, 10, 0.001)
stichprobe2 <- data.frame(xvalue = rnorm(25, mean = -1, sd = 1),yvalue= rnorm(25, mean = -1, sd =1))
KMEANS(stichprobe2, 3,TRUE, 10, 0.001)

stichprobe3 <- data.frame(xvalue = rnorm(25, mean = 1, sd = 1),yvalue= rnorm(25, mean = 1, sd =1))

stichprobe4 <- data.frame(xvalue = rnorm(25, mean = 1, sd = 1),yvalue= rnorm(25, mean = -1, sd =1))

Random_data <- rbind(stichprobe1,stichprobe2, stichprobe3, stichprobe4)
plot(Random_data)
KMEANS(Random_data,4,TRUE,10,0.001)



# TEST-CODE FUER OPTIONALE ZUSATZLEISTUNG
#install.packages(('cluster'))
library(cluster)
daten <- faithful
km <- KMEANS(daten,3)
dissE <- daisy(daten)
sk <- silhouette(km$index, dissE)
plot(sk)


# silhouettenplot und -koeffizient
anz <- 8
farben <- distinctColorPalette(anz)
dis <- dist(daten)^2
res <- KMEANS(daten, k = anz)
sil <- silhouette (res$index, dis)
windows() 
plot(sil, col = farben, xlab= "The silhouette coefficient values",main = paste("Silhouette analysis for KMeans clustering on sample data with n_cluster =", max(res$index)))


# Code fuer Silouttenkoeffizienten und -plots ohne zusaetzliche libraries

##-----------------------------------------------------
## FUNCTION silhoutten:
##-----------------------------------------------------

# Diese Funktion berechnet die Silhouttenwerte & Silhouettenkoeffizienten 
# von einem gruppierten Datensatz. Die Silhouttenwerte werden mithilfe eines
# Silhouttenplots dargestellt.

# Silhouttenwerte:
# s(i) = (b[i] - a[i])/max{a[i], b[i]}, wobei
# a ... durchschnittliche distanz zwischen jedem punkt und allen restlichen punkten im selben cluster
# b ... minimale mittlere distanz von jedem punkt i zu allen anderen punkten in einem anderen cluster, 
#       in dem i nicht liegt.

# input-parameter:
#   x ... dataframe (dimension n x 2, d.h. n rows, 2 cols)
#   erg ... list mit Ergebnissen vom KMEANS-Algorithmus


# output (list):
#   Werte ... vektor der laenge n (enthaelt fuer jeden datenpunkt info, zu welcher teilmenge er gehoert)
#   Summary ... summary von Silhouttenwerten
#   Koeffizienten ... Silhouttenkoeffizienten von Clusters
# und plots:
#   barplot von Silhouttenwerten
#   plot von geclusterten Punkten

silhouetten <- function(x = daten, erg = ergebnis) {
  # wieviele clusters 
  k <- length(unique(erg$index))
  # Initialisiere die Silhouttenwerte: Vektor lauter 0
  silloutten_werte <- numeric(nrow(x))
  
  for (i in 1:nrow(x)) {
    # mittlere Distanz von einem Punkt bis zu anderen in demselben Cluster
    bool <- erg$index == erg$index[i]
    pkt <- cbind(rep(x[i, 1], times = nrow(x)), rep(x[i, 2], times = nrow(x)))
    
    dist_within <- sqrt(rowSums((pkt[bool,] - x[bool, ])^2))
    a <- mean(dist_within[dist_within != 0])
    
    # kleinste mittlere Distanz von einem Punkt bis zu anderen in restlichen Clustern
    dist_nextgroup <- numeric(k)
    vek <- 1:k
    vek <- vek[vek != erg$index[i]] # alle Clustern ausser eigener
    for (j in vek) {
      bool <- erg$index == j
      dist_nextgroup[j] <- mean(sqrt(rowSums((pkt[bool, ] - x[bool, ])^2)))
    }
    
    b <- min(dist_nextgroup[dist_nextgroup != 0])
    
    silloutten_werte[i] <- (b - a)/max(a, b)
  }
  
  # Dataframe fuer Plot und Berechnung von Koeffizienten
  yy <- cbind(erg$index, silloutten_werte)
  yy <- yy[order(yy[, 1], yy[, 2]),] # nach Cluster und Wert sortieren
  yy <- as.data.frame(yy)
  
  # Silhouttenkoeffizienten
  n_in_cluster <- tapply(yy[, 2], yy[, 1], length)
  sil_koef <- tapply(yy[, 2], yy[, 1], mean)
  
  # Vorbereitung fuer Plot
  x11(160, 70)
  par(mfrow = c(1, 2))
  par(bg = "gray97")
  farben <- vector(mode = "character", length = k)
  #farben <- distinctColorPalette(k)
  #farben <- c("antiquewhite4", "chartreuse3", "aquamarine", "darkorange3", "cadetblue", "coral3",
  #             "darkblue","azure3", "black", "red", "green", "yellow", "gray", "pink", "purple")
  vek <- c(LETTERS[1:6], 0:9)
  for (i in 1:k) {
    farben[i] <- paste0("#", paste0(sample(vek, size = 6), collapse = ""))
  }
  # Silhouttenplot
  barplot(yy[, 2],  space = 0, horiz = TRUE, col = farben[yy[, 1]], border = NA, 
          main = paste("Silhouettenplot fuer ", k, "clusters"), 
          xlab = "Silhouttenwerte", ylab = "Clusters", 
          xlim = c(min(yy[, 2]), 1.5))
  legend("topright", legend = paste(1:k, ":", n_in_cluster, "|", round(sil_koef, digits = 3)), 
         col = farben[1:k], lwd = 3, title = "Silhouttenkoeffizienten")
  
  # Geclusterter Datensatz
  plot(x, xlab = '', ylab = '', main = "Clustered points", col = farben[erg$index])
  points(erg$zentren, col = farben[1:k], pch = 4, lwd = 4, cex = 1.5)
  
  return(list(Werte = silloutten_werte, Summary = summary(silloutten_werte), 
              Koeffiziente = sil_koef))
}


# Ausführung der function silhouetten:

ergebnis <- KMEANS(daten, 7, FALSE, 10, 0.0001)
ergebnis$iter
silhouetten(x = daten, erg = ergebnis)

ergebnis <- KMEANS(Random_data, 4, FALSE, 15, 0.001)
ergebnis$iter
silhouetten(x = Random_data, erg = ergebnis)
