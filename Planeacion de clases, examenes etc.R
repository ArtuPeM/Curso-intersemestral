URL <- "https://datos.cdmx.gob.mx/explore/dataset/covid-19-sinave-ciudad-de-mexico-a-nivel-colonia/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C"
dir.create("data")
download.file(URL, destfile = "./data/covid19CDMX.csv")
covidData <- read.csv("./data/covid19CDMX.csv")
#Capacidad hospitalaria
URL2 <- "https://datos.cdmx.gob.mx/explore/dataset/capacidad-hospitalaria/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C"
download.file(URL2, destfile = "./data/capacidadcovid19CDMX.csv" )
CapacidadHost <- read.csv("./data/capacidadcovid19CDMX.csv")

##hospitalizados link 2
URL4 <- "https://datos.cdmx.gob.mx/dataset/b0d4230e-f37b-463e-8c16-3565aa78cbfc/resource/8b29f1ab-6245-42f1-878b-78e9a4b02374/download/personas_hospitalizadas_con_diagnostico_covid19-series_totales.csv"
download.file(URL4, destfile = "./data/covid19CDMXhospitalizados.csv")
hospitalizados <- read.csv("./data/covid19CDMXhospitalizados.csv")
hospitalizados <- hospitalizados[hospitalizados$aÃ.o==2020,]
names(hospitalizados)

#21
ncol(hospitalizados)
#22
nrow(hospitalizados)
#23
sum(is.na(hospitalizados))
#24
hospitalizados[150,7]
#25
mean(hospitalizados$hospitalizados_totales_cdmx)
#26
mean(hospitalizados[hospitalizados$mes=="diciembre", "hospitalizados_totales_cdmx"])

## estructuas basicas de control
#27
x <- 1
if (x >= 1){y <- 2+x} else{y<-2*x}
#28
x <- vector("numeric", 1000)
set.seed(1)
for (i in 1:1000) {
     y <- rbinom(1,1,.5)
     if (y >= 1){x[i] <- 2+y} else{x[i]<-2*y}
}

sum(x==3)/length(x)

x <- 5
while (x>=3 & x<=10 ) {
     x <- x+.5
}

#funciones
estadisticos <- function(columna){
     media <- mean(columna)
     mediana <- median(columna)
     sd <- sd(columna)
     return(list("media"=media, "mediana"=mediana, "sd"=sd))
}
#1
estats <- apply(hospitalizados[,5:13],2,estadisticos)
sapply(hospitalizados[,5:13], estadisticos)
mapply(estadisticos, hospitalizados[,5:13])
#2
estats$camas_generales_cdmx["sd"]
#3
estats$hospitalizados_totales[2]
#4
estats$camas_intubados_cdmx["media"]
#5
meses <- factor(hospitalizados$mes, 
                levels = c("marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"))
estatsmes <- tapply(hospitalizados$hospitalizados_totales_cdmx, meses , estadisticos)
estatsmes2 <- lapply(split(hospitalizados, meses), function(x) estadisticos(x [ , "hospitalizados_totales_cdmx"]))

#6
estatsmes$noviembre["media"]
#7
estatsmes$diciembre["sd"]
#8
estatsmes$junio["mediana"]
#9
as.numeric(estatsmes$junio["mediana"]) - as.numeric(estatsmes$junio["media"])
#10
plot(xtabs(hospitalizados_totales_cdmx~meses, data = hospitalizados), ylab = "No. hospitalizados CDMX", type = "l"); lines(tapply(hospitalizados$camas_generales_cdmx,meses, sum ), col="blue"); lines(tapply(hospitalizados$camas_intubados_cdmx, meses, sum), col = 3); abline(h=max(tapply(hospitalizados$camas_generales_cdmx,meses, sum )), lty=2, col="red")

##ploting
dir.create("plots")
#1
png(filename = "boxhospitalizadosCDMX.png", width = 900)
boxplot(hospitalizados_totales_cdmx~meses, data = hospitalizados, ylab = "hospitalizados totales CDMX")
plot(hospitalizados$hospitalizados_totales_cdmx~meses,  ylab = "hospitalizados totales CDMX")
dev.off()


stripchart(hospitalizados_totales_cdmx~meses, data = hospitalizados,
           vertical=TRUE,method="jitter",pch=20, cex=0.1, add=TRUE)
#3
png(filename = "histhospitCDMX.png")
hist(hospitalizados$hospitalizados_totales_cdmx, main = "hospitalizados CDMX", xlab = "")
abline(v=mean(hospitalizados$hospitalizados_totales_cdmx), lty=2, col="red")
dev.off()
rug(hospitalizados$hospitalizados_totales_cdmx)

?plot
plot(meses, hospitalizados$hospitalizados_totales_cdmx, col=0, type="n")
abline(h=(max(hospitalizados$camas_generales_cdmx)), col="red")
abline(h= max(hospitalizados$camas_intubados_cdmx), col="blue")

png(filename = "./plots/total de hospitalizados.png", width = 1000)
plot(xtabs(hospitalizados_totales_cdmx~meses, data = hospitalizados), ylab = "No. hospitalizados CDMX", type = "l")
lines(tapply(hospitalizados$camas_generales_cdmx,meses, sum ), col ="blue")
lines(tapply(hospitalizados$camas_intubados_cdmx, meses, sum), col = 3)
abline(h=max(tapply(hospitalizados$camas_generales_cdmx,meses, sum )), lty=2, col="red")
legend("topleft",lty =c(1,1,1,2) , col = c("black", "blue", 3, "red"), 
       legend=c("hospitalizados", "camas ocupadas generales", "personas intubadas", "capacidad max. hosp."))
dev.off()

tapply(hospitalizados$camas_intubados_cdmx, meses, sum)+ tapply(hospitalizados$camas_generales_cdmx,meses, sum )

marjul <- factor(as.character(levels(meses)[1:5]), levels = c("marzo", "abril", "mayo",  "junio", "julio"))

png(filename = "barhospitalizadosCDMX.png", width = 1000)
barplot(height = tapply(hospitalizados$hospitalizados_totales_cdmx,meses, sum), ylim = c(0,160000), 
        main = "No. hospitalizados CDMX", col = 4)
abline(h=max(tapply(hospitalizados$camas_generales_cdmx,meses, sum )), lty=2)
dev.off()

#5        
png(filename = "./plots/scattercamasintubadosCDMX.png")
with(hospitalizados[hospitalizados$mes %in% c("junio", "diciembre"),], 
     plot(camas_intubados_cdmx, camas_generales_cdmx, type = "n", xlab = "", ylab = ""))
with(subset(hospitalizados, meses == "junio"), points(camas_intubados_cdmx, camas_generales_cdmx, col="blue"))     
with(subset(hospitalizados, meses == "diciembre"), points(camas_intubados_cdmx, camas_generales_cdmx, col="red"))     
legend("topleft",pch = 1 , col = c("blue", "red"), 
       legend=c("junio", "diciembre"))
title("CDMX", xlab = "No. de personas intubadas", ylab = "No. camas generales ocupadas")
dev.off() 
names(hospitalizados)

#7
png(filename = "./plots/textlabels.png", width = 800)
plot(tapply(hospitalizados$hospitalizados_totales_cdmx,meses, mean),
     pch=20, xlab = "meses", ylab = "Prom. Hospitalizados CDMX")
text(tapply(hospitalizados$hospitalizados_totales_cdmx,meses, mean)-200,
     labels = c("marzo", "abril", "mayo", "junio", "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"), cex = .8)
dev.off()

abline(h=mean(hospitalizados$camas_generales_cdmx), col="red", lty=2)
lines(tapply(hospitalizados$hospitalizados_totales_cdmx,meses, max), col="blue")
lines(tapply(hospitalizados$hospitalizados_totales_cdmx,meses, min), col=4)

as.character(meses[grepl(".*", meses)])