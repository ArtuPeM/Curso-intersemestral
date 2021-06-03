##RESPUESTAS DE LA SECCIÓN DOS DEL CUESTIONARIO 1
##hospitalizados link 2
URL <- "https://datos.cdmx.gob.mx/dataset/b0d4230e-f37b-463e-8c16-3565aa78cbfc/resource/8b29f1ab-6245-42f1-878b-78e9a4b02374/download/personas_hospitalizadas_con_diagnostico_covid19-series_totales.csv"
download.file(URL, destfile = "./data/covid19CDMXhospitalizados.csv")
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

x <- 5
while (x>=3 & x<=10 ) {
     x <- x+.5
}