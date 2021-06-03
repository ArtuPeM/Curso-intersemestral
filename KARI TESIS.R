library(xlsx)
datos <- read.xlsx("CAMDEX-TESIS.xlsx", header = TRUE, sheetIndex = 1)
datos <- na.omit(datos)
zoom <- datos[datos$GRUPO. == "Zoom",]
presencial <- datos[datos$GRUPO.=="Presencial",]
names(zoom)

##ZOOM
t.test(zoom[zoom$EDAD== "Adultos", 6],
       zoom[zoom$EDAD=="JÃ³venes", 6])$statistic

pvaluesZ <- function(x){
        y <- numeric()
        for (i in c(6,7, 8, 10, 11, 12)) {
                jovenes <-  x[x$EDAD=="JÃ³venes", i]
                adultos <- x[x$EDAD== "Adultos", i]
                y <- c(y, t.test(jovenes, adultos)$p.value)
        }
        y
}
pvaluesZ(zoom)
tvaluesZ(zoom)

bonferroni <- 0.05/6
sum(pvaluesZ < bonferroni)
which(pvaluesZ < bonferroni)
#medias
grupoZ <- split(zoom, zoom$EDAD)
lapply(grupoZ, function(x)colMeans(x[,6:12]))
#sd


apply(grupoZ$Adultos[,5:12], 2, sd)
apply(grupoZ$JÃ³venes[,5:12], 2, sd)

##Presenciales
t.test(presencial[presencial$EDAD=="JÃ³venes", 11], 
       presencial[presencial$EDAD== "Adultos", 11])$p.value
pvaluesP <- numeric(6)
for (i in c(6,7,8,11,12)) {
        pvaluesP <- numeric(6)
     pvaluesP <- c(pvaluesP, t.test(presencial[presencial$EDAD=="JÃ³venes", i], 
                           presencial[presencial$EDAD== "Adultos", i])$p.value)
}
pvaluesP <- na.omit(pvaluesP)
pvaluesP <- c(0.8808791, 0.3739010, 0.2629894, 0.8808791, 0.3910022)
bonferroni <- 0.05/5
sum(pvaluesP < bonferroni)
which(pvaluesP < bonferroni)

##CLUSTER

set.seed(1234)
dat <- data.frame(x=rnorm(12, mean = rep(1:3, each = 4), 0.2), 
                  y=rnorm(12, mean = rep(c(1,2,1, each=4)),0.2))
distance <- dist(dat)
clustert <- hclust(distance)
plot(cluster)

png("clusters.png")
par(mar=c(2,4,4,2), mfrow=c(2,2))
#zoom
distancia <- dist(zoom[,8])
cluster <- hclust(distancia)
plot(cluster, main = "Zoom/memoria")
#zoom de dimensiones
distancia <- dist(t(zoom[6:12]))
cluster <- hclust(distancia)
plot(cluster, main="Zoom/dimensiones")

#PRESENCIAL
#memoria
distancia <- dist(presencial[,8])
cluster <- hclust(distancia)
plot(cluster, main = "presencial/memoria")
#dimensiones
distancia <- dist(t(presencial[6:12]))
cluster <- hclust(distancia)
plot(cluster, main= "presencial/dimensiones", xlab = "")
dev.off()

elcluster()
elcluster <- function(hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1, ...){
     y <- rep(hclust$height,2)
     x <- as.numeric(hclust$merge)
     y <- y[which(x<0)]
     x <- c[which(x<0)]
     x <- abs(x)
     y <- y[order(x)]
     x <- x[order(x)]
     plot(hclust, labels=FALSE, hang=hang, ...)
     test(x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order], col=lab.col[hclust$order], str=90, adj=c(1,0.5), xpd=NA, ...)
}

