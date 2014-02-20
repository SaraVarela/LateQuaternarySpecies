# graphics for the paper 

setwd ("C:\\Users\\sara\\Documents\\_CIENCIAS\\human_climate_Goiania")
dato<- read.table ("results_experiments.csv", sep=",", header=T)
names (dato)
niches<- read.table ("niches_medias.csv", sep=",") 
colnames (niches)[1:2]<- c("mean", "niche.breadth")

datos<- data.frame (niches, dato)
datos [1,c(4,5)]<- c(1, 0)

colo<- colorRampPalette (c("dodgerblue4", "white", "firebrick3"))


boxplot (-datos$reduction_climate*100 ~ datos$mean, 
         ylab="% range shift after climatic change", xlab="Species temperature optimum", 
         axes=F, 
         col=colo(10), border="darkgrey")

axis (1, at=c(1:10), col="grey", 
      labels =c( "-20", "-15", "-10", "-5", 
                 "0", "5", "10", "15", "20", "25"))
axis (2, col="grey")
text (x=1, y=180, "A", cex=2)

boxplot (-datos$reduction_homo*100 ~ datos$mean, 
         ylab="% range shift after early human impact", 
         xlab="Temperature optimum", 
         axes=F, 
         col=colo(10), border="darkgrey")

axis (1, at=c(1:10), col="grey", 
      labels =c( "-20", "-15", "-10", "-5", 
                 "0", "5", "10", "15", "20", "25"))
axis (2, col="grey")
text (x=1, y=-4, "B", cex=2)

## other plots 

boxplot (-datos$reduction_homo*100 ~ datos$niche.breadth, 
         ylab="% Range shift after human impact", 
         xlab="Temperature optimum", 
         axes=F, 
         col=colo(10), border="darkgrey")

axis (1, at=c(1:4), col="grey", 
      labels =c( "5", "10", "15", "20"))
axis (2, col="grey")

boxplot (-datos$reduction_climate*100 ~ datos$niche.breadth, 
         ylab="% Reduction", 
         xlab="Temperature optimum", 
         main="Human impact", axes=F, 
         col=colo(10), border="darkgrey")

axis (1, at=c(1:4), col="grey", 
      labels =c( "5", "10", "15", "20"))
axis (2, col="grey")

cor.test(datos$reduction_climate*100, datos$niche.breadth)
cor.test (datos$reduction_homo*100, datos$niche.breadth)

cor.test(datos$reduction_climate*100, datos$mean)
cor.test(datos$reduction_homo*100, datos$mean)

