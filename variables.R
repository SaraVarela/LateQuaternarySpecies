library (raster)
library (rgdal)
#read variables

setwd ("C:\\Macroecology\\CCSM_21")
variables_LGM<- raster("bio1.bil")
for (i in 2:19)
{
  layer <- raster(paste("bio",i,".bil",sep=""))
  variables_LGM<- stack(variables_LGM,layer)
}

setwd ("C:\\Macroecology\\worldclim")
variables_present<- raster("bio1.bil")
for (i in 2:19)
{
  layer <- raster(paste("bio",i,".bil",sep=""))
  variables_present<- stack(variables_present,layer)
}
# crop variables
e<- extent(-10,50, 30, 80)
eurasia<- crop (variables_present, e)
eurasia_LGM<- crop (variables_LGM, e)

# show map with the differences of climate change in Europe
dif<- (eurasia - eurasia_LGM)/10

colores<- colorRampPalette(c("grey90", "grey20", "grey10"))(100)

plot (dif [[1]], axes=F, box=F, col=colores)

title ("Increase in Annual Mean Temperature after LGM", "CCSM GCM")

# read human fossil sites and plot them

library (rgdal)
library (raster)
setwd ("C:/Users/sara/Documents/_CIENCIAS/pbdb")
wmap <- readOGR(dsn="maps/ne_110m_land.shp", layer="ne_110m_land")
eurasia_map<- crop (wmap, e)
par (mar =c(0,0,0,0))
plot (eurasia_map, col="grey", border=F)

setwd ("C:\\Users\\sara\\Documents\\_CIENCIAS\\human_climate_Goiania")

homo<- read.table ("homo.csv", sep=",", 
                   header=T, stringsAsFactors=FALSE)

humans<- homo[homo$Corrected.date<24000, 1:2]
xyz<- cbind (humans, 1)
names (xyz)<- c("x", "y", "z")
r<- reclassify (eurasia[[1]], c(-Inf, +Inf, 0))
mask<- reclassify (eurasia[[1]], c(-500, +Inf,1))
plot (mask)

humanos<- rasterize (as.matrix(humans), r, fun="count")

hum<- reclassify (humanos, c(0,0,NA))
# para hacer la capa de humanos
b_hum<- buffer(hum, width=200000)

colores<- colorRampPalette(c("grey20", "grey20", "grey20"))(100)
colore<- colorRampPalette(c("grey70", "grey70", "grey70"))(100)

par (mar=c(0,0,0,0))
plot (dif [[1]], axes=F, box=F, col="grey60", legend=F)
plot (b_hum, xlim=c(-50, 80), col=terrain.colors(25, alpha=0.50), add=T, legend=F)
points (xyz$x, xyz$y, pch=20, cex=1.5, ylim=c(30, 70), xlab="", 
      ylab="", col="grey10")



## species 

setwd ("C:\\Users\\sara\\Documents\\_CIENCIAS\\human_climate_Goiania")
sp<- function (media, desv)
{
  infe<- (media*10) - (desv*10)
  supe<- (media*10) + (desv*10)
  
  mapa<- reclassify (eurasia[[1]], c(-Inf, infe, 0, infe, supe, 1, supe, +Inf, 0))
  mapa21<- reclassify (eurasia_LGM[[1]], c(-Inf, infe, 0, infe, supe, 1, supe, +Inf, 0))
  humans<- mapa*b_hum4
  
  
  nombre<- c(paste ("Mean =", media, "; Niche Breadth =", desv, sep=" " ))
  nombre2<- c(paste ("Mean_", media, "_NB_", desv, sep=""))
  
  tiff (paste (nombre2, ".tiff", sep=""))
  par(mar = c(0, 0, 0.5, 0), mfrow = c(2,3)) 
  plot (mapa21, axes=F, box=F, legend=F, colNA="lightblue")
  title ("21 KYBP: Potential range")
  plot (mapa, axes=F, box=F, legend=F, colNA="lightblue")
  title ("D: Climate Change")
  plot (humans_d, axes=F, box=F, legend=F, colNA="lightblue")
  title ("D: Climate + Humans") 
  plot (humans, axes=F, box=F, legend=F, colNA="lightblue")
  title ("ND: Niche shift + Human Impact")
  plot (climate, axes=F, box=F, legend=F, colNA="lightblue")
  title ("ND: Climate change")
  plot (both,axes=F, box=F, legend=F, colNA="lightblue")
  title ("ND: Climate + Humans")               
  mtext(nombre, line= 0, side = 4, outer = F, cex=0.9)
  dev.off()   
}

