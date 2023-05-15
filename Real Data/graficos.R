load('Real Data/Yfdata.Rdata')
load('Real Data/r1.Rdata')
load('Real Data/r2.Rdata')
load('Real Data/r3.Rdata')
load('Real Data/cropeado.Rdata')
load('Real Data/df.Rdata')
load('Real Data/y.Rdata')

# Gráfico de temperatura para todos los meses
ty <- as.data.frame(t(y))
ty$x <- dff[,1]
ty$y <- dff[,2]

library(ggplot2)
library(reshape2)

# Convert the data to long format
melted_data <- melt(ty, id.vars = c("x", "y"))

# Create a common scale for all plots
zlim <- range(melted_data$value)
mes <- unique(melted_data$variable)

my_palette <- colorRampPalette(c("blue", "pink", "red"))(n = 100)
# Create a list of ggplot objects, one for each month
plots <- list()
for (i in 1:12) {
  plot_data <- subset(melted_data, variable == mes[i])
  plot_title <- paste("Temperature for Month", i)
  plot <- ggplot(plot_data)+ theme_classic()+
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line = element_line(color = "white")
    )+
    scale_x_continuous(NULL) + 
    scale_y_continuous(NULL) +
    geom_raster( aes(x = x, y = y, fill = value)) +
    coord_equal() +
    scale_fill_gradientn(name = "LST", colours = my_palette, limits = zlim) +
    labs(title = plot_title) 
  plots[[i]] <- plot
}

# Combine all the plots into a single plot using gridExtra
library(gridExtra)
grid.arrange(grobs = plots, ncol = 3)


# Grafico de datos funcionales

matplot(Y,type = 'l',col='#424242',xlab = 't',ylab = 'LST')

# Graficos de outliers en andalucia
library(raster)

spain.sp <- readRDS("Real Data/gadm36_ESP_1_sp.rds")
dem <- stack('Real Data/LST_Spain.tif')
s.na <- function(x) {
  sum(is.na(x))  
}
nadf <- apply(df[,3:264],1,s.na)
dff <- df[-which(nadf==262),]

andalucía <- spain.sp[spain.sp$NAME_1=='Andalucía',]
#str(andalucía)
andalucía <- spTransform(andalucía, CRS(proj4string(dem)))



k <- seq(1.5,2,by=0.25)
df_kl2 <- list() 
for(i in 1:length(k)){
    aux <- (r1$MDEF > (k[i] * r1$norm_MDEF))
    aux2 <- rep(0,length(r1$MDEF))
    aux2[aux] <- 1
    df_kl2[[i]] <- aux2
}

df_koptl2 <- list()
for(i in 1:length(k)){
   
    aux <- (r2$MDEF > (k[i] * r2$norm_MDEF))
    aux2 <- rep(0,length(r2$MDEF))
    aux2[aux] <- 1
    df_koptl2[[i]] <- aux2
}

df_krl2 <- list() 
for(i in 1:length(k)){
   
    aux <- (r3$MDEF > (k[i] * r3$norm_MDEF))
    aux2 <- rep(0,length(r3$MDEF))
    aux2[aux] <- 1
    df_krl2[[i]] <- aux2
}

ol2 <- which(df_kl2[[2]]==1)
oopt <- which(df_koptl2[[2]]==1) 
orl2 <- which(df_krl2[[2]]==1)

length(which(df_kl2[[2]]==1 & df_koptl2[[2]]==1 & df_krl2[[2]]==1)) # cantidad de outlier detectados por los 3 metodos
length(which(df_kl2[[2]]==1 & df_koptl2[[2]]==1) ) # cantidad de outlier detectados por los metodos sL2 y optL2
length(which(df_kl2[[2]]==1  & df_krl2[[2]]==1))# cantidad de outlier detectados por los 3 metodos sL2 y L2
length(which(df_koptl2[[2]]==1 & df_krl2[[2]]==1))# cantidad de outlier detectados por los 3 metodos optL2 y L2

puntosl2 <- dff[ol2,1:2]
puntosoptl2 <- dff[oopt,1:2]
puntosrl2 <- dff[orl2,1:2]

# Graficar Andalucía
plot(andalucía, main = "Andalucía")

# Agregar los puntos al gráfico
points(puntosl2$x, puntosl2$y, col = "red", pch = 16, cex = 1)
points(puntosoptl2$x, puntosoptl2$y, col = "blue", pch = 16, cex = 1.5)
points(puntosrl2$x, puntosrl2$y, col = "black", pch = 16, cex = 1.5)

#################################################################################

library(maptools)
library(rgeos)

andalucía1 <- as(andalucía,"SpatialPolygons")


xy <- dff[,1:2]
grid.pts<-SpatialPointsDataFrame(coords= xy, data=xy)
gridded(grid.pts) <- TRUE
grid <- as(grid.pts, "SpatialPolygons")
plot(grid)
#grid=raster::intersect(grid,andalucía1)
proj4string(grid)=proj4string(dem)
grid=as(grid,"SpatialPolygons")
areas=gArea(grid,byid = T)
plot(grid)
grid.copy <- grid

grid_union=unionSpatialPolygons(grid.copy,IDs=rep(1,length(grid.copy)))
plot(grid_union)

## grafico de andalucia de acuerdo al los outliers que detecta cada método

combinaciones1=rbind(c(0,0,0),c(1,0,0),c(0,1,0),c(0,0,1),c(1,1,0),c(0,1,1),
                     c(1,0,1),c(1,1,1))

#postscript(paste0("Map_risk1_.eps"),family="Helvetica")
plot(grid_union,border="#898989")
#title('(a)',line=-1)
colores=c("#21C1BD","#FF00FF","#ffd700","#984ea3","#196B2C","#ff7f00","#e31a1c")
for (i in 2:nrow(combinaciones1)){
  
  celdas=which(df_kl2[[2]]==combinaciones1[i,1] &
                 df_koptl2[[2]]==combinaciones1[i,2] &
                 df_krl2[[2]]==combinaciones1[i,3])
  plot(grid[celdas,],col=colores[i-1],add=T,border="white")
  
}


pos_leyenda="bottomright"
legend(pos_leyenda, legend=c("LOCI_sL2","LOCI_optL2","LOCI_L2","sL2-optL2","sL2-L2","optL2-L2","sL2-optL2-L2"),
       fill=colores, cex=0.7, title=NULL,ncol=4)

dev.off()


# gráfico MDEF en andalucia
library(ggthemes)
library(tidyverse)

dff[,265] <- r1$MDEF
dff <- dff %>% rename (MDEF = V265)
ggplot() + theme_classic()+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_line(color = "white"),
        plot.title=element_text(hjust=0.5, face='bold', color='black')
  )+
  #labs(title='(b)')+
  scale_x_continuous(NULL) + 
  scale_y_continuous(NULL) +
  geom_raster(data = dff[,c(1,2,265)] , aes(x = x, y = y, fill = MDEF)) +
  coord_equal() +
  scale_fill_gradient(low = "white", high = "orange", na.value = NA)#colours = rev(terrain.colors(5)))


# Grafico de outliers detectados con LOCI_sL2 en datos funcionales

matplot(Y,type = 'l',col='#424242',xlab = 't',ylab = 'LST')
matlines(Y[, ol2], type = "l",
        col = 4)
