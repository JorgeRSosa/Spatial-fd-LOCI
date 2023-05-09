library(raster)
#library(rsat)
#library(sf)
library(rgdal)
library(ggplot2)
#library(tidyverse)

library(cluster)
library(vows)
library(clusterSim)
library(fossil)
library(geofd)
library(geoR)
library(MASS)
library(fda)
library(fda.usc)
library(fdasrvf)

source('Fijo/nuevo LOCI.R')

dem <- stack('Real Data/LST_Spain.tif')

spain.sp <- readRDS("/Real Data/gadm36_ESP_1_sp.rds")

plot(spain.sp)

andalucía <- spain.sp[spain.sp$NAME_1=='Andalucía',]
#str(andalucía)
andalucía <- spTransform(andalucía, CRS(proj4string(dem)))

nay_mask <- mask(dem, mask = andalucía)

cropeado <- crop(x = nay_mask, y = andalucía)
plot(cropeado[[1]])
save(cropeado,file = 'Real Data/cropeado.Rdata')
load('Real Data/cropeado.Rdata')
df <- as.data.frame(merge(raster::as.data.frame(cropeado[[1]], xy = TRUE),raster::as.data.frame(cropeado[[2]], xy = TRUE)),by=c('x','y'))

for (i in 3:262) {
  df <- merge(df,raster::as.data.frame(cropeado[[i]], xy = TRUE),by=c('x','y'))
}

ggplot() +
  geom_raster(data = df[,1:3] , aes(x = x, y = y, fill = LST_Spain_1)) +
  coord_equal() +
  scale_fill_gradientn(colours = rev(terrain.colors(10)))


save(df,file = 'Real Data/df.Rdata')
load('Real Data/df.Rdata')

s.na <- function(x) {
  sum(is.na(x))  
}
nadf <- apply(df[,3:264],1,s.na)
dff <- df[-which(nadf==262),] #considero únicamente los pixeles de España

View(dff[which(apply(dff,1,s.na)!=0),]) # no hay valores perdidos
# m.na  <- function(x) {
#   mean(x,na.rm=T)
# }
# dfmean <- apply(dff[,3:264], 1, m.na)
# dff[,3] <- ifelse(is.na(dff[,3]),dfmean,dff[,3]) 
# dff[,4] <- ifelse(is.na(dff[,4]),dfmean,dff[,4])
# dff[,5] <- ifelse(is.na(dff[,5]),dfmean,dff[,5])

aux <- dff[,3:264]
aux1 <- aux[,12:251]
aux20000 <- aux[,1:11]
aux2021 <- aux[,252:262]
dfene <- as.data.frame(aux1[,1])
for (i in 1:19) {
  dfene <- cbind(dfene,aux1[,i*12+1])
}
dfene <- cbind(dfene,aux2021[,1])

dffeb <- as.data.frame(aux1[,2])
for (i in 1:19) {
  dffeb <- cbind(dffeb,aux1[,i*12+2])
}
dffeb <- cbind(dffeb,aux20000[,1],aux2021[,2])

dfmar <- as.data.frame(aux1[,3])
for (i in 1:19) {
  dfmar <- cbind(dfmar,aux1[,i*12+3])
}
dfmar <- cbind(dfmar,aux20000[,2],aux2021[,3])

dfabr <- as.data.frame(aux1[,4])
for (i in 1:19) {
  dfabr <- cbind(dfabr,aux1[,i*12+4])
}
dfabr <- cbind(dfabr,aux20000[,3],aux2021[,4])

dfmay <- as.data.frame(aux1[,5])
for (i in 1:19) {
  dfmay <- cbind(dfmay,aux1[,i*12+5])
}
dfmay <- cbind(dfmay,aux20000[,4],aux2021[,5])

dfjun <- as.data.frame(aux1[,6])
for (i in 1:19) {
  dfjun <- cbind(dfjun,aux1[,i*12+6])
}
dfjun <- cbind(dfjun,aux20000[,5],aux2021[,6])

dfjul <- as.data.frame(aux1[,7])
for (i in 1:19) {
  dfjul <- cbind(dfjul,aux1[,i*12+7])
}
dfjul <- cbind(dfjul,aux20000[,6],aux2021[,7])

dfago <- as.data.frame(aux1[,8])
for (i in 1:19) {
  dfago <- cbind(dfago,aux1[,i*12+8])
}
dfago <- cbind(dfago,aux20000[,7],aux2021[,8])

dfsep <- as.data.frame(aux1[,9])
for (i in 1:19) {
  dfsep <- cbind(dfsep,aux1[,i*12+9])
}
dfsep <- cbind(dfsep,aux20000[,8],aux2021[,9])

dfoct <- as.data.frame(aux1[,10])
for (i in 1:19) {
  dfoct <- cbind(dfoct,aux1[,i*12+10])
}
dfoct <- cbind(dfoct,aux20000[,9],aux2021[,10])

dfnov <- as.data.frame(aux1[,11])
for (i in 1:19) {
  dfnov <- cbind(dfnov,aux1[,i*12+11])
}
dfnov <- cbind(dfnov,aux20000[,10],aux2021[,11])

dfdic <- as.data.frame(aux1[,12])
for (i in 1:19) {
  dfdic <- cbind(dfdic,aux1[,i*12+12])
}
dfdic <- cbind(dfdic,aux20000[,11])

ene <- apply(dfene, 1, mean)
feb <- apply(dffeb, 1, mean)
mar <- apply(dfmar, 1, mean)
abr <- apply(dfabr, 1, mean)
may <- apply(dfmay, 1, mean)
jun <- apply(dfjun, 1, mean)
jul <- apply(dfjul, 1, mean)
ago <- apply(dfago, 1, mean)
sep <- apply(dfsep, 1, mean)
oct <- apply(dfoct, 1, mean)
nov <- apply(dfnov, 1, mean)
dic <- apply(dfdic, 1, mean)


##########################################################################
y <- rbind(ene,feb,mar,abr,may,jun,jul,ago,sep,oct,nov,dic)
save(y,file = 'Real Data/y.Rdata')
load('Real Data/y.Rdata')
#Ubicaciones
G2=dff[,1:2]
#Matriz que contiene las ubicaciones
DG2=as.matrix(dist(G2))

n.curve <- length(dff[,1])
t <- 1:12
nbasis <- 7
ngrid <- 12

bsb = create.fourier.basis(range(t), nbasis=nbasis)
B = eval.basis(t, bsb)
P = getbasispenalty(bsb)

W <- eval.penalty(bsb, rng=range(t),Lfdobj=0)

t.grid=seq(min(t), max(t), length.out=ngrid)
delta=t.grid[2]-t.grid[1]
B.grid=eval.basis(t.grid, bsb)

arrayVp=array(NA, c(nbasis,nbasis, n.curve))
coef=matrix(NA, nbasis, n.curve)
for (i in 1:n.curve){
  fit0=gam(y[,i]~B-1, paraPen=list(B=list(P)), method="REML")
  fit1=gam((fit0$residuals)^2~B-1, paraPen=list(B=list(P)), method="REML", family=Gamma)
  swmod = gam(y[,i]~B-1, weights=(1/fit1$fitted.values), paraPen=list(B=list(P)), method="REML")
  arrayVp[,,i]=swmod$Vp
  coef[,i]=swmod$coefficients
}
Y=B.grid%*%coef
save(Y,file = 'Real Data/Y.Rdata')
load('Real Data/Y.Rdata')

#matplot(Y, type = "l")
#matlines(Y[, aux], type = "l",
#         col = 4, lwd = 2,
#         lty = 1)

L2n<-matrix(0,nrow=n.curve,ncol=n.curve)
for (i in 1:(n.curve-1)){
  coef.i<-coef[,i]
  for (j in (i+1):n.curve)
  {
    coef.j<-coef[,j]
    L2n[i,j]<-t(coef.i-coef.j)%*%W%*%(coef.i-coef.j)
    L2n[j,i]<-L2n[i,j]
  }
}

save(L2n,file = 'Real Data/L2n.Rdata')
load('Real Data/L2n.Rdata')
nbasis.w=nbasis
bsb.w = create.fourier.basis(range(t), nbasis=nbasis.w)
B.grid.w=eval.basis(t.grid, bsb.w)

save.image('Real Data/alldata.Rdata')
load('Real Data/alldata.Rdata')
n <- n.curve
##############################################################

r1 <- fLOCI(t(Y),alpha = 0.78,nn=n.curve/2,dist = 's.L2',coord = G2)
save(r1,file = 'Real Data/r1.Rdata')

r3 <- fLOCI(t(Y),alpha = 0.78,nn=n.curve/2,dist = 'L2')
save(r3,file = 'Real Data/r3.Rdata')

r2 <- fLOCI(t(Y),alpha = 0.78,nn=n.curve/2,dist = 'opt.sL2',coord = G2)
save(r2,file='Real Data/r2.Rdata')


