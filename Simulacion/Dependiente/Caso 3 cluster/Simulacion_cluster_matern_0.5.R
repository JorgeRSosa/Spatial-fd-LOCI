source('Fijo/nuevo LOCI.R')


n=200
simf1<-function(t){0.8-0.2*cos(0.5*(t-1))*t^(1.5)*sqrt(5*sqrt(t)+0.5)}
simf2<-function(t){-2*sin(t-1)*log(t+0.5)}

sigmaf<-function(t){2*exp(-(t-2.5)^2/4)}

gen_sep2=function(n, pj){
inn1o=1
fin1o=10
speko=10
inn2o=fin1o+1+speko
fin2o=inn2o+fin1o-1

inn1v=1
fin1v=20
spekv=10
inn2v=fin1v+1+spekv
fin2v=inn2v+fin1v-1

xlim_min=inn1o
xlim_max=fin1o
xinc=1
ylim_min=inn1v
ylim_max=fin1v
yinc=1
#par(mar=c(5,5,4,1))
grid1<-expand.grid(seq(xlim_min,xlim_max,by=xinc),seq(ylim_min,ylim_max, by=yinc))

xlim_min=inn2o
xlim_max=fin2o
xinc=1
ylim_min=inn1v
ylim_max=fin1v
yinc=1
#par(mar=c(5,5,4,1))
grid2<-expand.grid(seq(xlim_min,xlim_max,by=xinc),seq(ylim_min,ylim_max, by=yinc))

xlim_min=inn1o
xlim_max=fin1o
xinc=1
ylim_min=inn2v
ylim_max=fin2v
yinc=1
#par(mar=c(5,5,4,1))
grid3<-expand.grid(seq(xlim_min,xlim_max,by=xinc),seq(ylim_min,ylim_max, by=yinc))

xlim_min=inn2o
xlim_max=fin2o
xinc=1
ylim_min=inn2v
ylim_max=fin2v
yinc=1
#par(mar=c(5,5,4,1))
grid4<-expand.grid(seq(xlim_min,xlim_max,by=xinc),seq(ylim_min,ylim_max, by=yinc))

set.seed(pj)
g1=sample(1:n, n*0.3)
grid1=grid1[g1, ]
g2=sample(1:n, n*0.3)
grid2=grid2[g2, ]
g3=sample(1:n, n*0.3)
grid3=grid3[g3, ]
g4=sample(1:n, n*0.1)
grid4=grid4[g4, ]

par(mfrow=c(1,1))
grid=data.frame("Latitudine"=c(grid1[, 1], grid2[, 1], grid3[, 1], grid4[, 1]), "Longitudine"=c(grid1[, 2], grid2[, 2], grid3[, 2], grid4[, 2]))
return(grid)
}
lab=array(1, n)
lab[trunc((n*0.9)+1):n]=2


mc <- 100
result <- list()

for (k in 1:mc) {
  
  MC <- list()
  set.seed(k)
  G2=gen_sep2(n, k)
  
  #plot(G2,col=lab)
  DG2=as.matrix(dist(G2))
  heter=T
  m=100; n.curve=n; rho=0.5; nbasis=15
  Cor.mat=CovS(DG2, 0.04, 0.09) ### Familia matern con factor nu = 1/2
  svdcor=svd(Cor.mat)
  Cor.math=svdcor$u%*%diag(sqrt(svdcor$d))%*%t(svdcor$v) ##Cor.mat^{1/2}
  ngrid=100
  t=sort(runif(m)*5)
  
  
  E2=mvrnorm(m, 0*(1:n), 2*Cor.math)
  y= sqrt(1)*matrix(rnorm(m*n.curve), m, n.curve)*sigmaf(t)+(E2)
  
  y[,1:trunc(n.curve*0.9)]=y[,1:trunc(n.curve*0.9)] + simf1(t)
  y[,trunc((n.curve*0.9)+1):n.curve]=y[, trunc((n.curve*0.9)+1):n.curve] + simf2(t)
  
  bsb = create.bspline.basis(range(t), nbasis=nbasis)
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
  #matplot(Y, type = "l",col = lab)
  #matlines(Y[, aux], type = "l",
  #         col = 4, lwd = 2,
  #         lty = 1)
  
  L2n<-matrix(0,nrow=n,ncol=n)
  for (i in 1:(n-1))
  {
    coef.i<-coef[,i]
    for (j in (i+1):n)
    {
      coef.j<-coef[,j]
      L2n[i,j]<-t(coef.i-coef.j)%*%W%*%(coef.i-coef.j)
      L2n[j,i]<-L2n[i,j]
    }
  }
  
  nbasis.w=nbasis
  bsb.w = create.bspline.basis(range(t), nbasis=nbasis.w)
  B.grid.w=eval.basis(t.grid, bsb.w)
  
  r1 <- fLOCI(t(Y),alpha = 0.78,nn=n/2,dist = 's.L2',coord = G2)
  r3 <- fLOCI(t(Y),alpha = 0.78,nn=n/2,dist = 'L2')
  r2 <- fLOCI(t(Y),alpha = 0.78,nn=n/2,dist = 'opt.sL2',coord = G2)
  MC[[1]] <- Y
  MC[[2]] <- lab
  MC[[3]] <- r1
  MC[[4]] <- r2
  MC[[5]] <- r3
  
  result[[k]] <- MC
}

save(result,file = 'Rdata/Dependiente/Caso 3 cluster/result_cluster_matern_0.5.rdata')
