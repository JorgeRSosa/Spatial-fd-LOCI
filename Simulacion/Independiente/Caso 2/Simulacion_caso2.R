source('Fijo/nuevo LOCI.R')


n=200
simf1<-function(t){0.8-0.2*cos(0.5*(t-1))*t^(1.5)*sqrt(5*sqrt(t)+0.5)}
simf2<-function(t){-0.2*sin(0.5*(t-1))*t^(1.5)*sqrt(sqrt(t)+0.5)}

sigmaf<-function(t){2*exp(-(t-2.5)^2/4)}


mc <- 100
result <- list()

for (k in 1:mc) {
  
  MC <- list()
  set.seed(k)
  G2=gen_sep(n, k)
  lab=array(1, n)
  aux <- sample(1:n, n*0.1)
  lab[aux]=2
  #plot(G2,col=lab)
  DG2=as.matrix(dist(G2))
  heter=T
  m=100; n.curve=n; rho=0.5; nbasis=15
  #Cor.mat=CovS(DG2, 0.04, 0.09) ### Familia matern con factor nu = 1/2
  #svdcor=svd(Cor.mat)
  #Cor.math=svdcor$u%*%diag(sqrt(svdcor$d))%*%t(svdcor$v) ##Cor.mat^{1/2}
  ngrid=100
  t=sort(runif(m)*5)
  
  
  #E2=mvrnorm(m, 0*(1:n), 2*Cor.math)
  y= sqrt(1)*matrix(rnorm(m*n.curve), m, n.curve)*sigmaf(t)#+(E2)
  
  y[,-aux]=y[,-aux] + simf1(t)
  y[,aux]=y[, aux] + simf2(t)
  
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

save(result,file = 'Rdata/Independiente/Caso 2/result.rdata')
