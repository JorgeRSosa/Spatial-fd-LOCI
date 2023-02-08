###
###   calcolo l'integrale
###
trap=function(f, I)
{
  n=length(f)
  Sum=sum(((f[1:(n-1)]+f[2:n])*(I[2:n]-I[1:(n-1)])/2))
  return(Sum)
}

###
###   creo la griglia
###


##compute the numerator and denomator matrices B and A respectively
s.computAB<-function(coef=coef, arrayVp=arrayVp, B.grid=B.grid, B.grid.weight=B.grid.weight, weight=rep(1, ngrid), delta=delta, s=s.dip){
  
  nbasis=ncol(B.grid.weight)
  n.curve=ncol(coef)
  A=B1=B2=matrix(0, nbasis, nbasis)
  
  for(i in 1:(n.curve-1)){
    for(j in (i+1):n.curve){
      V=arrayVp[,,i]+arrayVp[,,j]
      varf=diag(B.grid%*%V%*%t(B.grid))
      #### here, the use of spatially dipendens
      ebeta=s[i,j]*coef[,i]-s[j,i]*coef[,j]
      fhat=as.vector(B.grid%*%ebeta)
      
      ### calculate the squared mean
      temp=B.grid.weight*((fhat^2+varf)*sqrt(weight)*delta)
      A1=apply(temp, 2, sum)
      A=A+(A1%*%t(A1))
      
      ### calculate the variance
      M=t(B.grid)%*%(B.grid*(weight*delta))
      temp2=diag(B.grid%*%V%*%M%*%V%*%t(B.grid))
      B1=B1+(t(B.grid.weight)%*%(B.grid.weight*temp2*delta))
      
      tempv=t(B.grid.weight)%*%(B.grid*(fhat*sqrt(weight)*delta))
      B2=B2+(tempv%*%V%*%t(tempv))
    }
  }
  
  
  Bv=2*B1+4*B2
  
  return(list(A=A, Bv=Bv))
}


########################################################################################
#### coef: an nbasisxn spline coefficient matrix 
#### arrayVp: nbasisXnbasisXn array,  Bayesian variance of the spline coefficient
#### B.grid: ngridXnbasis spline design matrix for the smoothed curves
#### B.grid.weight: spline design matrix for the weight function
#### t.grid: the equally spaced grid points 
########################################################################################

s.weight.minCV=function(coef=coef, arrayVp=arrayVp, B.grid=B.grid, B.grid.weight=B.grid.weight, t.grid=t.grid, niter=100, tol=1e-4, s.dip=s.dip){
  
  ngrid=nrow(B.grid.weight) ###number of grid points
  delta=(max(t.grid)-min(t.grid))/ngrid
  fit=s.computAB(coef=coef, arrayVp=arrayVp, B.grid=B.grid, B.grid.weight=B.grid.weight, weight=rep(1, ngrid),  delta=delta, s=s.dip)
  svdB=svd(fit$Bv)
  Bh=svdB$u%*%diag(svdB$d^(-1/2))%*%t(svdB$v) ##B^{-1/2}
  svdBA=svd(Bh%*%fit$A%*%Bh)
  qnew=Bh%*%svdBA$u[,1]
  CVar2=t(qnew)%*%fit$Bv%*%qnew/(t(qnew)%*%fit$A%*%qnew)
  weight=as.vector((B.grid.weight%*%qnew)^2)
  weight=weight/sum(weight*delta)
  
  #    par(mfrow=c(3,3))
  diff=iter=1
  while(diff>tol & iter<niter){
    cat("iteration=", iter,
        "difference=", diff,
        "CVar^2=", CVar2, "\n")
    
    qold=qnew
    fit=s.computAB(coef=coef, arrayVp=arrayVp, B.grid=B.grid, B.grid.weight=B.grid.weight, weight=weight, delta=delta, s=s.dip)
    svdB=svd(fit$Bv)
    Bh=svdB$u%*%diag(svdB$d^(-1/2))%*%t(svdB$v)
    svdBA=svd(Bh%*%fit$A%*%Bh)
    qnew=Bh%*%svdBA$u[,1]; 
    qnew=(qnew+qold)/2
    weight=as.vector((B.grid.weight%*%qnew)^2);  
    weight=weight/sum(weight*delta)
    diff=mean(abs(B.grid.weight%*%(qold-qnew)))
    iter=iter+1
    CVar2=t(qnew)%*%fit$Bv%*%qnew/(t(qnew)%*%fit$A%*%qnew)
    #        plot(t.grid, weight, pch=20, col='red', ylab='Weight', xlab='t')
  }        
  cat("iteration=", iter,
      "difference=", diff,
      "CVar^2=", CVar2, "\n")
  
  return(list(weight=weight, q=qnew, B.grid.weight=B.grid.weight, CVar2=CVar2, iter=iter))
}

Gh=function(g, coord, Eu.d)
{
  
  emp.trace.vari <- trace.variog(coords=coord,
                                 L2norm=g, bin=F,
                                 uvec="default",breaks="default")
  nugget.fix=NULL
  max.dist.variogram=NULL
  sigma2.0<-quantile(emp.trace.vari$v,0.75, na.rm=T)
  phi.0<-quantile(emp.trace.vari$u,0.75, na.rm=T)
  if(is.null(nugget.fix)){
    fix.nugget<-FALSE
    nugget<-0
  }else{
    fix.nugget=TRUE
    nugget<-nugget.fix
  }
  if (is.null(max.dist.variogram))
    max.dist.variogram<-max(emp.trace.vari$u, na.rm=T)   
  models=fit.tracevariog(emp.trace.vari, models=c("exponential","spherical", "matern", "circular", "cubic", "powered.exponential", "pure.nugget","wave","gaussian", "gneiting", "cubic", "cauchy"), ##cubic is ok ##circular is better
                         sigma2.0,phi.0,
                         max.dist=max.dist.variogram,fix.nugget=fix.nugget,
                         nugget=nugget)

  sigm2 <- models$best$cov.pars[1]
  Sig <-sigm2 - cov.spatial(Eu.d,cov.model=models$best$cov.model,
                            cov.pars=models$best$cov.pars,
                            kappa=models$best$kappa)
  return(Sig)
}

gen_sep=function(n, pj)
{
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
  g1=sample(1:n, n/4)
  grid1=grid1[g1, ]
  g2=sample(1:n, n/4)
  grid2=grid2[g2, ]
  g3=sample(1:n, n/4)
  grid3=grid3[g3, ]
  g4=sample(1:n, n/4)
  grid4=grid4[g4, ]
  
  par(mfrow=c(1,1))
  grid=data.frame("Latitudine"=c(grid1[, 1], grid2[, 1], grid3[, 1], grid4[, 1]), "Longitudine"=c(grid1[, 2], grid2[, 2], grid3[, 2], grid4[, 2]))
  
  #  plot(grid, xlab="x coordinate", ylab="y coordinate", col=1, pch=16, cex.axis=1.3,cex.lab=1.5)
  return(grid)
}
Chen= function(variables) {
  s1=matrix(1, n,n)
  fit=s.weight.minCV(coef=coef, arrayVp=arrayVp, B.grid=B.grid, B.grid.weight=B.grid.w, t.grid=t.grid, s.dip=s1)
  w=fit$weight
  plot(t.grid, w, type='l', main="Chen")
  ###
  ###calcolo della matrice di gram associata alla norma peso
  ###
  Wq=matrix(0, nbasis, nbasis)
  for(i in 1:nbasis)
  {
    for(j in 1:nbasis)
    {
      Wq[i, j]=Wq[j, i]=trap(w*(B.grid[,i]*B.grid[,j]), t.grid)
    }
  }
  ###
  ###Calcolo della distanza tramite la matrice di gram associata alla norma peso
  ###
  L2d=matrix(0, n, n)
  for(i in 1:(n-1))
  {
    for(j in (i+1):n)
    {
      L2d[i,j]=L2d[j,i]=t(coef[,i]-coef[,j])%*%Wq%*%(coef[,i]-coef[,j])
    }
  }
  return(L2d)  
}


us=function(){
  
  s5=Gh(L2n, G2, DG2)
  fit=s.weight.minCV(coef=coef, arrayVp=arrayVp, B.grid=B.grid, B.grid.weight=B.grid.w, t.grid=t.grid, s.dip=s5)
  w=fit$weight
  #plot(t.grid, w, type='l', main="Us")
  Wq=matrix(0, nbasis, nbasis)
  for(i in 1:nbasis)
  {
    for(j in 1:nbasis)
    {
      Wq[i, j]=Wq[j, i]=trap(w*(B.grid[,i]*B.grid[,j]), t.grid)
    }
  }
  ###
  ###   calcolo delle distanze con la funzione peso
  ###
  s.L2d5=matrix(0, n, n)
  for(i in 1:(n-1))
  {
    for(j in (i+1):n)
    {
      s.L2d5[i,j]=s.L2d5[j,i]=t(coef[,i]-coef[,j])%*%Wq%*%(coef[,i]-coef[,j])
    }
  }
  return(s.L2d5)
}
mod_lab=function(lista1, lista2){
  
  lista=lista2
  c=max(lista)
  fatto=matrix(1, nrow=c, 1)
  ma=matrix(0, nrow=c, ncol=1)
  tab=table(lista1, lista2)
  for(j in 1:c)
  {
    ma[j]=max(tab[fatto==1,j])
    for(i in 1:c)
    {
      if(tab[i,j]==ma[j])
      {  
        lista2[lista==j]=i
        fatto[i]=0
      }
    }
  }
  return(lista2)
}

CovS=function(h, ni, c)
{
  covs=(1-ni)*exp(-c*abs(h))
  return(covs)
}
# h es la distancias