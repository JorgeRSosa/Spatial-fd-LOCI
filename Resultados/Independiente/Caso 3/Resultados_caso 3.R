library(ggplot2)
library(caret)
library(dplyr)

load('Rdata/Independiente/Caso 3/result.rdata')



n <- 200
outlier <- lapply(result, function(x) {
  aux <- array(0, n)
  aux[which(x[[2]]==2)] <- 1
  return(aux)
})

k <- seq(1.5,5.5,by=0.5)
#k <- seq(5,7,by=0.25)
# Cada elemento es una lista de 100 elementos con los resultados de clasificacón para el respectivo k
df_kl2 <- list() 
for(i in 1:length(k)){
  df_kl2[[i]] <- lapply(result, function(x){
    aux <- (x[[3]]$MDEF > (k[i] * x[[3]]$norm_MDEF))
    aux2 <- rep(0,length(x[[3]]$MDEF))
    aux2[aux] <- 1
    return(aux2)})
}

df_koptl2 <- list()
for(i in 1:length(k)){
  df_koptl2[[i]] <- lapply(result, function(x){
    aux <- (x[[4]]$MDEF > (k[i] * x[[4]]$norm_MDEF))
    aux2 <- rep(0,length(x[[4]]$MDEF))
    aux2[aux] <- 1
    return(aux2)})
}

df_krl2 <- list() 
for(i in 1:length(k)){
  df_krl2[[i]] <- lapply(result, function(x){
    aux <- (x[[5]]$MDEF > (k[i] * x[[5]]$norm_MDEF))
    aux2 <- rep(0,length(x[[5]]$MDEF))
    aux2[aux] <- 1
    return(aux2)})
}


#############
# cada elemento tiene una lista donde cada elemento es la matriz de confusión para LOCI dist l2 
df_kl2_mc <- lapply(df_kl2,function(y) {
  aux <- list()
  for (i in 1:100) {
    aux[[i]] <- confusionMatrix(as.factor(y[[i]]),as.factor(outlier[[i]]),positive='1')
  }
  return(aux)
})

l2.stat <- lapply(df_kl2_mc, function(x) {
  l2.f1f <- c(x[[1]]$byClass['Sensitivity'],x[[1]]$byClass['Detection Rate'],x[[1]]$byClass['Specificity'],x[[1]]$byClass['Precision'])
  for(i in 2:100) l2.f1f <- rbind(l2.f1f,c(x[[i]]$byClass['Sensitivity'],x[[i]]$byClass['Detection Rate'],x[[i]]$byClass['Specificity'],x[[i]]$byClass['Precision']))
  l2.f1f <- as.data.frame(l2.f1f)
  l2.f1f['Method'] <- 'LOCI_sL2'
  return(l2.f1f)
})

for (i in 1:length(k)) {
  l2.stat[[i]]$K <- paste0('k=',round(k[i],4))
}

########################
df_koptl2_mc <- lapply(df_koptl2,function(y) {
  aux <- list()
  for (i in 1:100) {
    aux[[i]] <- confusionMatrix(as.factor(y[[i]]),as.factor(outlier[[i]]),positive='1')
  }
  return(aux)
})

optl2.stat <- lapply(df_kl2_mc, function(x) {
  l2.f1f <- c(x[[1]]$byClass['Sensitivity'],x[[1]]$byClass['Detection Rate'],x[[1]]$byClass['Specificity'],x[[1]]$byClass['Precision'])
  for(i in 2:100) l2.f1f <- rbind(l2.f1f,c(x[[i]]$byClass['Sensitivity'],x[[i]]$byClass['Detection Rate'],x[[i]]$byClass['Specificity'],x[[i]]$byClass['Precision']))
  l2.f1f <- as.data.frame(l2.f1f)
  l2.f1f['Method'] <- 'LOCI_opt.sL2'
  return(l2.f1f)
})

for (i in 1:length(k)) {
  optl2.stat[[i]]$K <- paste0('k=',round(k[i],4))
}

#####################

df_krl2_mc <- lapply(df_krl2,function(y) {
  aux <- list()
  for (i in 1:100) {
    aux[[i]] <- confusionMatrix(as.factor(y[[i]]),as.factor(outlier[[i]]),positive='1')
  }
  return(aux)
})

rl2.stat <- lapply(df_kl2_mc, function(x) {
  l2.f1f <- c(x[[1]]$byClass['Sensitivity'],x[[1]]$byClass['Detection Rate'],x[[1]]$byClass['Specificity'],x[[1]]$byClass['Precision'])
  for(i in 2:100) l2.f1f <- rbind(l2.f1f,c(x[[i]]$byClass['Sensitivity'],x[[i]]$byClass['Detection Rate'],x[[i]]$byClass['Specificity'],x[[i]]$byClass['Precision']))
  l2.f1f <- as.data.frame(l2.f1f)
  l2.f1f['Method'] <- 'LOCI_L2'
  return(l2.f1f)
})

for (i in 1:length(k)) {
  rl2.stat[[i]]$K <- paste0('k=',round(k[i],4))
}

######################
## armo un solo dataframe
auxl2 <- c()
for (i in 1:length(l2.stat)) {
  auxl2 <- rbind(auxl2,l2.stat[[i]])
}

auxoptl2 <- c()
for (i in 1:length(optl2.stat)) {
  auxoptl2 <- rbind(auxoptl2,optl2.stat[[i]])
}

auxrl2 <- c()
for (i in 1:length(rl2.stat)) {
  auxrl2 <- rbind(auxrl2,rl2.stat[[i]])
}



df <- rbind(auxl2,auxoptl2,auxrl2)
df <- df %>% rename(Detection_Rate=colnames(df)[2])

ggplot(df, aes(x = Method, y = Sensitivity)) +
  geom_boxplot(show.legend = FALSE,na.rm = T) +
  facet_wrap(~K)#+
#labs(title='Indepent Curve - Shape Variation')

ggplot(df, aes(x = Method, y = Detection_Rate)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~K)#+
#labs(title='Indepent Curve - Shape Variation')

ggplot(df, aes(x = Method, y = Specificity)) +
  geom_boxplot(show.legend = FALSE,na.rm = T) +
  facet_wrap(~K)# +
#labs(title='Indepent Curve - Shape Variation')

ggplot(df, aes(x = Method, y = Precision)) +
  geom_boxplot(show.legend = FALSE,na.rm = T) +
  facet_wrap(~K)# +
#labs(title='Indepent Curve - Shape Variation')