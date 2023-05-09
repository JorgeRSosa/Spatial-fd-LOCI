###### Para figura 1
Conf2mas1 = matrix(c(1,3,2,3), nrow=2, byrow=F) # Creamos un matriz a partir de un vector con los valores c(1:3,3) que es igual que c(1,2,3,3)

Conf2mas1

layout(Conf2mas1)
layout.show(3)

matplot(Y1, type = "l",col = lab,lty = 1,xlab = "t",ylab = "X(t)",main="(a)")#caso 1 matern 1/2 semilla 1
matplot(Y2, type = "l",col = lab,lty = 1,xlab = "t",ylab = "X(t)",main="(b)")#caso 2 matern 3/2 semilla 1
matplot(Y3, type = "l",col = lab,lty = 1,xlab = "t",ylab = "X(t)",main="(c)")#caso 3 matern inf semilla 1


##### Para figura2
#semilla 1
par( mfrow= c(1,2) )
#outliers aleatorios 
plot(G2,col=lab, pch = 20)
points(G2[aux,], pch = 20, col = "red")
#cluster de outlier
plot(G22,col=lab2, pch = 20)
points(G22[trunc((n*0.9)+1):n,], pch = 20, col = "red")

library(reshape2)
library(tidyverse)
library(ggthemes)
library(patchwork) # para combinar gráficas ggplot

################ Outlier aleatorios ############################################
#---------------- Para matern de 1/2 ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1_0.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p1 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +
  scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2_0.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p2 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3_0.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p3 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()


p1 + p2 + p3 +
  plot_layout(ncol = 1)

#---------------- Para matern de 3/2 ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1_1.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p1 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2_1.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p2 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3_1.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p3 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()


p1 + p2 + p3 +
  plot_layout(ncol = 1)
#---------------- Para matern inf ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1_INF.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p1 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2_INF.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p2 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3_INF.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p3 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()


p1 + p2 + p3 +
  plot_layout(ncol = 1)

################ cluster de Outlier  ###########################################
#---------------- Para matern de 1/2 ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1cluster_0.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p1 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2cluster_0.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p2 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3cluster_0.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p3 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()


p1 + p2 + p3 +
  plot_layout(ncol = 1)

#---------------- Para matern de 3/2 ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1cluster_1.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p1 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2cluster_1.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p2 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3cluster_1.5.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p3 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()


p1 + p2 + p3 +
  plot_layout(ncol = 1)
#---------------- Para matern inf ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1cluster_INF.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p1 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2cluster_INF.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p2 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3cluster_INF.Rdata")

dfc1 <- df %>% group_by(b,Method) %>% summarise(mean_Detection_rate=mean(Detection_Rate,na.rm=T),mean_Precision=mean(Precision,na.rm=T))
dfc1 <- melt(dfc1,id=c('b','Method'))
p3 <- ggplot(dfc1, aes(x = b, y = value,col=Method)) +
  geom_point(na.rm = T) +
  geom_line()+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()


p1 + p2 + p3 +
  plot_layout(ncol = 1)

###----------------- Boxplot ----------------------------

#---------------- Para matern de 1/2 ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1_0.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p1 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2_0.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p2 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3_0.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p3 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()

#---------------- Para matern de 3/2 ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1_1.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p1 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2_1.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p2 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3_1.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p3 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()

#---------------- Para matern de INF ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1_INF.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p1 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2_INF.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p2 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3_INF.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p3 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()


############## Cluster -------------------

#---------------- Para matern de 1/2 ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1cluster_0.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p1 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2cluster_0.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p2 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3cluster_0.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p3 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()

#---------------- Para matern de 3/2 ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1cluster_1.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p1 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2cluster_1.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p2 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3cluster_1.5.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p3 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()

#---------------- Para matern de INF ---------------------------

load("Resultados/Dependiente/Rdata/df_caso1cluster_INF.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p1 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 1") + theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso2cluster_INF.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p2 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 2")+ theme_few() +   scale_fill_few()

load("Resultados/Dependiente/Rdata/df_caso3cluster_INF.Rdata")

dfc1 <- df %>% filter(b==1.75) %>% dplyr::select(-b)
dfc1 <- melt(dfc1,id=c('Method'))
p3 <- ggplot(dfc1, aes(x = Method, y = value)) +
  geom_boxplot(show.legend = FALSE,na.rm = T)+
  facet_wrap(~variable,scales = "free")+
  labs(y = " ",title = "Case 3")+ theme_few() +   scale_fill_few()




