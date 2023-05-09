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

### Nota:
### si el paquete vows no lo tiene instalado, use el archivo vows.tar.gz de la carpeta 
### comprimida para instalarlo. Para ello dirijase a 
### Packages/Install/Install from: Packages Archive Files/seleccionar vows.tar.gz/instalar

################################################################################
########## Simulación considerando curvas sin dependencia espacial #############
################################################################################

#### Caso 1
source('Simulacion/Independiente/Caso 1/Simulacion_caso1.R')

#### Caso 2
source('Simulacion/Independiente/Caso 2/Simulacion_caso2.R')

#### Caso 3
source('Simulacion/Independiente/Caso 3/Simulacion_caso3.R')

################################################################################
########## Simulación considerando curvas con dependencia espacial #############
################################################################################

####### Considerando outlier distribuídos aleatoriamente en la grilla

##### Función matern con factor nu=1/2
#### Caso 1
source('Simulacion/Dependiente/Caso 1/Simulacion_matern_0.5.R')

#### Caso 2
source('Simulacion/Dependiente/Caso 2/Simulacion_matern_0.5.R')

#### Caso 3 
source('Simulacion/Dependiente/Caso 3/Simulacion_matern_0.5.R')

##### Función matern con factor nu=3/2
#### Caso 1
source('Simulacion/Dependiente/Caso 1/Simulacion_matern_1.5.R')

#### Caso 2
source('Simulacion/Dependiente/Caso 2/Simulacion_matern_1.5.R')

#### Caso 3 
source('Simulacion/Dependiente/Caso 3/Simulacion_matern_1.5.R')

##### Función matern con factor nu=inf
#### Caso 1
source('Simulacion/Dependiente/Caso 1/Simulacion_matern_INF.R')

#### Caso 2
source('Simulacion/Dependiente/Caso 2/Simulacion_matern_INF.R')

#### Caso 3 
source('Simulacion/Dependiente/Caso 3/Simulacion_matern_INF.R')

################################################################################

####### Considerando cluster de outlier en la grilla

##### Función matern con factor nu=1/2
#### Caso 1
source('Simulacion/Dependiente/Caso 1 cluster/Simulacion_cluster_matern_0.5.R')

#### Caso 2
source('Simulacion/Dependiente/Caso 2 cluster/Simulacion_cluster_matern_0.5.R')

#### Caso 3 
source('Simulacion/Dependiente/Caso 3 cluster/Simulacion_cluster_matern_0.5.R')

##### Función matern con factor nu=3/2
#### Caso 1
source('Simulacion/Dependiente/Caso 1 cluster/Simulacion_cluster_matern_1.5.R')

#### Caso 2
source('Simulacion/Dependiente/Caso 2 cluster/Simulacion_cluster_matern_1.5.R')

#### Caso 3 
source('Simulacion/Dependiente/Caso 3 cluster/Simulacion_cluster_matern_1.5.R')

##### Función matern con factor nu=inf
#### Caso 1
source('Simulacion/Dependiente/Caso 1 cluster/Simulacion_cluster_matern_INF.R')

#### Caso 2
source('Simulacion/Dependiente/Caso 2 cluster/Simulacion_cluster_matern_INF.R')

#### Caso 3 
source('Simulacion/Dependiente/Caso 3 cluster/Simulacion_cluster_matern_INF.R')

