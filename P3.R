library(TTR)
library(quantmod)
library(ggplot2)
library(ggpubr)
library(hrbrthemes)
library(qmao)
library(xtable)
library(tidyverse)
library(knitr)
library(broom)
library(plyr)
library(DescTools)
library(devtools)
#library(elasticsearch)  #da problem para usar PF de qmao 
library(NLP)
#primero grafica si no da error


#P&l NEGATIVO ES POSITIVO PORQUE SE EVALUA CON PRECIO FINAL- NUEVO

#cargamos datos de las emisoras

ticks<-c("WALMEX.MX","GFNORTEO.MX","PE&OLES.MX","AMXL.MX","GAPB.MX","GMEXICOB.MX","TLEVISACPO.MX","GCARSOA1.MX","KIMBERA.MX","LABB.MX")

date_end<-as.Date(c("2022-09-30"))
date_start<-as.Date((c("2019-01-01")))

getSymbols(ticks ,src="yahoo",from=date_start ,to=date_end, periodicity="daily")

#Precios de cierre como BD

data<-PF(ticks,silent = T,prefer = "Close") #cierre ajusatdo
data<-as.data.frame(data)

rendimiento<-matrix(0,nrow = nrow(data),ncol = length(ticks))

for (j in 1:length(ticks)) {
  for(i in 1:nrow(data)){
    
    if(i==1){
      rendimiento[i,j]<-NA
    }else{
      
      rendimiento[i,j]<- (data[i,j]/data[i-1,j]) -1
      
    }
    
    
  }
}

colnames(rendimiento)<-c(paste("var",ticks) )


data<-cbind(data, as.data.frame( rendimiento) ) 

#colnames(data)<-cbind(ticks, paste("var",ticks) )

last_prices<-data[nrow(data),c(1:length(ticks))]
last_prices

alpha<-c(.05,.025,.01) #niveles de confianza
########################################################################################################################################################
#                                                                         SIM HISTORICA
########################################################################################################################################################

#SIMULACION HISTORICA individual

#creamos vector de reevaluacion y P&L para ultimo precio

reev<-matrix(0,nrow = nrow(data),ncol = length(ticks))
PL<-matrix(0,nrow = nrow(data),ncol = length(ticks))

reev[1,]<-NA
PL[1,]<-NA

for(j in 1:length(ticks)){
  for(i in 2:nrow(data)){
    
    reev[i,j]<-as.numeric(last_prices[j]*(1+ data[i,length(ticks)+j]))
    PL[i,j]<-as.numeric( last_prices[j]-reev[i,j] )
    
  }
  
}

dataSH<-cbind(data,reev,PL)
colnames(dataSH)<-cbind(ticks, paste("var",ticks), paste("Reev",ticks), paste("P&L",ticks)  )


#calculamos stats de PL

minimo<-apply(dataSH[,c( (3*length(ticks) +1) : (4*length(ticks)) )][-1,] ,2,min )
maximo<-apply(dataSH[,c( (3*length(ticks) +1) : (4*length(ticks)) )][-1,],2,max )
rango<-maximo-minimo

kable(rbind(minimo,maximo,rango),digits = 4,caption = "P&L de la emisora")

medias<-apply(dataSH[,c( (3*length(ticks) +1) : (4*length(ticks)) )][-1,] ,2,mean )
varianzas<-apply(dataSH[,c( (3*length(ticks) +1) : (4*length(ticks)) )][-1,] ,2,var )
desvs<-sqrt(varianzas)


#ojo a la formula apra usar quantile()
VaR_SH<-apply(dataSH[,c( (3*length(ticks) +1) : (4*length(ticks)) )][-1,], 2,quantile,probs=1-alpha,na.rm=T)
tVaR_sh<-kable(VaR_SH,caption = "VaR Simulacion Historica", align = "r",digits = 4 ) #Tabla final
tVaR_sh

#Sim Historica Portafolio

#agregamos PL general (suma de los P&L)

portfolioPL_sh<-apply(dataSH[,c( (3*length(ticks)+1 ): (4*length(ticks))) ], 1 , sum )

dataSH<-cbind(dataSH,portfolioPL_sh)

#VaR del portafolio

P_VaR_sh<-quantile(dataSH$portfolioPL_sh,probs = 1-alpha,na.rm = T)
P_VaR_sh

VaRP_sh<-kable(P_VaR_sh, col.names = "VaR",caption = "VaR del Portafolio por Simulación Histórica")

VaRP_sh

######################################################################################################################################################
#                                                             Simulacion Monte Carlo
######################################################################################################################################################

#sim MC individual

#hallamos valores est de los rendimientos para simular nuevos

media<-apply(data[,c((length(ticks) +1): (2*length(ticks)) )][-1,],2,mean )
varianzas<-apply(data[,c((length(ticks) +1): (2*length(ticks)) )][-1,],2,var )
desviacion<-sqrt(varianzas)

kable(rbind(media,varianzas,desviacion),digits = 4,caption = "Estadísticos de los rendimientos por emisora")

#matriz de rendimientos como VA normales diferentes:

rends<-matrix(0,nrow = nrow(data),ncol = length(ticks))

for(j in 1:length(ticks)){
    
    rends[,j]<- rnorm( n=nrow(data), mean = media[j],sd=desviacion[j]  )

}

rends[1,]<-NA  #matriz de rendimientos simulados

dataSM<-cbind(data,rends)
colnames(dataSM)<-cbind(ticks, paste("var",ticks), paste("var_sim",ticks) )

#reevaluamos y sacamos P&L

reev_sm<-matrix(0,nrow = nrow(data),ncol = length(ticks))
PL_sm<-matrix(0,nrow = nrow(data),ncol = length(ticks))

reev_sm[1,]<-NA
PL_sm[1,]<-NA

for(j in 1:length(ticks)){
  for(i in 2:nrow(dataSM)){
    
    reev_sm[i,j]<-as.numeric(last_prices[j]*(1+ dataSM[i,2*length(ticks)+j]))
    PL_sm[i,j]<-as.numeric( last_prices[j]-reev_sm[i,j] )
    
  }
}


dataSM<-cbind(dataSM,reev_sm,PL_sm)
colnames(dataSM)<-cbind(ticks, paste("var",ticks), paste("var_sim",ticks) , paste("Reev_sm",ticks), paste("P&L_sm",ticks)  )


#calculamos stats de PL

minimo_sm<-apply(dataSM[,c( (4*length(ticks) +1) : (5*length(ticks) ) ) ][-1,]  ,2,min )
maximo_sm<-apply(dataSM[,c( (4*length(ticks) +1) : (5*length(ticks) ) ) ][-1,]  ,2,max )
rango_sm<-maximo_sm-minimo_sm

kable(rbind(minimo_sm,maximo_sm,rango_sm),digits = 4,caption = "P&L de la emisora")

medias<-apply(dataSM[,c( (4*length(ticks) +1) : (5*length(ticks) ) ) ][-1,]  ,2,mean )
varianzas<-apply(dataSM[,c( (4*length(ticks) +1) : (5*length(ticks) ) ) ][-1,]  ,2,var )
desvs<-sqrt(varianzas)

alpha<-c(.05,.025,.01)

#ojo a la formula apra usar quantile()
VaR_SM<-apply(dataSM[,c( (4*length(ticks) +1) : (5*length(ticks) ) ) ][-1,]  , 2,quantile,probs=1-alpha,na.rm=T)
tVaR_sm<-kable(VaR_SM,caption = "VaR Simulacion Monte-Carlo", align = "r",digits = 4 ) #Tabla final

tVaR_sm

#SM del portafolio

#agregamos PL general (suma de los P&L)

portfolioPL_sm<-apply(dataSM[,c( (4*length(ticks)+1 ): (5*length(ticks))) ], 1 , sum )

dataSM<-cbind(dataSM,portfolioPL_sm)

#VaR SM portafolio

P_VaR_sm<-quantile(dataSM$portfolioPL_sm,probs = 1-alpha,na.rm = T)
P_VaR_sm

VaRP_sm<-kable(P_VaR_sm, col.names = "VaR",caption = "VaR del Portafolio por Simulación Monte-Carlo")

VaRP_sm


########################################################################################################################################################
#                                                                 Simulacion Bootstrap
########################################################################################################################################################


num<-seq(1:nrow(data))

intervalo<-vector()

for (i in 1:nrow(data)) {
  
  intervalo[i]<- (i-1)/nrow(data)
  
}

aleatorios<-runif(nrow(data))

dataBoot<-cbind(dataSH,num,intervalo,aleatorios)

#hacemos tipo Vlookup para sacar la pérdida simulada (P&L)

ind<-vector()
PL_sim<-matrix(0,nrow = nrow(data),ncol = length(ticks))

for(j in  1:length(ticks)){
  for (i in 1:nrow(dataBoot)) {

  ind[i]<-Closest(dataBoot$intervalo, dataBoot$aleatorios[i] ,which = 1) #dice que fila de dataBoot$intervalo es mas cercana al numero aleatorio
  
  PL_sim[i,j]<-dataBoot[ ind[i], (3*length(ticks) + j)  ]               #asignamos el valor del P&L de la fila a la emisora dada por j 
 
   }
}

colnames(PL_sim)<-c(paste("P&L sim",ticks) )
dataBoot<-cbind(dataBoot,PL_sim)

#tenemos que simular un madral de veces la pinche matriz para sacar media del var de cada una 

SimBoot<-function(n){
  
VaR_matrix1<-matrix(0,nrow = n,ncol = length(ticks))  #95%
VaR_matrix2<-matrix(0,nrow = n,ncol = length(ticks))  #97.5%
VaR_matrix3<-matrix(0,nrow = n,ncol = length(ticks))  #99%
  

for(k in 1:n){   
  rand<-runif(nrow(dataBoot))
  
  ind<-vector()
  PL_sim<-matrix(0,nrow = nrow(data),ncol = length(ticks))
  
  for(j in  1:length(ticks)){
    for (i in 1:nrow(dataBoot)) {
      
      ind[i]<-Closest(dataBoot$intervalo, rand[i] ,which = 1)[1] #dice que fila de dataBoot$intervalo es mas cercana al numero aleatorio
      
      PL_sim[i,j]<-dataBoot[ ind[i], (3*length(ticks) + j)  ]               #asignamos el valor del P&L de la fila a la emisora dada por j 
      
    }#i
  }#j
  
  VaR_matrix1[k,]<- as.numeric(apply(PL_sim,2,quantile,probs=1-alpha[1],na.rm=T))
  VaR_matrix2[k,]<- as.numeric(apply(PL_sim,2,quantile,probs=1-alpha[2],na.rm=T))
  VaR_matrix3[k,]<- as.numeric(apply(PL_sim,2,quantile,probs=1-alpha[3],na.rm=T))
  
  
}#k
  
#ya con las matrices de VaR sacamos el promedio de VaR y amonos

VaR_95<-apply( VaR_matrix1,2,mean,na.rm=T)
VaR_975<-apply( VaR_matrix2,2,mean,na.rm=T)
VaR_99<-apply( VaR_matrix3,2,mean,na.rm=T)

Matriz_VaR<-rbind( VaR_95,VaR_975,VaR_99  )
Matriz_VaR

}#function, el otput es una matriz con los VaR promedio a cada nivel en renglon y emisora en columna

#simulamos el calculo de muchos VaRs para al final sacar la media

nsims<-500
VaR_matrix<-SimBoot(nsims) #si jala
colnames(VaR_matrix)<-c(paste("VaR",ticks) )
rownames(VaR_matrix)<-c("95%","97.5","99%")
tVaR_boot<-kable(VaR_matrix,digits=4,caption = c( paste("Promedios de VaR con",nsims, "simulaciones Bootstrap" )))

tVaR_boot

#Bootstrap para el portafolio

#agregamos PL general (suma de los P&L)

portfolioPL_boot<-apply(dataBoot[,c( (4*length(ticks)+4+1 ): (5*length(ticks) + 4 )) ], 1 , sum )

dataBoot<-cbind(dataBoot,portfolioPL_boot)

#VaR del portafolio

P_VaR_boot<-quantile(dataBoot$portfolioPL_boot,probs = 1-alpha,na.rm = T)
P_VaR_boot

VaRP_boot<-kable(P_VaR_boot, col.names = "VaR",caption = c( paste("VaR del Portafolio con",nsims, "simulaciones Bootstrap" )) )

VaRP_boot




#######################################################################################################################################################
#                                                                   VaR alisado exponencial
#######################################################################################################################################################

#creamos el vector de numeros para asignar los pesos al alisado exp y dar mas peso a las obs recientes:

nums<-seq(from=nrow(data),to=1,by=-1)

#parametros del alisado

beta<-.05
a<-1-beta

#función del alisado

Aexp<-function(x){
  
  y<-beta*(a^(x-1)) 
  
  return(y)
  
}

A_exp<-Aexp(nums)
A_exp[1]<-NA

dataAE<-cbind(dataSH[,-ncol(dataSH)],nums,A_exp)

#Ej para primer emisora NO CORRER

df<-dataAE[,c(24:26)]
fd<-arrange(df,desc(`P&L GAPB.MX`)) #ordena el dataframe de acuerdo al orden descendente de la Va de P&L


#dataframes ordenados para todas las emisoras al pinche chile

for(i in 1:length(ticks)){
  
  aux_df<-cbind(  dataAE[,( 3*length(ticks) +i )]   , dataAE[,(4*length(ticks) +1 )], dataAE[,(4*length(ticks) +2 )]   ) #jala los valores P&L de cada i y los numero y el alisado
  aux_df<-as.data.frame(aux_df)
  #ordena cada uno de los df
  aux_df<-arrange(aux_df, desc(aux_df[,1]) )
  #creamos el vector de probas acumuladas
  
  aux<-vector()
  y<-0
  lista<-list() #metemos df en lista para cceder mas rapido
  
  for (j in 1:nrow(aux_df)) {
    
    if( is.na(aux_df[nrow(aux_df)+1-j,3]) ){
      
      aux[j]<-0
      y<-aux[j]
      
    }else{
    
    aux[j]<-aux_df[nrow(aux_df)+1-j,3] + y
    y<-aux[j]
    
    }
  }
  
  Fx<- (1-aux)
  
  aux_df<-cbind(aux_df,Fx)
  colnames(aux_df)<-c( paste("P&L sim",ticks[i]) , "nums" , "A_exp" , "Fx")
  assign(paste0("AE", ticks[i]), aux_df  )

  #usar funcion closest
  
} #ya quedaron hijo de prra

#creamos for para asignar a cada elemento de la lista un df de cada emisora y calcularemos el VaR por cada una 

#creamos matriz de VaR con columnas dadas por el num de emisoras
VaR_AEmatrix<-matrix(0,nrow = 3,ncol = length(ticks))

for (k in 1:length(ticks)) {
  
  
  lista[[k]]<- get(paste0("AE", ticks[k]))  #asignamos df al elemento k de la lista
  
  indx1<- Closest( lista[[k]][,4], a=1-alpha[1], which = T)[1]
  indx2<- Closest( lista[[k]][,4], a=1-alpha[2], which = T)[1]
  indx3<- Closest( lista[[k]][,4], a=1-alpha[3], which = T)[1]
  
  VaR_ae1<-lista[[k]][indx1,1]  #var 95
  VaR_ae2<-lista[[k]][indx2,1]  #var 97.5
  VaR_ae3<-lista[[k]][indx3,1]  #var 99
  
  VaR_AEmatrix[1,k]<- VaR_ae1 
  VaR_AEmatrix[2,k]<- VaR_ae2 
  VaR_AEmatrix[3,k]<- VaR_ae3 
  
  }

colnames(VaR_AEmatrix)<-c(paste("VaR P&L",ticks) )
rownames(VaR_AEmatrix)<-c("95%","97.5","99%")
tVaR_AE<-kable(VaR_AEmatrix,digits=4,caption = paste0("VaR por Alisado Exponencial, con alpha=", a ) )

tVaR_AE

#para el AE del portafolio:


portfolioPL_ae<-dataSH$portfolioPL_sh

AEport<-cbind( nums, A_exp, portfolioPL_ae )
AEport<-as.data.frame(AEport)

#ordenamos y repetimos el proceso con closest()

AEport_ord<-arrange(AEport,desc( portfolioPL_ae )) #ordena el dataframe de acuerdo al orden descendente de la Va de P&L

#creamos columna de probas acumuladas

aux<-vector()
y<-0

for (i in 1:nrow(AEport_ord)) {
  
  if( is.na( AEport_ord$A_exp[i] ) ){
    
    aux[i]<-0
    y<-aux[i]
    
  }else{
    
    aux[i]<-AEport_ord$A_exp[i] + y
    y<-aux[i]
    
  }
}

Fx<-(1-aux)

AEport_ord<-cbind(AEport_ord,Fx)

indice1<-Closest( AEport_ord$Fx , a=1-alpha[1], which = T)[1]
indice2<-Closest( AEport_ord$Fx , a=1-alpha[2], which = T)[1]
indice3<-Closest( AEport_ord$Fx , a=1-alpha[3], which = T)[1]

indices<-c(indice1,indice2,indice3)

VaRport_ae<-AEport_ord[indices,3]  
por<-c("95%","97.5%","99%")

VaRport_ae<-cbind(por,VaRport_ae)

VaRP_ae<-kable(VaRport_ae, digits=4,caption = paste0("VaR del Portafolio por Alisado Exponencial, con alpha=", a ) , col.names = c("","VaR"))

VaRP_ae #ya qdo


###################################################################################################################################################################
#                                                               VaR No Diversificado y Diversificado
###################################################################################################################################################################

#matriz covarianza de rendimientos

MCov<-cov(data[,c((length(ticks) +1): (2*length(ticks)) ) ] , use = "complete.obs")
MCov


Cantidad<-c(450,300,200,2000,150,450,1400,450,1200,2500) #Cantidad de acciones de C/u

Monto<-Cantidad*as.numeric(last_prices) #monto de C/u

Share<-Monto/sum(Monto) #% de

#composicion del portafolio

tabcomp<-data.frame()
tabcomp<-as.data.frame(cbind( as.data.frame( ticks) , Cantidad, Monto, Share ))

colnames(tabcomp)<-c("Emisora","No Acciones","Monto","Porcentaje")



#añadimos VaR individual por SH

VaR1<-as.numeric( VaR_SH[1,] ) 
VaR2<-as.numeric( VaR_SH[2,] )
VaR3<-as.numeric( VaR_SH[3,] )

VaR_cols<-cbind(VaR1,VaR2,VaR3)
colnames(VaR_cols)<-c("VaR 95%","VaR 97.5%","VaR 99%")
VaR_cols<-as.data.frame(VaR_cols)

tabcomp<-cbind(tabcomp,VaR_cols)

kable(tabcomp,caption = "Portafolio",digits = 4)
#percentiles

p99<-qnorm(.99)
p975<-qnorm(.975)
p95<-qnorm(.95)

percentiles<-c(p95,p975,p99)

#vector de desv estandar de rendimientos de cada accion (volatilidad diaria)

rendimientos<- data[, (length(ticks)+1):(2*length(ticks))] 

volatilidad<-as.numeric(apply(rendimientos, 2, sd,na.rm=T))

VaRajusatdo<-matrix()

VaRajusatdo<-VaR_cols*Cantidad #VaR ajustado a la posicion de cada accion
colnames(VaRajusatdo)<-c("Adj VaR 95%","Adj VaR 97.5%","Adj VaR 99%")

tabcomp<-cbind(tabcomp,as.data.frame(VaRajusatdo) )

kable(tabcomp)

#sapply=apply(,2,)

VaR_noD<-sapply(tabcomp[,8:10], sum)
VaR_noD #VaR no diversificado del portafolio usando SH

######################################################
#VaR no diversificado parametrico

VaRindiv<-matrix(0,nrow = 3,ncol = length(ticks))

#for para llenar la amtriz

for(i in 1:3){
VaRindiv[i,]<-(volatilidad*Monto)*matrix(percentiles)[i,1]*sqrt(1)  #raiz de uno porque es la raiz del tiempo (1 dia)
}

rownames(VaRindiv)<-c("95%","97.5%","99%")
colnames(VaRindiv)<-paste("VaR",ticks)

#calculamos suma de VaR

VaR_nd<-apply(VaRindiv,1,sum)

VaRindiv<-cbind(VaRindiv,VaR_nd)

colnames(VaRindiv)[ncol(VaRindiv)]<-c("VaR Portafolio") #parametrico

kable(VaRindiv,digits = 4, caption = "VaR del portafolio no diversificado") 

#VaR diversificado

var_portfolio<- Share%*%MCov%*%matrix(Share)
var_portfolio

sd_portfolio<-sqrt(var_portfolio)

VaR_D<-matrix(0,nrow = 3,ncol = 1)

for(i in 1:3){
  VaR_D[i,]<- sd_portfolio*percentiles[i]*sum(Monto)*sqrt(1)  #raiz de uno porque es la raiz del tiempo (1 dia)
}

rownames(VaR_D)<-c("95%","97.5%","99%")
colnames(VaR_D)<- c("VaR Diversificado")

kable(VaR_D,caption = "VaR del Portafolio Diversificado")


