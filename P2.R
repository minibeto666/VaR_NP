library(readxl)
library(readr)
library(knitr)
library(xtable)
library(printr)
library(stargazer)
library(rmarkdown)
library(effects)
library(car)
library(AER)
library(broom)
library(stats)
library(lmtest)
library(sandwich)
library(tinytex)
library(openxlsx)
library(readxl)
library(plot3D)
library(latexpdf)
library(nlWaldTest)
library(forecast)
library(nnet)
library(dplyr)

#datos de VA macro y finan

datos <-  read_excel("~/666/9no semestre/Admin de Riesgos/Proyecto/Proyecto2/Data.xlsx")
#datos sin NA

DF<-na.omit(datos)

#añadimos logatirmos de los valores grandes de los datos

lnIPC<-log(datos$IPC)
lnINPP<-log(datos$INPP)
lnRemesas<-log(datos$Remesas)
lnSMG<-log(datos$SMG)
datos<-cbind(datos,lnIPC,lnINPP,lnRemesas,lnSMG)

#matriz de covarianza y correlacion de los datos

cov(na.omit(datos[,-1]))
cor(na.omit(datos[,-1]))

#datos de PD Santander y TBM

PD_data<-read_excel("~/666/9no semestre/Admin de Riesgos/Proyecto/Proyecto2/Proyecto 2 Stress Testing.xlsx", 
                    sheet = "PD")
colnames(PD_data)[1]<-"Fecha"




#creamos Scores de las probas de incumplimiento

score<-function(p){
  
  score<- log(p/(1-p))
  return(score)
}

Scores<-cbind( PD_data[,1] ,apply(PD_data[,-1],2,score) )

#juntamos bases de datos alv

data<-merge.data.frame(datos,Scores)
data <- data %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) #quita infinitos por NA

#recortamos para obs en toda la base

# Nos quedamos solo con las observaciones donde todas las variables macroeconómicas empiezan
# a tomar valores
data2<-data[62:nrow(data),]



#creamos modelo de regresión lineales para cada uno alv


#empresas
#-------------------------------------------------------------------------------

#santander

ModeloFull_Sempresalog<-lm(S_empresa~INPC+lnIPC+lnINPP+PIB+TIIE+lnRemesas+lnSMG+TipoCambio+TasaCete+TasaDes+UDIs+IAhorro,data = data2) #log
anova(ModeloFull_Sempresalog)
summary(ModeloFull_Sempresalog)

ModeloStep_Sempresalog<-lm(S_empresa~INPC+lnIPC+lnINPP+TipoCambio+IAhorro+lnSMG+UDIs+TasaCete+TasaDes,data = data2) #modelo que minimiza AIC, MEJOR MODELO
anova(ModeloStep_Sempresalog)
summary(ModeloStep_Sempresalog) #mejorcito

#usando los datos del mejor modelo:

modeloNLS_Sempresa <- nls(formula =  S_empresa~a+ b1*INPC+b2*lnIPC+b3*lnINPP+b4*TipoCambio+b5*IAhorro+
                            b6*lnSMG+b7*UDIs+b8*TasaCete+b9*TasaDes, data = data2,
                            start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7=0,b8=0,b9=0),
                            lower = c(a = -Inf,b1 = 0,b2 = -Inf, b3 = 0,b4 = 0,b5 = -Inf,b6 = -Inf,b7=-Inf,b8=0,b9=0),
                            upper = c(a = Inf,b1 = Inf,b2 = Inf, b3 = Inf,b4 = Inf,b5=Inf,b6=Inf,b7=Inf,b8=Inf,b9=Inf),
                            algorithm = "port") 
summary(modeloNLS_Sempresa)


# Gráfico
# Para crear el gráfico ponemos: la PD de la cartera de santander y la PD estimada con el modelo NLS

# Creamos las PD
curvelm<- 1/(1+exp(-(ModeloStep_Sempresalog$fitted.values )))
fitted_curve1<-1/(1+exp(-(modeloNLS_Sempresa$m$fitted())))
sample_curve<-1/(1+exp(-data2$S_empresa))
x<-data2$Fecha

# Creamos el gráfico
plot(x,sample_curve,pch=16,col="gray40",main="Ajuste Modelos Santander Empresa",xlab="Fecha",ylab="PD",bty="L",ylim = c(0,.07))
lines(x,fitted_curve1,col="red",lwd=2)
lines(x,curvelm,col="blue",lwd=2)
# Add a legend
legend("bottom", 
       legend = c("Modelo Lineal", "Modelo NLS","PD"), 
       col = c("blue","red","gray40"), 
       pch = c(19,19,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = "black",
       inset =  c(0, -.07),
       xpd=TRUE,#para sacar del grafico
       horiz = TRUE)



#TBM
ModeloFull_TBMempresalog<-lm(TBM_empresa~INPC+lnIPC+lnINPP+PIB+TIIE+lnRemesas+lnSMG+TipoCambio+TasaCete+TasaDes+UDIs+IAhorro,data = data2)
anova(ModeloFull_TBMempresalog)
summary(ModeloFull_TBMempresalog)

ModeloStep_TBM_empresalog<-lm(formula = TBM_empresa ~ lnIPC + PIB + TIIE + lnRemesas + lnSMG + 
                                TipoCambio + TasaDes + UDIs + IAhorro, data = data2) #echar ojo
anova(ModeloStep_TBM_empresalog)
summary(ModeloStep_TBM_empresalog)

#usando los datos del mejor modelo:

modeloNLS_TBMempresa <- nls(formula =  TBM_empresa ~ a+b1*lnIPC +b2* PIB + b3*TIIE + b4*lnRemesas + b5*lnSMG + 
                              b6*TipoCambio + b7*TasaDes + b8*UDIs + b9*IAhorro, data = data2,
                 start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7=0,b8=0,b9=0),
                 lower = c(a = -Inf,b1 = 0,b2 = -Inf, b3 = 0,b4 = -Inf,b5 = -Inf,b6 = 0,b7=0,b8=-Inf,b9=-Inf),
                 upper = c(a = Inf,b1 = Inf,b2 = 0, b3 = Inf,b4 = Inf,b5=Inf,b6=Inf,b7=Inf,b8=Inf,b9=Inf),
                 algorithm = "port") 
summary(modeloNLS_TBMempresa)


# Gráfico
# Para crear el gráfico ponemos: la PD de la cartera de santander y la PD estimada con el modelo NLS

modeloNLS_TBMempresa$m$fitted()  #Con este obtenemos los valores estimados


# Creamos las PD
curvelm<- 1/(1+exp(-(ModeloStep_TBM_empresalog$fitted.values )))
fitted_curve1<-1/(1+exp(-(modeloNLS_TBMempresa$m$fitted())))
sample_curve<-1/(1+exp(-data2$TBM_empresa))
x<-data2$Fecha

# Creamos el gráfico
plot(x,sample_curve,pch=16,col="gray40",main="Ajuste Modelos TBM Empresa",xlab="Fecha",ylab="PD",bty="L",ylim = c(.01,.055))
lines(x,fitted_curve1,col="red",lwd=2)
lines(x,curvelm,col="blue",lwd=2)
# Add a legend
legend("bottom", 
       legend = c("Modelo Lineal", "Modelo NLS","PD"), 
       col = c("blue","red","gray40"), 
       pch = c(19,19,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = "black",
       inset =  c(0, -.07),
       xpd=TRUE,#para sacar del grafico
       horiz = TRUE)
#consumo
#-------------------------------------------------------------------------------

#Santander
ModeloFull_San_consumolog<-lm(S_consumo~INPC+lnIPC+lnINPP+PIB+TIIE+lnRemesas+lnSMG+TipoCambio+TasaCete+TasaDes+UDIs+IAhorro,data = data2)
anova(ModeloFull_San_consumolog)
summary(ModeloFull_San_consumolog)

step(ModeloFull_San_consumolog,direction = "both",trace = 0)

ModeloStep_San_consumolog<-lm(S_consumo~INPC + lnINPP + lnSMG + TipoCambio + TasaCete + TasaDes,data = data2)
anova(ModeloStep_San_consumolog)
summary(ModeloStep_San_consumolog)


modeloNLS_Sconsumo <- nls(formula =  S_consumo~a+ b1*INPC +b2* lnINPP +b3* lnSMG +b4* TipoCambio +b5* TasaCete + b6*TasaDes, data = data2,
                            start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0),
                            lower = c(a = -Inf,b1 = 0,b2 = 0, b3 = -Inf,b4 = 0,b5 = 0,b6 = 0),
                            upper = c(a = Inf,b1 = Inf,b2 = Inf, b3 = Inf,b4 = Inf,b5=Inf,b6=Inf),
                            algorithm = "port") 
summary(modeloNLS_Sconsumo)


# Gráfico

# Creamos las PD
curvelm<- 1/(1+exp(-(ModeloStep_San_consumolog$fitted.values )))
fitted_curve1<-1/(1+exp(-(modeloNLS_Sconsumo$m$fitted())))
sample_curve<-1/(1+exp(-data2$S_consumo))
x<-data2$Fecha

# Creamos el gráfico
plot(x,sample_curve,pch=16,col="gray40",main="Ajuste Modelos Santander Consumo",xlab="Fecha",ylab="PD",bty="L",ylim = c(.045,.18))
lines(x,fitted_curve1,col="red",lwd=2)
lines(x,curvelm,col="blue",lwd=2)
# Add a legend
legend("bottom", 
       legend = c("Modelo Lineal", "Modelo NLS","PD"), 
       col = c("blue","red","gray40"), 
       pch = c(19,19,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = "black",
       inset =  c(0, -.07),
       xpd=TRUE,#para sacar del grafico
       horiz = TRUE)


#TBM
ModeloFull_TBM_consumolog<-lm(TBM_consumo~INPC+lnIPC+lnINPP+PIB+TIIE+lnRemesas+lnSMG+TipoCambio+TasaCete+TasaDes+UDIs+IAhorro,data = data2)
anova(ModeloFull_TBM_consumolog)
summary(ModeloFull_TBM_consumolog)

step(ModeloFull_TBM_consumolog,direction = "both",trace = 0)

ModeloStep_TBM_consumolog<-lm(TBM_consumo~INPC + lnINPP + lnRemesas + lnSMG + 
                                TipoCambio + TasaCete + TasaDes + UDIs,data = data2) 
anova(ModeloStep_TBM_consumolog)
summary(ModeloStep_TBM_consumolog)

modeloNLS_TBMconsumo <- nls(formula =  TBM_consumo~a +b1*INPC + b2*lnINPP +b3* lnRemesas +b4* lnSMG + 
                            b5*TipoCambio +b6* TasaCete +b7* TasaDes +b8* UDIs, data = data2,
                          start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7=0,b8=0),
                          lower = c(a = -Inf,b1 = 0,b2 = 0, b3 = -Inf,b4 = -Inf,b5 = 0,b6 = 0,b7=0,b8=-Inf),
                          upper = c(a = Inf,b1 = Inf,b2 = Inf, b3 = Inf,b4 = Inf,b5=Inf,b6=Inf,b7=Inf,b8=Inf),
                          algorithm = "port") 
summary(modeloNLS_TBMconsumo)


# Gráfico

# Creamos las PD
curvelm<- 1/(1+exp(-(ModeloStep_TBM_consumolog$fitted.values )))
fitted_curve1<-1/(1+exp(-(modeloNLS_TBMconsumo$m$fitted())))
sample_curve<-1/(1+exp(-data2$TBM_consumo))
x<-data2$Fecha
# Creamos el gráfico
plot(x,sample_curve,pch=16,col="gray40",main="Ajuste Modelos TBM Consumo",xlab="Fecha",ylab="PD",bty="L",ylim = c(.06,.25))
lines(x,fitted_curve1,col="red",lwd=2)
lines(x,curvelm,col="blue",lwd=2)
# Add a legend
legend("bottom", 
       legend = c("Modelo Lineal", "Modelo NLS","PD"), 
       col = c("blue","red","gray40"), 
       pch = c(19,19,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = "black",
       inset =  c(0, -.07),
       xpd=TRUE,#para sacar del grafico
       horiz = TRUE)

#Vivienda
#-------------------------------------------------------------------------------

#Santander
ModeloFull_San_viviendalog<-lm(S_vivienda~INPC+lnIPC+lnINPP+PIB+TIIE+lnRemesas+lnSMG+TipoCambio+TasaCete+TasaDes+UDIs+IAhorro,data = data2)
anova(ModeloFull_San_viviendalog)
summary(ModeloFull_San_viviendalog)

step(ModeloFull_San_viviendalog,direction = "both",trace = 0)

ModeloStep_San_viviendalog<-lm(S_vivienda ~ INPC + lnINPP + PIB + lnSMG + TipoCambio + 
                                TasaCete + TasaDes + UDIs,data = data2)
anova(ModeloStep_San_viviendalog)
summary(ModeloStep_San_viviendalog)


modeloNLS_Svivienda <- nls(formula =  S_vivienda ~a +b1* INPC +b2* lnINPP + b3*PIB +b4* lnSMG +b5* TipoCambio + 
                             b6*TasaCete + b7*TasaDes +b8* UDIs, data = data2,
                          start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7=0,b8=0),
                          lower = c(a = -Inf,b1 = 0,b2 = 0, b3 = -Inf,b4 = -Inf,b5 = 0,b6 = 0,b7=0,b8=-Inf),
                          upper = c(a = Inf,b1 = Inf,b2 = Inf, b3 = 0,b4 = Inf,b5=Inf,b6=Inf,b7=Inf,b8=Inf),
                          algorithm = "port") 
summary(modeloNLS_Svivienda)


# Gráfico

# Creamos las PD
curvelm<- 1/(1+exp(-(ModeloStep_San_viviendalog$fitted.values )))
fitted_curve1<-1/(1+exp(-(modeloNLS_Svivienda$m$fitted())))
sample_curve<-1/(1+exp(-data2$S_vivienda))
x<-data2$Fecha

# Creamos el gráfico
plot(x,sample_curve,pch=16,col="gray40",main="Ajuste Modelos Santander Vivienda",xlab="Fecha",ylab="PD",bty="L",ylim = c(.015,.07))
lines(x,fitted_curve1,col="red",lwd=2)
lines(x,curvelm,col="blue",lwd=2)
# Add a legend
legend("bottom", 
       legend = c("Modelo Lineal", "Modelo NLS","PD"), 
       col = c("blue","red","gray40"), 
       pch = c(19,19,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = "black",
       inset =  c(0, -.07),
       xpd=TRUE,#para sacar del grafico
       horiz = TRUE)


#TBM
ModeloFull_TBM_viviendalog<-lm(TBM_vivienda~INPC+lnIPC+lnINPP+PIB+TIIE+lnRemesas+lnSMG+TipoCambio+TasaCete+TasaDes+UDIs+IAhorro,data = data2)
anova(ModeloFull_TBM_viviendalog)
summary(ModeloFull_TBM_viviendalog)

step(ModeloFull_TBM_viviendalog,direction = "both",trace = 0)

ModeloStep_TBM_viviendalog<-lm(TBM_vivienda ~ INPC + lnIPC + lnINPP + PIB + TIIE + 
                                 lnRemesas + lnSMG + TipoCambio + TasaDes + UDIs,data = data2) 
anova(ModeloStep_TBM_viviendalog)
summary(ModeloStep_TBM_viviendalog)

modeloNLS_TBMvivienda <- nls(formula =  TBM_vivienda ~a +b1* INPC +b2* lnIPC + b3*lnINPP + b4*PIB + b5*TIIE + 
                               b6*lnRemesas + b7*lnSMG + b8*TipoCambio + b9*TasaDes + b10*UDIs, data = data2,
                            start = list(a = 0,b1 = 0.0,b2 = 0, b3 = 0,b4 = 0,b5 = 0,b6 = 0,b7=0,b8=0,b9=0,b10=0),
                            lower = c(a = -Inf,b1 = 0,b2 = 0, b3 = 0,b4 = -Inf,b5 = 0,b6 = -Inf,b7=-Inf,b8=0,b9=0,b10=-Inf),
                            upper = c(a = Inf,b1 = Inf,b2 = Inf, b3 = Inf,b4 = 0,b5=Inf,b6=Inf,b7=Inf,b8=Inf,b9=Inf,b10=Inf),
                            algorithm = "port") 
summary(modeloNLS_TBMvivienda)


# Gráfico

# Creamos las PD
curvelm<- 1/(1+exp(-(ModeloStep_TBM_viviendalog$fitted.values )))
fitted_curve1<-1/(1+exp(-(modeloNLS_TBMvivienda$m$fitted())))
sample_curve<-1/(1+exp(-data2$TBM_vivienda))
x<-data2$Fecha

# Creamos el gráfico
plot(x,sample_curve,pch=16,col="gray40",main="Ajuste Modelos TBM vivienda",xlab="Fecha",ylab="PD",bty="L",ylim = c(.03,.07))
lines(x,fitted_curve1,col="red",lwd=2)
lines(x,curvelm,col="blue",lwd=2)
# Add a legend
legend("bottom", 
       legend = c("Modelo Lineal", "Modelo NLS","PD"), 
       col = c("blue","red","gray40"), 
       pch = c(19,19,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = "black",
       inset =  c(0, -.07),
       xpd=TRUE,#para sacar del grafico
       horiz = TRUE)
