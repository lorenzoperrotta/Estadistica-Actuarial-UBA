################################
####      TP FINAL          ####
#### ESTADISTICIA ACTUARIAL ####
####      GRUPO 16          ####
################################

#CARGO LA SERIE
Serie=read.csv2(file.choose())

setwd("C:/Users/Loren/Desktop/TP ESTADISTICA ACTUARIAL")
getwd()

###################
###   Serie A   ###
###################


Serie1=Serie$Serie_A


#la defino como serie de tiempo y chequeo que lo sea (ojo que en el script que pasaste del punto 1 no estaba esto)
SerieA=ts(Serie1)
data.class(Serie1)

library(moments)
#ANÁLISIS DESCRIPTIVO

s1<-summary(SerieA);s1
quantile(SerieA)
v1<-var(SerieA)
sd1<-sd(SerieA)
k1<-kurtosis(SerieA)
sk1<-skewness(SerieA)
tablaA<-data.frame(cbind(v1,sd1,k1,sk1))
colnames(tablaA)=c("Varianza", "Desvio", "Curtosis", "Asimetría")
rownames(tablaA)=("Serie A")
tablaA
#medidas de tendencia central tienden a 0, falta interpretar asimetría y curtosis

install.packages("xtable")
library(xtable)

print(xtable(tablaA, type = "latex"), file = "Serie A2.tex")

ss1<-data.frame(unclass(s1))
ss1<-t(ss1)
rownames(ss1)=("Serie A")
colnames(ss1)=c("Mínimo", "1° Cuartil", "Mediana", "Media", "3° Cuartil", "Máximo")
ss1
print(xtable(ss1, type = "latex"), file = "Serie A1.tex")

#ANÁLISIS GRÁFICO

par(mfrow=c(2,2))
plot(SerieA, main="Gráfico de la Serie",col="red")
acf(SerieA,main="FAS",type=c("covariance"))
acf(SerieA,main="FAC",type="correlation")
pacf(SerieA,main="FACP")
#parece ser estacionaria a primera vista, autocorrelacion parcial tiende a 0 cuando t tiende a infinito;
#serie revierte a la media

boxplot(SerieA,col="orange")
#boxplot presenta más outliers en los picos que en los mínimos, pero la media está muy cercana a 0

#ANÁLISIS DE ESTACIONARIEDAD

#Test Dickey-Fuller:
install.packages("urca")
library(urca)

kpss.test(SerieA, null = c("Level","Trend")) #TENDENCIA DETERMINISTICA
summary(trend.df<-ur.df(SerieA,type="trend",lags = 2)) #RAIZ UNITARIA
summary(drift.df<-ur.df(SerieA,type="drift",lags = 2))
summary(none.df<-ur.df(SerieA,type="none",lags = 2))
pp.test(SerieA) #SIGNIFICATIVIDAD CONJUNTA

Tau_99 = c(-2.6,-3.51,-4.04)     #99% Nivel de Confianza
Tau_95 = c(-1.95, -2.89, -3.45)  #95% Nivel de Confianza
Tau_90 = c(-1.61, -2.58, -3.15)  #90% Nivel de Confianza
Dicky_Fuller_TS_A = c(none.df@teststat,drift.df@teststat[1],trend.df@teststat[1])
Tabla_TAU=data.frame(Tau_90, Tau_95, Tau_99, Dicky_Fuller_TS_A)
rownames(Tabla_TAU) <- c("None", "Drift", "Trend")
colnames(Tabla_TAU) <- c("Tau 90%","Tau 95%","Tau 99%","Valor Empírico")
Tabla_TAU
print(xtable(Tabla_TAU, type = "latex"), file = "Serie A DF.tex")


#ELECCIÓN DEL MODELO
#Busco cuál es el mejor modelo; vamos a generar modelos aleatorios ARMA, del (0,0) hasta el (5,5);
#y comparo según los valores de AIC/BIC que salgan del objeto "gaich"
#columna 1: aic, columna 2: bic
#uso la función modelitos

library(tseries)
SerieA

modelitos<-function(x,y){
vector1<-c()
vector2<-c()
  for(i in 0:x){
  for(j in 0:y){
    modelo<-arima(SerieA,order=c(i,0,j))
    summary(modelo)
    aise<-AIC(modelo)
    vector1<-c(vector1,aise)
    bise<-BIC(modelo)
    vector2<-c(vector2,bise)
  }
juanfer<-data.frame(vector1,vector2)
      }
return(juanfer)
}

gaich<-modelitos(5,5)
gaich
colnames(gaich)=c("AIC","BIC")
rownames(gaich)=c("(0,0)","(0,1)","(0,2)","(0,3)","(0,4)","(0,5)",
                  "(1,0)","(1,1)","(1,2)","(1,3)","(1,4)","(1,5)",
                  "(2,0)","(2,1)","(2,2)","(2,3)","(2,4)","(2,5)",
                  "(3,0)","(3,1)","(3,2)","(3,3)","(3,4)","(3,5)",
                  "(4,0)","(4,1)","(4,2)","(4,3)","(4,4)","(4,5)",
                  "(5,0)","(5,1)","(5,2)","(5,3)","(5,4)","(5,5)")
gaich

gaich<-t(gaich)
gaich
print(xtable(gaich, type = "latex"), file = "Serie A info.tex")

gaich1<-data.frame(gaich)
mode(gaich1)

install.packages("writexl")
library(writexl)
write_xlsx(gaich1,"SerieA.xlsx")
write.csv(gaich,"SerieA.csv")
  
  
#aclaración:
#1.(0,0)
#2.(0,1)
#3.(0,2)
#4.(0,3)
#5.(0,4)
#6.(0,5)
#7.(1,0)
#8.(1,1)
#9.(1,2)
#y así sucesivamente

#modelos candidatos; #MA(3),MA(4),ARMA(1,3)

#SIGNIFICATIVIDAD INDIVIDUAL
matres<-arima(SerieA,order=c(0,0,3))
summary(matres)
#los coeficientes de la parte ma son todos significativos, el intercepto no

macuatro<-arima(SerieA,order=c(0,0,4))
summary(macuatro)
#los coeficientes de la parte ma son todos significativos y el intercepto no

armaunotres<-arima(SerieA,order=c(1,0,3))
summary(armaunotres)
#los coeficientes de la parte ma son todos significativos, los de la parte ar y el intercepto no

MA3N<-arima(SerieA,order=c(0,0,3),include.mean = FALSE)
MA3N #Nos quedamos con este modelo


#PREDICCIÓN

#Distintos Horizontes de Predicción
Predict20=predict(MA3N,n.ahead=20)$pred
Predict20
Error20=predict(MA3N,n.ahead=20)$se
Error20
Serie20p=c(SerieA,Predict20)
Serie20p
Predict3=predict(MA3N,n.ahead=3)$pred
Predict3
Error3=predict(MA3N,n.ahead=3)$se
Error3
Serie3p=c(SerieA,Predict3)
Serie3p
Predict2=predict(MA3N,n.ahead=2)$pred
Predict2
Error2=predict(MA3N,n.ahead=2)$se
Error2
Serie2p=c(SerieA,Predict2)
Serie2p
Predict1=predict(MA3N,n.ahead=1)$pred
Predict1
Error1=predict(MA3N,n.ahead=1)$se
Error1
Serie1p=c(SerieA,Predict1)
Serie1p

#Gráfico

par(mfrow=c(2,2))
plot(Serie1p,type="l",main="Predicción en n=1",col="brown",ylab=c("Valores de la variable"),xlab="Tiempo",lwd=1)
plot(Serie2p,type="l",main="Predicción en n=2",col="brown",ylab=c("Valores de la variable"),xlab="Tiempo",lwd=1)
plot(Serie3p,type="l",main="Predicción en n=3",col="brown",ylab=c("Valores de la variable"),xlab="Tiempo",lwd=1)
plot(Serie20p,type="l",main="Predicción en n=20",col="brown",ylab=c("Valores de la variable"),xlab="Tiempo",lwd=1)

#INTERVALOS DE CONFIANZA

install.packages("forecast")
library("forecast")

IC1.P1=forecast(MA3N,h=1,level= c(0.94,0.95,0.99))
IC1.P1
IC1.P2=forecast(MA3N,h=2,level= c(0.94,0.95,0.99))
IC1.P2
IC1.P3= forecast(MA3N,h=3,level= c(0.94,0.95,0.99))
IC1.P3
IC1.P20=forecast(MA3N,h=20,level= c(0.94,0.95,0.99))
IC1.P20


#Unimos los datos
DFIC1=data.frame(IC1.P20)
DFIC1=as.data.frame (DFIC1[c(0,2,4,6,1,3,5,7)])
colnames(DFIC1)=c("94%","95%","99%","Predicción","94%","95%","99%")
View(DFIC1)
DFIC1
write_xlsx(DFIC1,"IC SerieA.xlsx")
#Exportamos las Predicciones de la serie A en formato .csv
write.csv2(DFIC1,"C:/Users/aldub/Documents/DFIC1.csv") 

#ANÁLISIS DE LOS RESIDUOS 
Residuos1=residuals(MA3N)
summary(Residuos1)

#NORMALIDAD
#Gráfico de la Normalidad de Residuos
par(mfrow=c(1,1))
qqnorm(Residuos1)
qqline(Residuos1,col="red")

#Test de Normalidad 
shapiro.test(Residuos1)
jarque.bera.test(Residuos1) #No ReH0: Residuos Normales
install.packages("nortest")
library(nortest)
ad.test(Residuos1)

#INCORRELACION
Box.test(Residuos1,type="Ljung-Box") #No ReH0: Residuos Incorrelacionados

#HOMOCEDASTICIDAD
white.test(Residuos1) #No ReH0: Hay presencia de homocedastidad

#Gráficos
par(mfrow=c(2,2))
plot(Residuos1,main="Residuos",col="brown")
acf(Residuos1,main="FAC Residuos",col="brown")
pacf(Residuos1,main="FACP Residuos",col="brown")

-----------------------------------------------------------------------------------------------------
###################
###   Serie B   ###
###################

#CARGO LA SERIE

Serie2=Serie$Serie_B

SerieB=ts(Serie2)
data.class(SerieB)

#ANÁLISIS DESCRIPTIVO

s2<-summary(SerieB)
v2<-var(SerieB)
sd2<-sd(SerieB)
library(moments)
k2<-kurtosis(SerieB)
sk2<-skewness(SerieB)

#desvío bastante alto, media y tendencias centrales lejos de 0
#leptocúrtica
#asimetría negativa, la mediana es más grande que la media
#parecería no ser estacionaria
#intentamos confirmarlo con los gráficos

tablaB<-data.frame(cbind(v2,sd2,k2,sk2))
colnames(tablaB)=c("Varianza", "Desvio", "Curtosis", "Asimetría")
rownames(tablaB)=("Serie B")
tablaB
print(xtable(tablaB, type = "latex"), file = "Serie B2.tex")

ss2<-data.frame(unclass(s2))
ss2<-t(ss2)
rownames(ss2)=("Serie B")
colnames(ss2)=c("Mínimo", "1° Cuartil", "Mediana", "Media", "3° Cuartil", "Máximo")
ss2
print(xtable(ss2, type = "latex"), file = "Serie B1.tex")

#ANÁLISIS GRÁFICO

par(mfrow=c(2,2))
plot(SerieB,main="Gráfico de la Serie",col="red")
acf(SerieB,main="FAS",type=c("covariance"))
acf(SerieB,main="FAC",type="correlation")
pacf(SerieB,main="FACP")
#autocorrelación decrece en forma lineal, serie no revierte alrededor de una media, todo indica que no es estacionaria

#limpio los plots para que me lo muestre en una sola
boxplot(SerieB,col="light blue")
#media alrededor de 2, clara presencia de outliers a ambos extremos

#ANÁLISIS DE ESTACIONARIEDAD

#Test Dickey-Fuller:
kpss.test(SerieB, null = c("Level","Trend")) #TENDENCIA DETERMINISTICA
pp.test(SerieB) #SIGNIFICATIVIDAD CONJUNTA

summary(trend.df<-ur.df(SerieB,type="trend",lags = 2)) #RAIZ UNITARIA
summary(drift.df<-ur.df(SerieB,type="drift",lags = 2))
summary(none.df<-ur.df(SerieB,type="none",lags = 2))

Tau_99 = c(-2.6,-3.51,-4.04)     #99% Nivel de Confianza
Tau_95 = c(-1.95, -2.89, -3.45)  #95% Nivel de Confianza
Tau_90 = c(-1.61, -2.58, -3.15)  #90% Nivel de Confianza
Dicky_Fuller_TS_B = c(none.df@teststat,drift.df@teststat[1],trend.df@teststat[1])
Tabla_TAU_B=data.frame(Tau_90, Tau_95, Tau_99, Dicky_Fuller_TS_B)
rownames(Tabla_TAU_B) <- c("None", "Drift", "Trend")
colnames(Tabla_TAU_B) <- c("Tau 90%","Tau 95%","Tau 99%","Valor Empírico")
Tabla_TAU_B
#chequeo que NO es estacionaria

print(xtable(Tabla_TAU_B, type = "latex"), file = "Serie B.tex")


##### A PARTIR DE ACÁ, DIFERENCIO LA SERIE Y TRABAJO CON LA SERIE DIFERENCIADA ####

Serie2dif<-diff(Serie2)
SerieBdif=ts(Serie2dif)
data.class(SerieBdif)

#ANÁLISIS DESCRIPTIVO

s3<-summary(SerieBdif)
v3<-var(SerieBdif)
sd3<-sd(SerieBdif)
library(moments)
k3<-kurtosis(SerieBdif)
sk3<-skewness(SerieBdif)
#media tiende a 0, desvío razonable, mucho mejor

tablaBdif<-data.frame(cbind(v3,sd3,k3,sk3))
colnames(tablaBdif)=c("Varianza", "Desvio", "Curtosis", "Asimetría")
rownames(tablaBdif)=("Serie B Dif.")
tablaBdif
print(xtable(tablaBdif, type = "latex"), file = "Serie Bdif2.tex")

ss3<-data.frame(unclass(s3))
ss3<-t(ss3)
rownames(ss3)=("Serie B Dif.")
colnames(ss3)=c("Mínimo", "1° Cuartil", "Mediana", "Media", "3° Cuartil", "Máximo")
ss3
print(xtable(ss3, type = "xlsx"), file = "SerieBdif1.xlsx")


#ANÁLISIS GRÁFICO

par(mfrow=c(2,2))
plot(SerieBdif,main="Gráfico Serie Dif.",col="red")
acf(SerieBdif,main="FAS",type=c("covariance"))
acf(SerieBdif,main="FAC",type="correlation")
pacf(SerieBdif,main="FACP")
#autocorrelación y media bien, medio rara la pacf pero bueno

#limpio los plots para que me lo muestre en una sola
boxplot(SerieBdif,col="green")
#mucho mejor

#ANÄLISIS DE ESTACIONARIEDAD

#Test Dickey-Fuller:
kpss.test(SerieBdif, null = c("Level","Trend")) #TENDENCIA DETERMINISTICA
summary(trend.df<-ur.df(SerieBdif,type="trend",lags = 2)) #RAIZ UNITARIA
summary(drift.df<-ur.df(SerieBdif,type="drift",lags = 2))
summary(none.df<-ur.df(SerieBdif,type="none",lags = 2))

Tau_99 = c(-2.6,-3.51,-4.04)     #99% Nivel de Confianza
Tau_95 = c(-1.95, -2.89, -3.45)  #95% Nivel de Confianza
Tau_90 = c(-1.61, -2.58, -3.15)  #90% Nivel de Confianza
Dicky_Fuller_TS_Bdif = c(none.df@teststat,drift.df@teststat[1],trend.df@teststat[1])
Tabla_TAU_Bdif=data.frame(Tau_90, Tau_95, Tau_99, Dicky_Fuller_TS_Bdif)
rownames(Tabla_TAU_Bdif) <- c("None", "Drift", "Trend")
colnames(Tabla_TAU_Bdif) <- c("Tau 90%","Tau 95%","Tau 99%","Valor Empírico")
Tabla_TAU_Bdif
#chequeo que SI es estacionaria

print(xtable(Tabla_TAU_Bdif, type = "latex"), file = "Serie B DiF.tex")


#ELECCIÓN DEL MODELO

#busco el mejor modelo, lo mismo que antes pero con la función "modelitos dos"
modelitosdos<-function(x,y){
  vector1<-c()
  vector2<-c()
  for(i in 0:x){
    for(j in 0:y){
      modelo<-arima(SerieBdif,order=c(i,0,j))
      summary(modelo)
      aise<-AIC(modelo)
      vector1<-c(vector1,aise)
      bise<-BIC(modelo)
      vector2<-c(vector2,bise)
    }
    juanfer<-data.frame(vector1,vector2)
  }
  return(juanfer)
}

penco<-modelitosdos(5,5)
#"penco"es el vector de los AIC/BIC
penco
#los mejores son el ARMA(0,3) y el ARMA(2,5)
#me quedo con el ARMA(2,5) aunque el (0,3) tenga menos BIC pues es muy significativa la diferencia en AIC

colnames(penco)=c("AIC","BIC")
rownames(penco)=c("(0,0)","(0,1)","(0,2)","(0,3)","(0,4)","(0,5)",
                  "(1,0)","(1,1)","(1,2)","(1,3)","(1,4)","(1,5)",
                  "(2,0)","(2,1)","(2,2)","(2,3)","(2,4)","(2,5)",
                  "(3,0)","(3,1)","(3,2)","(3,3)","(3,4)","(3,5)",
                  "(4,0)","(4,1)","(4,2)","(4,3)","(4,4)","(4,5)",
                  "(5,0)","(5,1)","(5,2)","(5,3)","(5,4)","(5,5)")

penco<-t(penco)
penco
#print(xtable(gaich, type = "latex"), file = "Serie A info.tex")

penco1<-data.frame(penco)
mode(penco1)
write_xlsx(penco1,"SerieB.xlsx")

#modelos candidatos; #MA(3),ARMA(2,5)

#SIGNIFICATIVIDAD INDIVIDUAL

#evaluo significatividad de ambos para volver a chequear
armadoscinco<-arima(SerieBdif,order=c(2,0,5))
summary(armadoscinco)
#todos los coeficientes son significativos
#el coeficiente ar(1) da mayor a 1 en módulo
#no se cumplen las condiciones de liapunov, es decir, el modelo sería no estacionario

matres<-arima(SerieBdif,order=c(0,0,3))
summary(matres)
#el ARMA (0,3) dan todos los coeficientes significativos salvo el intercepto, nos quedamos con ese

ARIMA03<-arima(SerieBdif,order=c(0,0,3),include.mean = FALSE)
ARIMA03 #Nos quedamos con este modelo, sin el intercepto, por eso el false


#PREDICCIÓN

#Distintos Horizontes de Predicción
P1=predict(ARIMA03,n.ahead=1)$pred;P1
E1=predict(ARIMA03,n.ahead=1)$se;E1
SyP1=c(SerieB,P1);SyP1
P2=predict(ARIMA03,n.ahead=2)$pred;P2
E2=predict(ARIMA03,n.ahead=2)$se;E2
SyP2=c(SerieB,P2);SyP2
P3=predict(ARIMA03,n.ahead=3)$pred;P3
E3=predict(ARIMA03,n.ahead=3)$se;E3
SyP3=c(SerieB,P3);SyP3
P20=predict(ARIMA03,n.ahead=20)$pred;P20
E20=predict(ARIMA03,n.ahead=20)$se;E20
SyP20=c(SerieB,P20);SyP20


#Gráfico
par(mfrow=c(2,2))
plot(SyP1,type="l",main="Predicción en n=1",col="purple",ylab=c("Valores de la variable"),xlab="Tiempo",lwd=1)
plot(SyP2,type="l",main="Predicción en n=2",col="purple",ylab=c("Valores de la variable"),xlab="Tiempo",lwd=1)
plot(SyP3,type="l",main="Predicción en n=3",col="purple",ylab=c("Valores de la variable"),xlab="Tiempo",lwd=1)
plot(SyP20,type="l",main="Predicción en n=20",col="purple",ylab=c("Valores de la variable"),xlab="Tiempo",lwd=1)


#INTERVALOS DE CONFIANZA

library(forecast)
IC.P1 <- forecast(ARIMA03,h=1,level= c(0.94,0.95,0.99));IC.P1
IC.P2 <- forecast(ARIMA03,h=2,level= c(0.94,0.95,0.99));IC.P2
IC.P3 <- forecast(ARIMA03,h=3,level= c(0.94,0.95,0.99));IC.P3
IC.P20 <- forecast(ARIMA03,h=20,level= c(0.94,0.95,0.99));IC.P20

#Unimos los datos
DFIC <- data.frame(IC.P20)
DFIC <- as.data.frame (DFIC[c(0,2,4,6,1,3,5,7)])
colnames(DFIC)=c("94%","95%","99%","Predicción","94%","95%","99%")
View(DFIC)
DFIC
write_xlsx(DFIC,"IC SerieB.xlsx")
#Exportamos las Predicciones de la serie B en formato .csv
write.csv2(DFIC,"C:/Users/aldub/Documents/DFIC.csv")

#ANÁLISIS DE LOS RESIDUOS
residuos<-residuals(ARIMA03)
summary(residuos)

#NORMALIDAD
#Gráfico de la Normalidad de Residuos
par(mfrow=c(1,1))
qqnorm(residuos,col="red")
qqline(residuos,col="blue")

#Test de Normalidad
shapiro.test(residuos)
library(tseries)
jarque.bera.test(residuos)#No ReH0: Residuos Normales
library(nortest)
ad.test(residuos)


#INCORRELACION
Box.test(residuos,type="Ljung-Box") #No ReH0: Residuos Incorrelacionados

#HOMOCEDASTICIDAD
white.test(residuos) #No ReH0: Hay presencia de homocedastidad

#Gráficos
par(mfrow=c(2,2))
plot(residuos,main="Residuos",col="purple")
acf(residuos,main="FAC Residuos",col="purple")
pacf(residuos,main="FACP Residuos",col="purple")

