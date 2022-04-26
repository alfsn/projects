##### TP FINAL #####
# Series de Tiempo # 
# Prof. Dra. Magdalena Cornejo #
# Francisco Guerrero & Alfredo Sampron #


##### PAQUETES #####
#install.packages("forecast")
#install.packages("rugarch")


library(zoo)
library(urca)
library(forecast)
library(stargazer)
library(vars)
library(ggplot2)
library(ggfortify)
library(TSA)
library(dynlm)
library(dynamac)
library(TSstudio)

#### SERIE A ESTIMAR ####
setwd("D:/RStudio/CodigoR/SeriesTiempo/TPFinal")
M1 = read.csv('M1.csv')

M1=ts(M1[,2], frequency=12, start=c(2005,1))

#### TESTS DE RAIZ UNITARIA ####
summary(ur.df(M1, type = 'drift', selectlags = 'BIC'))
# ENCONTRAMOS QUE NO ES ESTACIONARIA DE ORDEN 0

summary(ur.df(diff(log(M1)), 
              type = 'drift', selectlags = 'BIC'))
# ENCONTRAMOS QUE ES ESTACIONARIA DE ORDEN 1

#### DEFINICION DE SERIE, INSAMPLE OUT OF SAMPLE ####

# usamos I(1)
serie=diff(log(M1))
length(serie)

in_sample <- serie[1:156] #80% in sample
in_sample <- ts(in_sample, start=c(2005,1), frequency = 12)

out_of_sample <- serie[157:196] 
#20% out of sample
out_of_sample <- ts(out_of_sample, start=c(2018,1), frequency = 12)

#### ARIMA ####

fit <- auto.arima(in_sample, ic = "bic", seasonal=FALSE)
summary(fit)
# NOS DEVUELVE UN PROCESO AR(2)

fit2 <- Arima(out_of_sample, order= c(2,0,0))
arimaforecast <- fitted(fit2)

plot.ts(arimaforecast, main="Forecast AR(2) vs Actual for Diff^2M1")
par(new = TRUE)
plot.ts(out_of_sample, col="red", axes = FALSE, bty = "n", xlab="", ylab="")



# Reconstruyendo la serie en nivel a partir de los pronosticos del AR(4):
level_forecast_ar2 <- exp(arimaforecast+lag(log(M1)[157:196],-2))
# Es 2 lags porque estamos en I(2)

plot.ts(level_forecast_ar2, xaxt="n", yaxt="n", ann=FALSE)
par(new=TRUE)
plot.ts(M1[157:197], col="red", main="Forecast ARIMA(2,2,0) vs Actual for M1")
legend("bottomright",c("Forecast","Actual"),cex=0.8,col=c("black","red"), lty=1, lwd=c(2.5,2.5))


#### SARIMA ####

fit_sarima <- auto.arima(in_sample, ic = "bic", 
                         seasonal=TRUE)
                         
summary(fit_sarima)
# Nos devuelve un SARIMA(0,0,1)(0,1,1)

fit3 <- Arima(out_of_sample, order= c(0,0,1), seasonal=c(0,1,1))

sarimaforecast <- fitted(fit3)


plot.ts(sarimaforecast, main="Forecast SAR(2,1) vs Actual for Diff^2M1")
par(new = TRUE)
plot.ts(out_of_sample, col="red", axes = FALSE, bty = "n", xlab="", ylab="")



# Reconstruyendo la serie en nivel a partir de los pronosticos del AR(4):
level_forecast_sar <- exp(sarimaforecast+lag(log(M1)[157:196],-2))
# Es 2 lags porque estamos en I(2)

plot.ts(level_forecast_sar, xaxt="n", yaxt="n", ann=FALSE)
par(new=TRUE)
plot.ts(M1[157:197], col="red", main="Forecast SARIMA(4,2,0)(2,1,0) vs Actual for M1")
legend("bottomright",c("Forecast","Actual"),cex=0.8,col=c("black","red"), lty=1, lwd=c(2.5,2.5))



#### ETS ####

#fit.ets <- ets(in_sample)
#summary(fit.ets)
#checkresiduals(fit.ets)

#fcst.ets <- forecast(fit.ets, h = length(out_of_sample))

#autoplot(fcst.ets)
#### SARIMAX ####
# SERIES EXOGENAS DE DATOS

# CERCANIA A ELECCIONES 
# Ciclo político genera presiones a aumentar el gasto sin aumentar los impuestos
# Fuente: elaboracion propia
elect = read.csv("D:/RStudio/CodigoR/SeriesTiempo/TPFinal/Data/meses_hasta_elecciones.csv")
elect= ts(elect[,2], frequency=12, start=c(2005,1))


summary(ur.df(elect, type = 'drift', selectlags = 'BIC'))
# ENCONTRAMOS QUE ES ESTACIONARIA DE ORDEN 0


# ESTIMADOR MENSUAL (EMAE)
# El ingreso afecta la demanda de dinero y la recaudación
# Fuente: INDEC
emae = read.csv("D:/RStudio/CodigoR/SeriesTiempo/TPFinal/Data/emae.csv")
emae= ts(emae[,2], frequency=12, start=c(2005,1))

summary(ur.df(emae, type = 'drift', selectlags = 'BIC'))
# ENCONTRAMOS QUE  ES ESTACIONARIA DE ORDEN 0

# DEFICIT FISCAL
# Genera toma de deuda o emisión, utilizamos bache fiscal base caja ultimos 12 meses
# Fuente: elaboracion propia en base a datos MECON 
# https://www.economia.gob.ar/datos/ > Recursos tributarios > TN > RESULTADO FINANCIERO ANTES DE FIGURATIVOS
bache = read.csv("D:/RStudio/CodigoR/SeriesTiempo/TPFinal/Data/bache2.csv")

bache= ts(bache[2], frequency=12, start=c(2005,1))

summary(ur.df(bache, type = 'drift', selectlags = 'BIC'))
# ENCONTRAMOS QUE NO ES ESTACIONARIA DE ORDEN 0

summary(ur.df(diff(bache), type = 'drift', selectlags = 'BIC'))
# ENCONTRAMOS QUE ES ESTACIONARIA DE ORDEN 1
dbache=diff(bache)


# PRECIO INTERNACIONAL DE LA SOJA
# Determinante de la recaudación impositiva
soja = read.csv("D:/RStudio/CodigoR/SeriesTiempo/TPFinal/Data/soja.csv")
soja= ts(soja[,2], frequency=12, start=c(2005,1))
is.numeric(bache)

summary(ur.df(soja, type = 'drift', selectlags = 'BIC'))
# ENCONTRAMOS QUE NO ES ESTACIONARIA DE ORDEN 0
summary(ur.df(diff(soja), type = 'drift', selectlags = 'BIC'))
# ENCONTRAMOS QUE ES ESTACIONARIA DE ORDEN 1

dsoja = diff(log(soja))

# ARMO MATRIZ CON REGRESORES
xreg=cbind(elect[2:length(elect)], 
                 emae[2:length(emae)], 
                 dbache,
                 dsoja)
#droppeo la primera fila


#length(out_of_sample)
#length(xreg[157:196,1:4])

fit <- auto.arima(in_sample, ic = "aic", 
                  seasonal=FALSE, xreg=xreg[1:156,3:4])
summary(fit)


fit4 <- arima(out_of_sample, order= c(4,0,0), 
              seasonal=c(2,1,0),
              xreg=xreg[157:196,1:4])
sarimaxforecast <- fitted(fit4)


plot.ts(sarimaxforecast, main="Forecast AR(4)SAR(2,1) vs Actual for Diff^2M1")
par(new = TRUE)
plot.ts(out_of_sample, col="red", axes = FALSE, bty = "n", xlab="", ylab="")



# Reconstruyendo la serie en nivel a partir de los pronosticos del AR(4):
level_forecast_sarx <- exp(sarimaxforecast+lag(log(M1)[157:197],-2))
# Es 2 lags porque estamos en I(2)

plot.ts(level_forecast_sarx, xaxt="n", yaxt="n", ann=FALSE)
par(new=TRUE)
plot.ts(M1[157:197], col="red", main="Forecast sarimax(4,2,0)(2,1,0) vs Actual for M1")
legend("bottomright",c("Forecast","Actual"),cex=0.8,col=c("black","red"), lty=1, lwd=c(2.5,2.5))


#### VAR ####

# Vamos a utilizar algunas de las variables del ejercicio SARIMAX,
# en particular las que tienen un mayor grado de exogeneidad según la teoría.
# Eliminamos al déficit fiscal 

# CERCANIA A ELECCIONES 
# Ciclo político genera presiones a aumentar el gasto sin aumentar los impuestos
# Fuente: elaboracion propia
elect


# PRECIO INTERNACIONAL DE LA SOJA
# Determinante de la recaudación impositiva


length(dsoja)
# debemos eliminar el primer elemento de elect para que la matriz sea regular
delect=elect[2:length(elect)]
length(delect)

##### VAR CON ELECCIONES #####
# Armamos la matriz 
vary_elect=cbind(delect, serie)
vary_elect_df=data.frame(vary_elect)

vary_elect_is=vary_elect_df[1:156,]
vary_elect_oos=vary_elect_df[157:196,]


VARselect(vary_elect_is, lag.max = 12, type = "const", season = 12)
# MIN AIC se da para 1 lags
var_elect= VAR(vary_elect_oos, p = 1, type = "const", season = 12)
summary(var_elect)
# NO HAY VALORES SIGNIFICATIVOS
serial.test(var_elect, lags.pt = 12)
# La estacionalidad captura la correlacion serial


##### VAR CON SOJA #####
# Armamos la matriz 
vary_soja=cbind(dsoja, serie)
vary_soja_df=data.frame(vary_soja)

vary_soja_is=vary_soja_df[1:156,]
vary_soja_oos=vary_soja_df[157:196,]


VARselect(vary_soja_is, lag.max = 12, type = "const", season = 12)
# MIN AIC se da para 1 lags
var_soja= VAR(vary_soja_oos, p = 1, type = "const", season = 12)
summary(var_soja)
# NO HAY COEFICIENTES SIGNIFICATIVOS

serial.test(var_soja, lags.pt = 12)
# esta estructura no captura toda la varianza


##### VAR CON BACHE FISCAL #####
# Armamos la matriz 
vary_bache=cbind(dbache, serie)
vary_bache_df=data.frame(vary_bache)

vary_bache_is=vary_bache_df[1:156,]
vary_bache_oos=vary_bache_df[157:196,]


VARselect(vary_bache_is, lag.max = 12, type = "const")
# MIN AIC se da para 12 lags
var_bache= VAR(vary_bache_oos, p = 12, type = "const")
summary(var_bache)
# NO HAY COEFICIENTES SIGNIFICATIVOS

serial.test(var_bache, lags.pt = 12)
# esta estructura no captura toda la varianza por los efectos estacionales


##### VAR CON EMAE #####
# Armamos la matriz 
demae=emae[2:197]
vary_emae=cbind(demae, serie)
vary_emae_df=data.frame(vary_emae)

vary_emae_is=vary_emae_df[1:156,]
vary_emae_oos=vary_emae_df[157:196,]


VARselect(vary_emae_is, lag.max = 12, type = "const", season = 12)
# MIN AIC se da para 2 lags
var_emae= VAR(vary_emae_oos, p = 2, type = "const", season = 12)
summary(var_emae)
# NO HAY COEFICIENTES SIGNIFICATIVOS

serial.test(var_emae, lags.pt = 12)
# esta estructura no captura toda la correlacion serial 



##### VAR CON BADLAR #####
# Armamos la matriz 
badlar = read.csv("D:/RStudio/CodigoR/SeriesTiempo/TPFinal/Data/tasabadlar.csv")
badlar= ts(log(badlar[,2]), frequency=12, start=c(2005,1))

summary(ur.df(badlar, type = 'drift', selectlags = 'BIC'))
# ENCONTRAMOS QUE ES ESTACIONARIA DE ORDEN 0


dbadlar=badlar[2:197]
vary_badlar=cbind(dbadlar, serie)
vary_badlar_df=data.frame(vary_badlar)

vary_badlar_is=vary_badlar_df[1:156,]
vary_badlar_oos=vary_badlar_df[157:196,]


VARselect(vary_badlar_is, lag.max = 12, type = "const", season = 12)
# MIN AIC se da para 6 lags
var_badlar= VAR(vary_badlar_oos, p = 6, type = "const", season = 12)
summary(var_badlar)
# Hay dos ceoficientes significativos al 10%, con dos y tres meses de retraso

serial.test(var_badlar, lags.pt = 12)



##### VAR MULTIVARIADO #####
# Armamos la matriz 
vary_multivar=cbind(delect, serie, dbadlar)
vary_multivar_df=data.frame(vary_multivar)

vary_multivar_is=vary_multivar_df[1:156,]
vary_multivar_oos=vary_multivar_df[157:196,]


VARselect(vary_multivar_is, lag.max = 12, type = "const", season = 12)
# MIN AIC se da para 2 lags
var_multivar= VAR(vary_multivar_oos, p = 2, type = "const", season = 12)
summary(var_multivar)
# NO HAY COEFICIENTES SIGNIFICATIVOS

serial.test(var_multivar, lags.pt = 12)
# ENCONTRAMOS QUE 


#### VEC MODEL ####
serial.test()