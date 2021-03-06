# Laboratorio 1 
# Metodos Estadisticos 
# Johann Alexis Ospina
# Diana Arias - 1528008
# Kevin Garc�a - 1533173
# Cesar Saavedra - 1628466

# ------------------------------------------------------- #
# Punto 1
#Estimadores planteados:
#Estimador 1:
Est1 <- function(datos) {
  Resul <- (2 * mean(datos) - 1)
  return(Resul)
}

# Estimador 2
Est2 <- function(datos) {
  Resul <- max(datos) + min(datos) - 1
  return(Resul)
}

# Estimador 3
Est3 <- function(datos) {
  Resul <- max(datos)
  return(Resul)
}

# Estimador 4
Est4 <- function(datos) {
  Resul <- median(datos)
  return(Resul)
}

n <- c(20, 30, 40, 50,100, 250)
theta <- 1200
Nsim <- 1000
NsimECM <- 1000
Pob <- seq(1, theta, 1)
Res.Es1 <- numeric(Nsim)
Res.Es2 <- numeric(Nsim)
Res.Es3 <- numeric(Nsim)
Res.Es4 <- numeric(Nsim)
ECM.Es1 <- numeric(length(n))
ECM.Es2 <- numeric(length(n))
ECM.Es3 <- numeric(length(n))
ECM.Es4 <- numeric(length(n))
Sesgo.Es1 <- numeric(length(n))
Sesgo.Es2 <- numeric(length(n))
Sesgo.Es3 <- numeric(length(n))
Sesgo.Es4 <- numeric(length(n))
theta_est <- matrix(NA, nrow = 6, ncol = 4)
var_theta_est <- matrix(NA, nrow = 6, ncol = 4)
ECM_theta_est <- matrix(NA, nrow = 6, ncol = 4)
Sesgo_theta_est <- matrix(NA, nrow = 6, ncol = 4)

# Estimaci?n del Sesgo, Varianza y Error Cuadr?tico Medio
for (j in 1:length(n)) {
  
  muestra <- sample(Pob, n[j])
  
  for (i in 1:Nsim) {
    muestra <- sample(Pob, n[j])
    Res.Es1[i] <- Est1(muestra)
    Res.Es2[i] <- Est2(muestra)
    Res.Es3[i] <- Est3(muestra)
    Res.Es4[i] <- Est4(muestra)
  }
  
  ECM.Es1[j] <- sum((theta - Res.Es1)^2)/Nsim
  ECM.Es2[j] <- sum((theta - Res.Es2)^2)/Nsim
  ECM.Es3[j] <- sum((theta - Res.Es3)^2)/Nsim
  ECM.Es4[j] <- sum((theta - Res.Es4)^2)/Nsim
  
  Sesgo.Es1[j] <- (theta - mean(Res.Es1))
  Sesgo.Es2[j] <- (theta - mean(Res.Es2))
  Sesgo.Es3[j] <- (theta - mean(Res.Es3))
  Sesgo.Es4[j] <- (theta - mean(Res.Es4))
  
  theta_est[j, ] <- c(mean(Res.Es1), mean(Res.Es2),mean(Res.Es3),mean(Res.Es4))
  var_theta_est[j, ] <- c(var(Res.Es1), var(Res.Es2),var(Res.Es3),var(Res.Es4))
  ECM_theta_est[j, ] <- c(ECM.Es1[j], ECM.Es2[j], ECM.Es3[j], ECM.Es4[j])
  Sesgo_theta_est[j, ] <- c(Sesgo.Es1[j], Sesgo.Es2[j],Sesgo.Es3[j],Sesgo.Es4[j])
}



theta_est <- as.data.frame(theta_est)
var_theta_est <- as.data.frame(var_theta_est)
ECM_theta_est <- as.data.frame(ECM_theta_est)
Sesgo_theta_est <- as.data.frame(Sesgo_theta_est)


#Gr?fica del comportamiento del sesgo:
x11()
plot(n, Sesgo_theta_est[, 1], type = "b", ylim = c(min(Sesgo_theta_est) - 10, max(Sesgo_theta_est) + 
                                               10), pch = 19, ylab = expression(hat(theta)), main = "Comportamiento del sesgo")
lines(n, Sesgo_theta_est[,2], type = "b", col = 2, pch = 19)
lines(n, Sesgo_theta_est[,3], type = "b", col = 3, pch = 19)
lines(n, Sesgo_theta_est[,4], type = "b", col = 4, pch = 19)
legend(x=220,y=200, c(expression(hat(theta[1])), expression(hat(theta[2])),expression(hat(theta[3])),expression(hat(theta[4]))), 
       col = c(1, 2, 3,4), lwd = c(2, 2))
abline(h = 0, lty = 2, lwd = 2, col = 2)

#Gr?fica de la varianza de los estimadores
x11()
plot(n, var_theta_est[, 1], type = "b", ylim = c(min(var_theta_est) - 10, max(var_theta_est) + 
                                                     10), pch = 19, ylab = expression(hat(theta)), main = "Comportamiento de la varianza")
lines(n, var_theta_est[,2], type = "b", col = 2, pch = 19)
lines(n, var_theta_est[,3], type = "b", col = 3, pch = 19)
lines(n, var_theta_est[,4], type = "b", col = 4, pch = 19)
legend(x=220,y=10000, c(expression(hat(theta[1])), expression(hat(theta[2])),expression(hat(theta[3])),expression(hat(theta[4]))), 
       col = c(1, 2, 3,4), lwd = c(2, 2))
abline(h = 0, lty = 2, lwd = 2, col = 2)

#Gr?fica del resultado de la estimaci?n
x11()
plot(n, theta_est[, 1], type = "b", ylim = c(min(theta_est) - 10, max(theta_est) + 
                                                   10), pch = 19, ylab = expression(hat(theta)), main = "Resultado de la estimaci?n")
lines(n, theta_est[,2], type = "b", col = 2, pch = 19)
lines(n, theta_est[,3], type = "b", col = 3, pch = 19)
lines(n, theta_est[,4], type = "b", col = 4, pch = 19)
legend(x=220,y=1000, c(expression(hat(theta[1])), expression(hat(theta[2])),expression(hat(theta[3])),expression(hat(theta[4]))), 
       col = c(1, 2, 3,4), lwd = c(2, 2))
abline(h = 0, lty = 2, lwd = 2, col = 2)

#Comportamiento del Error Cuadratico medio
x11()
plot(n, ECM_theta_est[, 1], type = "b", ylim = c(min(ECM_theta_est) - 10, max(ECM_theta_est) + 
                                               10), pch = 19, ylab = expression(hat(theta)), main = "Comportamiento del ECM")
lines(n, ECM_theta_est[,2], type = "b", col = 2, pch = 19)
lines(n, ECM_theta_est[,3], type = "b", col = 3, pch = 19)
lines(n, ECM_theta_est[,4], type = "b", col = 4, pch = 19)
legend(x=220,y=300000, c(expression(hat(theta[1])), expression(hat(theta[2])),expression(hat(theta[3])),expression(hat(theta[4]))), 
       col = c(1, 2, 3,4), lwd = c(2, 2))
abline(h = 0, lty = 2, lwd = 2, col = 2)
# ------------------------------------------------------- #

# ------------------------------------------------------- #
# Punto 4.a.
# X~Exponencial(Lambda=5)
n=500
x <- rexp(n,5)
media1 <- mean(x)
sd1 <- sd(x)

mediaMuestral=function(n){
  muestra=rexp(n,5)
  media=mean(muestra)
  return(media)
}

m=1000
muchasMedias=replicate(m,mediaMuestral(n))
mean(muchasMedias)
sd(muchasMedias)

para5 <- muchasMedias[1:5]
para10 <- muchasMedias[1:10]
para20 <- muchasMedias[1:20]
para30 <- muchasMedias[1:30]
para50 <- muchasMedias[1:50]
para100 <- muchasMedias[1:100]

h <- hist(muchasMedias,xlab="Media muestral", ylab="Frecuencia", col="azure3",
     xlim=c(0.17,0.23),freq=T,
     main="Histograma de las medias muestrales observadas en 1000 muestras de tamaño 500 para una funcion Exponencial de Lambda = 5")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(muchasMedias), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(muchasMedias)
lines (xfit, yfit, col = "red", lwd = 2)

par(mfrow=c(3,3))
hist(x, main="Histograma funcion exponencial con Lambda = 5", col="azure3")
hist(para5, main="Medias observadas en 1000 muestras de tamaño 5", xlab="Media muestral", ylab="Frecuencia", col="azure3")
hist(para10, main="Medias observadas en 1000 muestras de tamaño 10", xlab="Media muestral", ylab="Frecuencia", col="azure3")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para10), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para10)
lines (xfit, yfit, col = "red", lwd = 2)
hist(para20, main="Medias observadas en 1000 muestras de tamaño 20", xlab="Media muestral", ylab="Frecuencia", col="azure3")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para20), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para20)
lines (xfit, yfit, col = "red", lwd = 2)
hist(para30, main="Medias observadas en 1000 muestras de tamaño 30", xlab="Media muestral", ylab="Frecuencia", col="azure3")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para30), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para30)
lines (xfit, yfit, col = "red", lwd = 2)
hist(para50, main="Medias observadas en 1000 muestras de tamaño 50", xlab="Media muestral", ylab="Frecuencia", col="azure3")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para50), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para50)
lines (xfit, yfit, col = "red", lwd = 2)
hist(para100, main="Medias observadas en 1000 muestras de tamaño 100", xlab="Media muestral", ylab="Frecuencia", col="azure3")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para100), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para100)
lines (xfit, yfit, col = "red", lwd = 2)

# Punto 4.b.
## t-Student, grados de libertad = 7 
n2=500
y <- rt(n2,7)
media2 <- mean(y)
sd2 <- sd(y)
hist(y)

mediaMuestral2=function(n2){
  muestra=rt(n2,7)
  media=mean(muestra)
  return(media)
}

m2=1000
muchasMedias2=replicate(m2,mediaMuestral2(n2))
mean(muchasMedias2)
sd(muchasMedias2)

tpara5 <- muchasMedias2[1:5]
tpara10 <- muchasMedias2[1:10]
tpara20 <- muchasMedias2[1:20]
tpara30 <- muchasMedias2[1:30]
tpara50 <- muchasMedias2[1:50]
tpara100 <- muchasMedias2[1:100]

t <- hist(muchasMedias2,xlab="Media muestral", ylab="Frecuencia", col="slategray1",freq=T,
          main="Histograma de las medias muestrales observadas en 1000 muestras de tamaño 500 para una funcion t-Student con k=7")
xfit <- seq(min(y), max(y), length = 1000)
yfit <- dnorm(xfit, mean=mean(muchasMedias2), sd(muchasMedias2))
yfit <- yfit * diff(t$mids[1:2]) * length(muchasMedias2)
lines (xfit, yfit, col = "red", lwd = 2)

par(mfrow=c(3,3))
hist(y, main="Histograma funcion t-Student con 7 grados de libertad",col="azure2")
hist(tpara5, main="Medias observadas en 1000 muestras de tamaño 5",xlab="Media muestral", ylab="Frecuencia", col="azure2")
hist(tpara10, main="Medias observadas en 1000 muestras de tamaño 10",xlab="Media muestral", ylab="Frecuencia", col="azure2")
xfit <- seq(min(y), max(y), length = 1000)
yfit <- dnorm(xfit, mean(tpara10), sd(muchasMedias2))
yfit <- yfit * diff(t$mids[1:2]) * length(tpara10)
lines (xfit, yfit, col = "red", lwd = 2)
hist(tpara20, main="Medias observadas en 1000 muestras de tamaño 20",xlab="Media muestral", ylab="Frecuencia", col="azure2")
xfit <- seq(min(y), max(y), length = 1000)
yfit <- dnorm(xfit, mean(tpara20), sd(muchasMedias2))
yfit <- yfit * diff(t$mids[1:2]) * length(tpara20)
lines (xfit, yfit, col = "red", lwd = 2)
hist(tpara30, main="Medias observadas en 1000 muestras de tamaño 30",xlab="Media muestral", ylab="Frecuencia", col="azure2")
xfit <- seq(min(y), max(y), length = 1000)
yfit <- dnorm(xfit, mean(tpara30), sd(muchasMedias2))
yfit <- yfit * diff(h$mids[1:2]) * length(tpara30)
lines (xfit, yfit, col = "red", lwd = 2)
hist(tpara50, main="Medias observadas en 1000 muestras de tamaño 50",xlab="Media muestral", ylab="Frecuencia", col="azure2")
xfit <- seq(min(y), max(y), length = 1000)
yfit <- dnorm(xfit, mean(tpara50), sd(muchasMedias2))
yfit <- yfit * diff(t$mids[1:2]) * length(tpara50)
lines (xfit, yfit, col = "red", lwd = 2)
hist(tpara100, main="Medias observadas en 1000 muestras de tamaño 100",xlab="Media muestral", ylab="Frecuencia", col="azure2")
xfit <- seq(min(y), max(y), length = 1000)
yfit <- dnorm(xfit, mean(tpara100), sd(muchasMedias2))
yfit <- yfit * diff(t$mids[1:2]) * length(tpara100)
lines (xfit, yfit, col = "red", lwd = 2)

# ------------------------------------------------------- #

# ------------------------------------------------------- #
# Punto 5.C.
m <-c(3700, 3600, 3500, 3400, 3300, 3200, 3100, 3000, 2900,2800,2700,2600,2500)
xbarra=3334  #Media de decision
sigma=400  #Desviacion de x
k=4   #tamaño de muestra
Proba=numeric(length(m))
for (i in 1:length(m)){
  prob=pnorm((xbarra-m[i])/(sigma/sqrt(k)))
  Proba[i]=prob
}

x11()
plot(m, Proba, type = "b",pch=19, ylab="Probabilidad", main="Grafica de probabilidades para distintos valores de m", col="black", lty=3, lwd=2) 

#Punto 5.D.
m2 <-c(3700, 3600, 3500, 3400, 3300, 3200, 3100, 3000, 2900,2800,2700,2600,2500)
xbarra=3334  #Media de decision
sigma=400  #Desviacion de x
k2=5   #tamaño de muestra
Proba2=numeric(length(m2))
for (i in 1:length(m2)){
  prob2=pnorm((xbarra-m2[i])/(sigma/sqrt(k2)))
  Proba2[i]=prob2
}

m3 <-c(3700, 3600, 3500, 3400, 3300, 3200, 3100, 3000, 2900,2800,2700,2600,2500)
xbarra=3334  #Media de decision
sigma=400  #Desviacion de x
k3=6   #tamaño de muestra
Proba3=numeric(length(m3))
for (i in 1:length(m3)){
  prob3=pnorm((xbarra-m3[i])/(sigma/sqrt(k3)))
  Proba3[i]=prob3
}

par(mfrow=c(1,2))
plot(m2, Proba, type = "b",pch=19, ylab="Probabilidad", main="Grafica de probabilidades para distintos valores de m con k=5", col="blue", lty=3, lwd=2) 
plot(m3, Proba, type = "b",pch=19, ylab="Probabilidad", main="Grafica de probabilidades para distintos valores de m con k=6", col="red", lty=3, lwd=2) 

# ------------------------------------------------------- #


