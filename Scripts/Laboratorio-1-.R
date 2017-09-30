# Laboratorio 1 
# Metodos Estadisticos 
# Kevin Chica - Diana Arias - Cesar Saavedra 


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
x <- rexp(500,5)
media1 <- mean(x)
sd1 <- sd(x)
hist(x)

mediaMuestral=function(n){
  muestra=rexp(500,5)
  media=mean(muestra)
  return(media)
}

mediaMuestral(5)
mediaMuestral(10)
mediaMuestral(20)
mediaMuestral(30)
mediaMuestral(50)
mediaMuestral(100)

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

h <- hist(muchasMedias,xlab="Media muestral", ylab="Frecuencia", col="lightcyan",
     xlim=c(0.17,0.23),freq=T,
     main="Histograma de las medias muestrales observadas en 1000 muestras de tamaño 500 para una funcion Exponencial de Lambda = 5")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(muchasMedias), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(muchasMedias)
lines (xfit, yfit, col = "red", lwd = 2)

par(mfrow=c(3,3))
hist(x, main="Histograma funcion exponencial con Lambda = 5" )
hist(para5, main="Medias observadas en 1000 muestras de tamaño 5")
hist(para10, main="Medias observadas en 1000 muestras de tamaño 10")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para10), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para10)
lines (xfit, yfit, col = "red", lwd = 2)
hist(para20, main="Medias observadas en 1000 muestras de tamaño 20")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para20), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para20)
lines (xfit, yfit, col = "red", lwd = 2)
hist(para30, main="Medias observadas en 1000 muestras de tamaño 30")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para30), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para30)
lines (xfit, yfit, col = "red", lwd = 2)
hist(para50, main="Medias observadas en 1000 muestras de tamaño 50")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para50), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para50)
lines (xfit, yfit, col = "red", lwd = 2)
hist(para100, main="Medias observadas en 1000 muestras de tamaño 100")
xfit <- seq(min(x), max(x), length = 1000)
yfit <- dnorm(xfit, mean(para100), sd = (media1)/(sqrt(500)))
yfit <- yfit * diff(h$mids[1:2]) * length(para100)
lines (xfit, yfit, col = "red", lwd = 2)


# ------------------------------------------------------- #
#4.a
options(max.print = 99999999)
numeroMuestras=1000
poblacion <- rexp(1000,5)

#Para n=5
n1=5
Mat.prom1 <- matrix(0, numeroMuestras, n1)
for (i in 1:numeroMuestras) {
  Mat.prom1[i, ] <- sample(poblacion, n1)
}
Dis.prom1 <- apply(Mat.prom1, 1, mean)

#Para n=10
n2=10
Mat.prom2 <- matrix(0, numeroMuestras, n2)
for (i in 1:numeroMuestras) {
  Mat.prom2[i, ] <- sample(poblacion, n2)
}
Dis.prom2 <- apply(Mat.prom2, 1, mean)

#Para n=20
n3=20
Mat.prom3 <- matrix(0, numeroMuestras, n3)
for (i in 1:numeroMuestras) {
  Mat.prom3[i, ] <- sample(poblacion, n3)
}
Dis.prom3 <- apply(Mat.prom3, 1, mean)

#Para n=30
n4=30
Mat.prom4 <- matrix(0, numeroMuestras, n4)
for (i in 1:numeroMuestras) {
  Mat.prom4[i, ] <- sample(poblacion, n4)
}
Dis.prom4 <- apply(Mat.prom4, 1, mean)

#Para n=50
n5=50
Mat.prom5 <- matrix(0, numeroMuestras, n5)
for (i in 1:numeroMuestras) {
  Mat.prom5[i, ] <- sample(poblacion, n5)
}
Dis.prom5 <- apply(Mat.prom5, 1, mean)

#Para n=100
n6=100
Mat.prom6 <- matrix(0, numeroMuestras, n6)
for (i in 1:numeroMuestras) {
  Mat.prom6[i, ] <- sample(poblacion, n6)
}
Dis.prom6 <- apply(Mat.prom6, 1, mean)

#Gr?ficos:
mat <- matrix(c(1,1,1,2,3,4,5,6,7),ncol = 3,byrow = T)
x11()
layout(mat)
hist(poblacion, xlab = "X", ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de frecuencias de una poblaci?n exponencial con landa=5",freq = FALSE)
hist(Dis.prom1, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=5",freq = FALSE)
hist(Dis.prom2, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=10",freq = FALSE)
hist(Dis.prom3, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=20",freq = FALSE)
hist(Dis.prom4, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=30",freq = FALSE)
hist(Dis.prom5, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=50",freq = FALSE)
hist(Dis.prom6, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=100",freq = FALSE)


#4.b
poblacionT <- rt(1000,5)
#Para n=5
Mat.pro1 <- matrix(0, numeroMuestras, n1)
for (i in 1:numeroMuestras) {
  Mat.pro1[i, ] <- sample(poblacionT, n1)
}
Dis.pro1 <- apply(Mat.pro1, 1, mean)

#Para n=10
Mat.pro2 <- matrix(0, numeroMuestras, n2)
for (i in 1:numeroMuestras) {
  Mat.pro2[i, ] <- sample(poblacionT, n2)
}
Dis.pro2 <- apply(Mat.pro2, 1, mean)

#Para n=20
Mat.pro3 <- matrix(0, numeroMuestras, n3)
for (i in 1:numeroMuestras) {
  Mat.pro3[i, ] <- sample(poblacionT, n3)
}
Dis.pro3 <- apply(Mat.pro3, 1, mean)

#Para n=30
Mat.pro4 <- matrix(0, numeroMuestras, n4)
for (i in 1:numeroMuestras) {
  Mat.pro4[i, ] <- sample(poblacionT, n4)
}
Dis.pro4 <- apply(Mat.pro4, 1, mean)

#Para n=50
Mat.pro5 <- matrix(0, numeroMuestras, n5)
for (i in 1:numeroMuestras) {
  Mat.pro5[i, ] <- sample(poblacionT, n5)
}
Dis.pro5 <- apply(Mat.pro5, 1, mean)

#Para n=100
Mat.pro6 <- matrix(0, numeroMuestras, n6)
for (i in 1:numeroMuestras) {
  Mat.pro6[i, ] <- sample(poblacionT, n6)
}
Dis.pro6 <- apply(Mat.pro6, 1, mean)

#Gr?ficos:
x11()
layout(mat)
hist(poblacion, xlab = "X", ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de frecuencias de una poblaci?n T de student con k=7",freq = FALSE)
hist(Dis.pro1, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=5",freq = FALSE)
hist(Dis.pro2, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=10",freq = FALSE)
hist(Dis.pro3, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=20",freq = FALSE)
hist(Dis.pro4, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=30",freq = FALSE)
hist(Dis.pro5, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=50",freq = FALSE)
hist(Dis.pro6, xlab = expression(bar(X)), ylab = "Frecuencia", col = "grey60", 
     main = "Histograma de la distribuci?n del promedio para n=100",freq = FALSE)



# ------------------------------------------------------- #
# Punto 5
m <-c(3700, 3600, 3500, 3400, 3300, 3200, 3100, 3000, 2900,2800,2700,2600,2500)
xbarra=3334  #Media de decisi?n
sigma=400  #Desviaci?n de x
k=4   #tama?o de muestra
Proba=numeric(length(m))
for (i in 1:length(m)){
  prob=pnorm((xbarra-m[i])/(sigma/sqrt(k)))
  Proba[i]=prob
}

x11()
plot(m, Proba, type = "b",pch = 19, ylab = "Probabilidad", main = "Gr?fica de probabilidades para distintos valores de m") 


# ------------------------------------------------------- #


