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

# Estimación del Sesgo, Varianza y Error Cuadrático Medio
for (j in 1:length(n)) {
  
  muestra <- sample(Pob, n[j])
  
  for (i in 1:Nsim) {
    
    Res.Es1[i] <- Est1(muestra)
    Res.Es2[i] <- Est2(muestra)
    Res.Es3[i] <- Est3(muestra)
    Res.Es4[i] <- Est4(muestra)
  }
  
  for (i in 1:NsimECM) {
    
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


#Gráfica del comportamiento del sesgo:
x11()
plot(n, Sesgo_theta_est[, 1], type = "b", ylim = c(min(Sesgo_theta_est) - 10, max(Sesgo_theta_est) + 
                                               10), pch = 19, ylab = expression(hat(theta)), main = "Comportamiento del sesgo")
lines(n, Sesgo_theta_est[,2], type = "b", col = 2, pch = 19)
lines(n, Sesgo_theta_est[,3], type = "b", col = 3, pch = 19)
lines(n, Sesgo_theta_est[,4], type = "b", col = 4, pch = 19)
legend(x=220,y=200, c(expression(hat(theta[1])), expression(hat(theta[2])),expression(hat(theta[3])),expression(hat(theta[4]))), 
       col = c(1, 2, 3,4), lwd = c(2, 2))
abline(h = 0, lty = 2, lwd = 2, col = 2)

#Gráfica de la varianza de los estimadores
x11()
plot(n, var_theta_est[, 1], type = "b", ylim = c(min(var_theta_est) - 10, max(var_theta_est) + 
                                                     10), pch = 19, ylab = expression(hat(theta)), main = "Comportamiento de la varianza")
lines(n, var_theta_est[,2], type = "b", col = 2, pch = 19)
lines(n, var_theta_est[,3], type = "b", col = 3, pch = 19)
lines(n, var_theta_est[,4], type = "b", col = 4, pch = 19)
legend(x=220,y=10000, c(expression(hat(theta[1])), expression(hat(theta[2])),expression(hat(theta[3])),expression(hat(theta[4]))), 
       col = c(1, 2, 3,4), lwd = c(2, 2))
abline(h = 0, lty = 2, lwd = 2, col = 2)

#Gráfica del resultado de la estimación
x11()
plot(n, theta_est[, 1], type = "b", ylim = c(min(theta_est) - 10, max(theta_est) + 
                                                   10), pch = 19, ylab = expression(hat(theta)), main = "Resultado de la estimación")
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
# Punto 4
# ------------------------------------------------------- #



# ------------------------------------------------------- #
# Punto 5
# ------------------------------------------------------- #


