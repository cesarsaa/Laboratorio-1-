# Laboratorio 1 Metodos Estadisticos 
# Kevin Chica - Diana Arias - Cesar Saavedra 


# ------------------------------------------------------- #
# Punto 1
# ------------------------------------------------------- #



# ------------------------------------------------------- #
# Punto 4
# ------------------------------------------------------- #


# ------------------------------------------------------- #
# Punto 5
# ------------------------------------------------------- #

# ------------------------------------------------------- #
# 
# ------------------------------------------------------- #
N <- 5000
n <- 5
Nsim <- 100
set.seed(84)
a <- 3
b <- 5
Pob <- runif(N, a, b)

#Se extraen muestra de n=5 y se repite el proceso 100 veces
Muestra <- matrix(sample(Pob, n*Nsim), ncol = n) 
head(Muestra)

#Estimador 1: Promedio arítmetico
T1 <- function(datos){
  mean(datos)
}

#Estimador 2: (Máximo+Mínimo-1)
T2 <- function(datos){
  max(datos) + min(datos) - 1
}

