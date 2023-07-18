########################################333
##### punto de boost straping 
###########################################
### separar las poblaciones
##############################################################
################### Segmentar por sexo
################################################
## intento primer filtrado
library(dplyr)
######### hombres
prey1 <- DATA %>% 
      filter(SEXO_VICTIMA == "MASCULINO")
y1 <- prey1$TOTAL_VICTIMAS
######### mujeres
prey2 <- DATA %>% 
      filter(SEXO_VICTIMA == "FEMENINO")
y2 <- prey2$TOTAL_VICTIMAS
################################################################
'''
Para el bootstrap parámetrico asumimos que los datos provienen de una 
población conocida que por el contexto del problema es una distribución 
poisson al cual sabemos que el estimador de maxima veorismilitud para 
lambda es la media muestral
'''
#############################################################
### conociendo la longitud de las poblaciones
n1 <- length(y1)
n2 <- length(y2)
#########################################################3
### Conociendo el EMV para cada población
mean_1 <- mean(y1)
mean_2 <- mean(y2)
'''
En el texto sugerido nos dicen que fijemos N en un intervalo entre 1000 y 
2000 así fijamos N en 1900
'''
############################################################
## fijando el N
N <- 1900
'''
ahora entonces obtendremos los 1900 theta de cada población mediante 
bootstrap parámetrico
'''
##########################################################
## Calculamos la estimación de eta la estadistica
###### para los hombres
set.seed(1979)
eta <- NULL
for (i in 1:N) {
      #### remmuestreo
      remuestra_1 <- rpois(n=n1,lambda = mean_1)
      remuestra_2 <- rpois(n=n2,lambda = mean_2)
      ##### almacenar EMV de la remuestra
      eta[i] <- ((mean(remuestra_2))/mean(remuestra_1)) - 1
}
##############################################################
mean(eta)
sqrt(var(eta))/mean(eta)
### intervalo de confianza
ic <- quantile(eta,probs = c(0.025,0.975))
ic
##################################################
out <- cbind(mean(eta),sqrt(var(eta))/mean(eta),ic[1],ic[2])
colnames(out) <- c("Media","cv","Q2.5%","Q97.5%")
########################
library(knitr)
########
knitr::kable(x= out,digits = 3,aling = "c")
################################################################3
### visualización
plot(NA, NA, xlim = c(-0.05,0.7), ylim = c(0,4), 
     xlab = expression(eta),
     ylab = expression(paste("p","(",eta," | ",y,")",sep="")), 
     main = "Posterior ")
# posterior para eta
lines(density(eta), col = 3, lwd = 2)
### intervalo de confianza
abline(v = quantile(x = eta, probs = c(0.025, 0.975)), lty = 2,
       lwd = 1.5, col = 1)
### media para los hombres
abline(v = mean(eta),lty = 4 ,lwd = 1, col = 2)
## poner el cuadro legendario 1
legend(locator(1),legend = c("Posterior","Media",
                             "IC 95%"),bty = "n",
       col= c(3,2,1),lty=c(1,1,4),lwd=c(2,2,2))
################################################################
#################################################################
################################################################
#### simular 100000 muestras para escenarios diferentes
'''
se tiene  4 escenarios diferenes donde para cada población coincide 
el tamaño muestral en cada escenario y para todos los escenarios los
parámetros coinciden siendo la media muestral de ambas poblaciones
por lo tanto simularesmos 100000 muestras para cada escenario
'''
#############################################################
'''
ajustaremos el modelo de manera bayesiana para cada muestra así
con la posterior de cada muestra tendremos un theta1 y un theta 2
con los cuales calcularemos eta 
'''
#######################################################33
## ajustando modelo para posterior de manera bayesiana 
a <- 0.01 # usaremos este porque son iguales los hiperparámetros
## usaremos monte carlo con B=20000 por cada muestra para tener 
B<- 20000
'''
Se requiere saber cuantos de los intervalos contienen la verdadero valor
el cual esta dado por las medias de las poblaciones de los datos dados
así hacemos lo siguiente
'''
##################################################
### valor real
eta_1 <- round((mean_2-mean_1)/mean_1,3)
#############################################################
##############################################################
## escenario 1
###############################################################
n_1 <- 10 # usaremos uno porque los valores coinciden
######### simulando las muestras para la poblacion de hombres
Esce1_y1 <- matrix(data = NA,nrow = 10, ncol = 100000)
set.seed(1944)
for (k in 1:100000) {
      Esce1_y1[,k] <- rpois(n=n_1,lambda = mean_1)
}
######### simulando las muestras para la poblacion de hombres
Esce1_y2 <- matrix(data = NA,nrow = 10, ncol = 100000)
set.seed(1944)
for (k in 1:100000) {
      Esce1_y2[,k] <- rpois(n=n_1,lambda = mean_2)
}
################################################3
# algoritmo para verificar si el verdadero valor cae 
'''
#primero lo hacemos para uno luego miramos pa todas las muestras
theta1 <- rgamma(n=B,shape = a + sum(Esce1_y1[,1]),rate = a + n_1)
theta2 <- rgamma(n=B,shape = a + sum(Esce1_y2[,1]),rate = a + n_1)
eta <- (theta2-theta1)/theta1
li <- round(quantile(x = eta, probs = 0.025),3)
ls <- round(quantile(x = eta, probs = 0.975),3)
si<- sum((li < eta_1) == (eta_1 < ls))
conteo <- conteo + si
conteo
'''
####################################################
'''
for normal el de toda la vida 
conteo <- 0
set.seed(1944)
for (k in 1:100000) {
      theta1 <- rgamma(n=B,shape = a + sum(Esce1_y1[,k]),rate = a + n_1)
      theta2 <- rgamma(n=B,shape = a + sum(Esce1_y2[,k]),rate = a + n_1)
      eta <- (theta2-theta1)/theta1
      li <- round(quantile(x = eta, probs = 0.025),3)
      ls <- round(quantile(x = eta, probs = 0.975),3)
      conteo <- conteo + sum((li < eta_1) == (eta_1 < ls))
}
print(conteo/100000)
'''
################################################
#########################################################3
#### comprobar para la 100000 muestras con MC bayes
library(foreach)# for en paralelo
library(parallel) # para trabajar en paralelo crear el cluster
library(parallelly) # para detectar nucleos
cl <- makeCluster(availableCores(omit = 1)) # Cuantos nucleos usaré
cl # cantidad de nucleos a usar
library(iterators) #necesario para usar DoParallel
library(doParallel) # registrar la cantidad de nucleos y para de hacerlo
registerDoParallel(cl) # backend
bp1 <-  a + n_1 # hiperparámetro 
conteo <- 0 # conteo
set.seed(1944)
conteo <- foreach(k = 1:100000, .combine = "+") %dopar% {
      theta1 <- rgamma(n = B, shape = a + sum(Esce1_y1[, k]),
                       rate = bp1)
      theta2 <- rgamma(n = B, shape = a + sum(Esce1_y2[, k]),
                       rate = bp1)
      eta <- (theta2/theta1) - 1
      ic <- quantile(x=eta,p=c(0.025,0.975))
      sum((ic[1] < eta_1) & (eta_1 < ic[2]))
}
print(conteo/100000)
######################################
##########################################################
### ahora usando bootstraping
conteo2 <- 0
set.seed(1979)
conteo2 <- foreach(k = 1:100000, .combine = "+") %dopar% {
      theta1 <- mean(Esce1_y1[,k])
      theta2 <- mean(Esce1_y2[,k])
      eta <- numeric(1000) # Inicializar eta como un vector de ceros
      for (i in 1:1000) { 
            #### remmuestreo
            remuestra_1 <- rpois(n=n_1,lambda = theta1)
            remuestra_2 <- rpois(n=n_1,lambda = theta2)
            ######## obteniendo las medias
            remedia_1 <- mean(remuestra_1)
            remedia_2 <- mean(remuestra_2)
            # Condición necesaria para no tener un eta inf
            if(remedia_1 != 0){
                  ##### almacenar EMV de la remuestra
                  eta[i] <- (remedia_2/remedia_1) - 1
            }
      }
      # intervalo
      ic <- quantile(eta,p = c(0.025,0.975))
      sum((ic[1] < eta_1) & (eta_1 < ic[2]))
}
print(conteo2/100000)
################################################################
'''
conteo2 <- numeric(1)
set.seed(1979)
for(k in 1:1000){
      theta1 <- mean(Esce1_y1[,k])
      theta2 <- mean(Esce1_y2[,k])
      eta <- numeric(1000) # Inicializar eta como un vector de ceros
      for (i in 1:1000) { 
            #### remmuestreo
            remuestra_1 <- rpois(n=n_1,lambda = theta1)
            remuestra_2 <- rpois(n=n_1,lambda = theta2)
            ######## obteniendo las medias
            remedia_1 <- mean(remuestra_1)
            remedia_2 <- mean(remuestra_2)
            if(remedia_1 != 0){
            ##### almacenar EMV de la remuestra
            eta[i] <- (remedia_2/remedia_1) - 1
            }
      }
      ic <- mean(eta) + (c(-1,1)*qt(0.975,df=999))*sqrt(var(eta)/1000)
      conteo2 <- conteo2 + sum((ic[1] < eta_1) & (eta_1 < ic[2]))
}
print(conteo2/1000)
'''
#############################################
#############################################################
##############################################################
## escenario 2,3,4 
###############################################################
nesce <- c(20,50,100) # usaremos para crear los escenarios
######### simulando las muestras para la poblacion de hombre
## donde almacenar las muestras
for (i in 1:3) {
      # para los niños
      assign(paste0("Esce", i + 1,"_y1",sep=""),
             matrix(data = NA,nrow =nesce[i],ncol = 100000))
      # para las niñas
      assign(paste0("Esce", i + 1,"_y2",sep=""),
             matrix(data = NA,nrow =nesce[i],ncol = 100000))
}
######## simulando las muestras para la poblacion de hombres
set.seed(1944)
for(k in 1:100000){
      Esce2_y1[,k] <- rpois(n=nesce[1],lambda = mean_1)
      Esce3_y1[,k] <- rpois(n=nesce[2],lambda = mean_1)
      Esce4_y1[,k] <- rpois(n=nesce[3],lambda = mean_1)
}
######## simulando las muestras para la poblacion de mujeres
set.seed(1944)
for(k in 1:100000){
      Esce2_y2[,k] <- rpois(n=nesce[1],lambda = mean_2)
      Esce3_y2[,k] <- rpois(n=nesce[2],lambda = mean_2)
      Esce4_y2[,k] <- rpois(n=nesce[3],lambda = mean_2)
}
################################################
########## escenario 2
#########################################################3
#### comprobar para la 100000 muestras con MC bayes
set.seed(1944)
conteo <- 0 # conteo
bp2 <- a + nesce[1] # hiperparámetro
conteo <- foreach(k = 1:100000, .combine = "+") %dopar% {
      theta1 <- rgamma(n = B, shape = a + sum(Esce2_y1[, k]),
                       rate = bp2)
      theta2 <- rgamma(n = B, shape = a + sum(Esce2_y2[, k]),
                       rate = bp2)
      eta <- (theta2/theta1) - 1
      ic <- quantile(x=eta,p=c(0.025,0.975))
      sum((ic[1] < eta_1) & (eta_1 < ic[2]))
}
print(conteo/100000)
##############################
##########################################################
### ahora usando bootstraping
conteo2 <- 0
set.seed(1979)
conteo2 <- foreach(k = 1:100000, .combine = "+") %dopar% {
      theta1 <- mean(Esce2_y1[,k])
      theta2 <- mean(Esce2_y2[,k])
      eta <- numeric(1000) # Inicializar eta como un vector de ceros
      for (i in 1:1000) { 
            #### remmuestreo
            remuestra_1 <- rpois(n=nesce[1],lambda = theta1)
            remuestra_2 <- rpois(n=nesce[1],lambda = theta2)
            ######## obteniendo las medias
            remedia_1 <- mean(remuestra_1)
            remedia_2 <- mean(remuestra_2)
            # Condición necesaria para no tener un eta inf
            if(remedia_1 != 0){
                  ##### almacenar EMV de la remuestra
                  eta[i] <- (remedia_2/remedia_1) - 1
            }
      }
      # usamos la estadística t pues no conocemosla varianza poblacional de 
      # eta
      ic <- quantile(eta,probs = c(0.025,0.975))
      sum((ic[1] < eta_1) & (eta_1 < ic[2]))
}
print(conteo2/100000)
################################################
########## escenario 3
#########################################################3
#### comprobar para la 100000 muestras con MC bayes
set.seed(1944)
conteo <- 0 #conteo
bp3 <- a + nesce[2] #hiperparámetro
conteo <- foreach(k = 1:100000, .combine = "+") %dopar% {
      theta1 <- rgamma(n = B, shape = a + sum(Esce3_y1[, k]),
                       rate = bp3)
      theta2 <- rgamma(n = B, shape = a + sum(Esce3_y2[, k]),
                       rate = bp3)
      eta <- (theta2/theta1) - 1
      ic <- quantile(x=eta,p=c(0.025,0.975))
      sum((ic[1] < eta_1) & (eta_1 < ic[2]))
}
print(conteo/100000)
##############################
##########################################################
### ahora usando bootstraping
conteo2 <- 0
set.seed(1979)
conteo2 <- foreach(k = 1:100000, .combine = "+") %dopar% {
      theta1 <- mean(Esce3_y1[,k])
      theta2 <- mean(Esce3_y2[,k])
      eta <- numeric(1000) # Inicializar eta como un vector de ceros
      for (i in 1:1000) { 
            #### remmuestreo
            remuestra_1 <- rpois(n=nesce[2],lambda = theta1)
            remuestra_2 <- rpois(n=nesce[2],lambda = theta2)
            ######## obteniendo las medias
            remedia_1 <- mean(remuestra_1)
            remedia_2 <- mean(remuestra_2)
            # Condición necesaria para no tener un eta inf
            if(remedia_1 != 0){
                  ##### almacenar EMV de la remuestra
                  eta[i] <- (remedia_2/remedia_1) - 1
            }
      }
      # intervalo
      ic <- quantile(eta,probs = c(0.025,0.975))
      sum((ic[1] < eta_1) & (eta_1 < ic[2]))
}
print(conteo2/100000)
################################################
########## escenario 4
#########################################################3
#### comprobar para la 100000 muestras con MC bayes
set.seed(1944)
conteo <- 0 #conteo
bp4 <- a + nesce[3]
conteo <- foreach(k = 1:100000, .combine = "+") %dopar% {
      theta1 <- rgamma(n = B, shape = a + sum(Esce4_y1[, k]),
                       rate = bp4)
      theta2 <- rgamma(n = B, shape = a + sum(Esce4_y2[, k]),
                       rate = bp4)
      eta <- (theta2/theta1) - 1
      ic <- quantile(x=eta,p=c(0.025,0.975))
      sum((ic[1] < eta_1) & (eta_1 < ic[2]))
}
print(conteo/100000)
##############################
##########################################################
### ahora usando bootstraping
conteo2 <- 0
set.seed(1979)
conteo2 <- foreach(k = 1:100000, .combine = "+") %dopar% {
      theta1 <- mean(Esce4_y1[,k])
      theta2 <- mean(Esce4_y2[,k])
      eta <- numeric(1000) # Inicializar eta como un vector de ceros
      for (i in 1:1000) { 
            #### remmuestreo
            remuestra_1 <- rpois(n=nesce[3],lambda = theta1)
            remuestra_2 <- rpois(n=nesce[3],lambda = theta2)
            ######## obteniendo las medias
            remedia_1 <- mean(remuestra_1)
            remedia_2 <- mean(remuestra_2)
            # Condición necesaria para no tener un eta inf
            if(remedia_1 != 0){
                  ##### almacenar EMV de la remuestra
                  eta[i] <- (remedia_2/remedia_1) - 1
            }
      }
       # intervalo
      ic <- quantile(eta,probs = c(0.025,0.975))
      sum((ic[1] < eta_1) & (eta_1 < ic[2]))
}
print(conteo2/100000)
##############################
stopCluster(cl) #para el trabajo en paralelo


