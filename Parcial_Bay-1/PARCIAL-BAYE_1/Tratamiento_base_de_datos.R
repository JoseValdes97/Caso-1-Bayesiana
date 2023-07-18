######### permite acceso a las varibles por nombre
#####################################################
attach(victimas)
######## conocer las variable sexo
table(SEXO_VICTIMA)
######################## Conocer las varibles necesarias 
#################### para el objetivo 
for (i in c(21,25)) {
      print(table(victimas[,i]))
}
#### Conociendo las varibles filtro
for (i in c(1,4,6:8,10,11,14,22,23)) {
      print(colnames(victimas)[i])
      print(table(victimas[,i]))
}
#### algunas salen con un vacio así miraremos con una el porque
table(ESTADO_NOTICIA)
sum(is.na(ESTADO_NOTICIA))
###### además el departamento bogotá esta codificado diferente
####### BOGOTÃ\u0081, D. C.
###### notamos que estas celdas al parcer estan vacías
########################################################
######### mi primer idea es un data frame con solo lo que usaremos así
######### preDATA tendra solo las variables que queremos
##################################################################
preDATA <- victimas[,c(21,25,1,4,6:8,10,11,14,22,23)]
#######################################
## exportar preDATA para el punto 3
##########################################
write.table(preDATA,file="preDATA.csv",sep=";",row.names = F)
###########################################################
############ mirar como la cage si este es el error
########################################################
## GRUPO_EDAD_VICTIMA la cual re quiere codificación así haremos uso 
## de for cats para recodificar y luego filtrar
library(forcats)
### Conocer la clase pues esta libreria trabaja con factores
class(preDATA$GRUPO_EDAD_VICTIMA)
## convertir a factor
preDATA$GRUPO_EDAD_VICTIMA <- factor(preDATA$GRUPO_EDAD_VICTIMA)
class(preDATA$GRUPO_EDAD_VICTIMA)
### Conocer los atributos para re nombar
attributes(preDATA$GRUPO_EDAD_VICTIMA)
####################################
##### renombrando
preDATA$GRUPO_EDAD_VICTIMA <- fct_recode(preDATA$GRUPO_EDAD_VICTIMA,
                                      "1" = "PRIMERA INFANCIA 0 - 5" ,
                                      "1" = "INFANCIA 6 - 11" ,
                                      "1" = "PRE-ADOLESCENTE 12 - 13",
                                      "1" = "ADOLESCENTE 14 - 17" )
table(preDATA$GRUPO_EDAD_VICTIMA)
################################################
## intento primer filtrado
library(dplyr)
################################################
try3 <- preDATA %>% 
      filter(HECHO == "SI",
             ESTADO_NOTICIA == "ACTIVO",
             ANIO_DENUNCIA == 2022,
             ANIO_ENTRADA == 2022,
             ANIO_HECHO == 2022,
             PAIS == "Colombia",
             DEPARTAMENTO == "BOGOTÃ\u0081, D. C.",
             GRUPO_DELITO == "DELITOS SEXUALES",
             GRUPO_EDAD_VICTIMA == "1",
             PAIS_NACIMIENTO == "Colombia")
'''
Aca notamos que podría afectar el filtrado el primer metodo que se uso 
entonces se replanteo la situación
########## primera parte
try1 <- preDATA %>% 
            filter(HECHO == "SI",
                   ESTADO_NOTICIA == "ACTIVO",
                   ANIO_DENUNCIA == 2022,
                   ANIO_ENTRADA == 2022,
                   ANIO_HECHO == 2022,
                   PAIS == "Colombia",
                   DEPARTAMENTO == "BOGOTÃ\u0081, D. C.",
                   GRUPO_DELITO == "DELITOS SEXUALES",
                   PAIS_NACIMIENTO == "Colombia")
table(try1$DEPARTAMENTO)
####### hasta este punto nos falta filtrar la variable 
## GRUPO_EDAD_VICTIMA la cual re quiere codificación así haremos uso 
## de for cats para recodificar y luego filtrar
library(forcats)
### Conocer la clase pues esta libreria trabaja con factores
class(try1$GRUPO_EDAD_VICTIMA)
## convertir a factor
try1$GRUPO_EDAD_VICTIMA <- factor(try1$GRUPO_EDAD_VICTIMA)
class(try1$GRUPO_EDAD_VICTIMA)
### Conocer los atributos para re nombar
attributes(try1$GRUPO_EDAD_VICTIMA)
##### renombrando
try1$GRUPO_EDAD_VICTIMA <- fct_recode(try1$GRUPO_EDAD_VICTIMA,
                                      "1" = "PRIMERA INFANCIA 0 - 5" ,
                                      "1" = "INFANCIA 6 - 11" ,
                                      "1" = "PRE-ADOLESCENTE 12 - 13",
                                      "1" = "ADOLESCENTE 14 - 17" )
table(try1$GRUPO_EDAD_VICTIMA)
################################################
#######  segundo filtrado
try2 <- try1 %>% 
            filter(GRUPO_EDAD_VICTIMA == "1")
pero luego notamos que daba lo mismo
sum(try2 != try3)
'''
##################################################
########## ahora tenemos la data que vamos a terminar de arreglar para
#### la primera parte
DATA <- try3[,c(1,2)]
###### conociendo como estanlas varibles de DATA
for (i in c(1,2)) {
      print(colnames(DATA)[i])
      print(table(DATA[,i]))
}
######### primero removemos los sin infromación de SEXO_VICTIMA
DATA <- DATA[DATA$SEXO_VICTIMA != "",]
########### ahora miramos el histograma de los datos
hist(DATA$TOTAL_VICTIMAS)
'''
 Donde notamos que los datos son sesgados a la derecha
 así pues basandonos en D\'Orazio (2022) usamos el paquete univOutl de R
con el método "asymmetric" el cual implementa esta formula
[Q1 ??? 2k × (Q2 ??? Q1); Q3 + 2k × (Q3 ??? Q2)] para la detección de outliers en 
estos casos
'''
##########################################3
install.packages("univOutl")
library(univOutl)
############################################
limites <- boxB(DATA$TOTAL_VICTIMAS,method='asymmetric')
limites$fences
## como vemos que el valor minimo es 1 entonces solo filtraremos por Limite
## superior por lo tanto
DATA <- DATA[DATA$TOTAL_VICTIMAS <= 9,]
for (i in c(1,2)) {
      print(colnames(DATA)[i])
      print(table(DATA[,i]))
}
#######################################
## exportar DATA por si las moscas
##########################################
write.table(DATA,file="DATA.csv",sep=";",row.names = F)
##############################################################
################### Segmentar por sexo
######### hombres
prey1 <- DATA %>% 
            filter(SEXO_VICTIMA == "MASCULINO")
y1 <- prey1$TOTAL_VICTIMAS
######### mujeres
prey2 <- DATA %>% 
      filter(SEXO_VICTIMA == "FEMENINO")
y2 <- prey2$TOTAL_VICTIMAS
#######################################################
############ distribucion previa para hombres
# hiperparametros de las previas son los mismos
a <- 0.01 
b <- 0.01
############################################################
## obteniendo número de hombres
n1 <- length(y1)
############################################################
## obteniendo número de niñas
n2 <- length(y2)
############################################################
## obteniendo la estadistica suficiente para hombres
s1 <- sum(y1)
############################################################
## obteniendo número de hombres
s2 <- sum(y2)

######################################################3
#### ajustando lo hiperparametros para la posterior de hombres
ap1 <- a + s1
bp1 <- b + n1
######################################################3
#### ajustando lo hiperparametros para la posterior de mujeres
ap2 <- a + s2
bp2 <- b + n2
###############################################
################ grafico todo junto
###########################################
#########################################
### crea la previa de ambas poblaciones
theta <- seq(0, 10, length = 10000) # puntos theta
plot(NA, NA, xlim = c(0,3), ylim = c(0,4.5), xlab = expression(theta), 
     ylab = expression(paste("p","(",theta," | ",y,")",sep="")), 
     main = "Posterior ")
### previa
lines(theta, dgamma(theta, shape = a, rate = b), col = 2, lwd = 2)
# posterior para hombres
lines(theta, dgamma(theta, shape = ap1, rate = bp1), col = 3, lwd = 2)
### intervalo de credibilidad hombres
abline(v = qgamma(c(.025,.975), shape = ap1, rate = bp1), lty = 2, 
       lwd = 1.5, col = 1)
### media para los hombres
abline(v = ap1/bp1,lty = 4 ,lwd = 1, col = 20)
# posterior para mujeres
lines(theta, dgamma(theta, shape = ap2, rate = bp2), col = 4, lwd = 2)
### intervalo de credibilidad mujeres
abline(v = qgamma(c(.025,.975), shape = ap2, rate = bp2), lty = 2, 
       lwd = 1.5, col = 14)
### media para los mujeres
abline(v = ap2/bp2,lty = 4 ,lwd = 1, col = 30)
# posterior para mujeres
## poner el cuadro legendario
legend(locator(1),legend = c("Previa","Posterior Hombres",
                               "Posterior Mujeres","Media Hombres",
                             "Media mujeres","IC 95% Hombres",
                             "IC 95% Mujeres"),
       col= c(2,3,4,20,30,1,14),lty=c(1,1,1,4,4,2,2),lwd=c(2,2,2,2,2,2,2))
'''
###############################################
################ grafico panel
###########################################
par(mfrow = c(1,2), mar = c(3,3,1.4,1.4), mgp = c(1.75,.75,0))
#############################
##### hombres
#########################
plot(NA, NA, xlim = c(0,3), ylim = c(0,4.5), xlab = expression(theta), 
     ylab = expression(paste("p","(",theta," | ",y,")",sep="")), 
     main = "Posterior población masculina")
### previa
lines(theta, dgamma(theta, shape = a, rate = b), col = 2, lwd = 2)
# posterior para hombres
lines(theta, dgamma(theta, shape = ap1, rate = bp1), col = 3, lwd = 2)
### intervalo de credibilidad hombres
abline(v = qgamma(c(.025,.975), shape = ap1, rate = bp1), lty = 2, 
       lwd = 1.5, col = 1)
### media para los hombres
abline(v = ap1/bp1,lty = 4 ,lwd = 1, col = 20)
## poner el cuadro legendario 1
legend(locator(1),legend = c("Previa","Posterior Hombres","Media Hombres",
                             "IC 95% Hombres"),bty = "n",
       col= c(2,3,1,20),lty=c(1,1,1,4),lwd=c(2,2,2,2))
#############################
##### mujeres
########################
plot(NA, NA, xlim = c(0,3), ylim = c(0,4.5), xlab = expression(theta), 
     ylab = expression(paste("p","(",theta," | ",y,")",sep="")), 
     main = "Posterior población femenina")
### previa
lines(theta, dgamma(theta, shape = a, rate = b), col = 2, lwd = 2)
# posterior para mujeres
lines(theta, dgamma(theta, shape = ap2, rate = bp2), col = 4, lwd = 2)
### intervalo de credibilidad mujeres
abline(v = qgamma(c(.025,.975), shape = ap2, rate = bp2), lty = 2, 
       lwd = 1.5, col = 1)
### media para los mujeres
abline(v = ap2/bp2,lty = 4 ,lwd = 1, col = 30)
## poner el cuadro legendario 2
legend(locator(1),legend = c("Previa","Posterior Mujeres","Media Mujeres",
                             "IC 95% Mujeres"),bty = "n",
       col= c(2,3,1,30),lty=c(1,1,1,4),lwd=c(2,2,2,2))
'''
#####################################################
########### tabla de información
######################################################
# media posterior e intervalo de credibilidad
tab <- cbind(c(ap1/bp1, qgamma(p = c(.025,.975), shape = ap1, rate = bp1)),
             c(ap2/bp2, qgamma(p = c(.025,.975), shape = ap2, rate = bp2)))
colnames(tab) <- c("Hombres", "Mujeres")
rownames(tab) <- c("Media", "Q2.5%", "Q97.5%")
print(round(t(tab), 3))
###################################################################
###################################################################
######### INFERENCIA SOBRE eta
## ajustando la cantidad de simulaciones B
'''
con B 20000 tenemos cv 0.392
con B 50000 tenemos cv 0.392
'''
B <- 20000
## semilla
set.seed(1984)
## creación del theta para la población de hombres
MC_theta1 <- rgamma(n= B, shape = ap1, rate = bp1)
## creación del theta para la población de mujeres
MC_theta2 <- rgamma(n= B, shape = ap2, rate = bp2)
############################################################
## ahora tenemos todo para poder tener la función eta de los 
## parámetros
MC_eta <- (MC_theta2-MC_theta1)/(MC_theta1)
# probabilidad posterior de que eta > 0
# probabilidad posterior de que gamma > 0
round(mean(MC_eta > 0), 3)
# estimacion puntual de eta
round(mean(MC_eta), 3)
# estimación de la varianza de eta
round(var(MC_eta),3)
## estimación del coeficiente de variación
round(sqrt(var(MC_eta))/mean(MC_eta),3)
# intervalo de credibilidad al 95% para gamma
round(quantile(x = MC_eta, probs = c(0.025, 0.975)), 3)
###########################################################
### visulazación
###############################################
plot(NA, NA, xlim = c(-0.05,0.65), ylim = c(0,4.5), xlab = expression(eta), 
     ylab = expression(paste("p","(",eta," | ",y,")",sep="")), 
     main = "Posterior ")
# posterior para eta
lines(density(MC_eta), col = 3, lwd = 2)
### intervalo de credibilidad 
abline(v = quantile(x = MC_eta, probs = c(0.025, 0.975)), lty = 2, lwd = 1.5,
       col = 1)
### media para los hombres
abline(v = mean(MC_eta),lty = 4 ,lwd = 1, col = 2)
############ cuadro legendario
legend(locator(1),legend = c("Post. eta","Media","IC 95%"),bty = "n",
       col= c(3,2,1),lty=c(1,2,4),lwd=c(2,2,2))
##################################################################
tab2 <- cbind(c(round(mean(MC_eta), 3), 
                round(sqrt(var(MC_eta))/mean(MC_eta),3),
                round(quantile(x = MC_eta, probs = c(0.025, 0.975)), 3)[1],
                round(quantile(x = MC_eta, probs = c(0.025, 0.975)), 3)[2]))
colnames(tab2) <- c(expression(eta))
rownames(tab2) <- c("Est. Puntual", "CV","Q2.5%", "Q97.5%")
print(round(t(tab2), 3))
#################################################################
############################################################
############# analisis de sensitividad ##################
################################################################
'''
lo primero que notamos es que para ambas poblaciónes se tienen los 
mismos hiperparametros en cada previa para el analisis de sensitividad
así entonces tendremos 6 previas para analizar.
También notamos que la previa 1 es la que ya se había utilizado en los 
puntos anteriores por tanto solo traeremos sus resultados
'''
################################
#### distribución previa 1
### media
a/b
### coeficiente de variación
sqrt(a/(b**2))/(a/b)
################################
#### distribución previa 2
a2 <- 0.1
b2 <- 0.1
### media
a2/b2
### coeficiente de variación
sqrt(a2/(b2**2))/(a2/b2)
################################
#### distribución previa 3
a3 <- 1
b3 <- 1
### media
a3/b3
### coeficiente de variación
sqrt(a3/(b3**2))/(a3/b3)
################################
#### distribución previa 4
a4 <- 1
b4 <- 1/2
### media
a4/b4
### coeficiente de variación
sqrt(a4/(b4**2))/(a4/b4)
################################
#### distribución previa 5
a5 <- 1
b5 <- 1/3
### media
a5/b5
### coeficiente de variación
sqrt(a5/(b5**2))/(a5/b5)
################################
#### distribución previa 6
a6 <- 1
b6 <- 1/4
### media
a6/b6
### coeficiente de variación
sqrt(a6/(b6**2))/(a6/b6)
###############################################################
####### tabla de resultados
#############################################################
tab3 <- cbind(c(a/b,a2/b2,a3/b3,a4/b4,a5/b5,a6/b6),
              c(sqrt(a/(b**2))/(a/b),
                sqrt(a2/(b2**2))/(a2/b2),
                sqrt(a3/(b3**2))/(a3/b3),
                sqrt(a4/(b4**2))/(a4/b4),
                sqrt(a5/(b5**2))/(a5/b5),
                sqrt(a6/(b6**2))/(a6/b6)))
colnames(tab3) <- c("Media","CV")
rownames(tab3) <- c("Previa 1", "Previa 2","Previa 3", "Previa 4","Previa 5","Previa 6")
print(round(t(tab3), 3))
##################################################################
##########################################################
### grafica de las previas en un solo panel
plot(NA, NA, xlim = c(-0.1,2.5), ylim = c(0,4.5), xlab =  expression(theta), 
     ylab = expression(paste("p","(",theta,")",sep="")),
     main = "Previas")
for(i in 1:6){
      lines(theta, dgamma(theta, shape = hiper__previas[i,1],
                          rate = hiper__previas[i,2]), col = i, lwd = 2)
}
## poner el cuadro legendario 2
legend(locator(1),legend = c("Previa 1","Previa 2","Previa 3",
                             "Previa 4","Previa 5","Previa 6"),bty = "n",
       col= c(1:6),lty=1,lwd=2)
###########################################################33
### inferencia sobe eta para cada previa
##########################################################
###### como son diferentes previas primero almacenamos los hiperparámetros
hiper__previas <- matrix(data = NA, nrow = 6,ncol = 2)
hiper__previas[1,] <- c(a,b)
hiper__previas[2,] <- c(a2,b2)
hiper__previas[3,] <- c(a3,b3)
hiper__previas[4,] <- c(a4,b4)
hiper__previas[5,] <- c(a5,b5)
hiper__previas[6,] <- c(a6,b6)
###################################################
### ahora empezamos para cada previa la  calculamos el eta
## teniendo encuenta su respectiva posterior
##

# lugar donde almacenaremos el eta dado por cada posterior de su
# respectiva previa
eta_sensi <- matrix(data=NA,nrow = 20000,ncol=6)
colnames(eta_sensi)<- c("prev1","prev2","prev3","prev4","prev5","prev6")
## semilla
set.seed(1984)
for (i in 1:6) {
      ## theta para los hombres
            theta1 <- rgamma(n= B, shape = hiper__previas[i,1] + s1,
                   rate = hiper__previas[i,2] + n1)
            ## theta para las mujeres
            theta2 <- rgamma(n= B, shape = hiper__previas[i,j] + s2,
                   rate = hiper__previas[i,j+1] + n2)
            ## eta para cada posterior dada por la previa
            eta_sensi[,i] <- (theta2-theta1)/theta1
}
#########################################################
### ahora las medias de los etas respectivos
round(colMeans(eta_sensi),3)
###################################################
### intervalos
for (i in 1:6) {
      print(colnames(eta_sensi)[i])
     print(round(quantile(x = eta_sensi[,i], probs = c(0.025, 0.975)), 3))
}
###############################################
### tabla
############################################################
tab3 <- cbind(c(round(colMeans(eta_sensi),3)[1],round(quantile(x = eta_sensi[,1], probs = c(0.025, 0.975)), 3)[1],round(quantile(x = eta_sensi[,1], probs = c(0.025, 0.975)), 3)[2]),
              c(round(colMeans(eta_sensi),3)[2],round(quantile(x = eta_sensi[,2], probs = c(0.025, 0.975)), 3)[1],round(quantile(x = eta_sensi[,2], probs = c(0.025, 0.975)), 3)[2]),
              c(round(colMeans(eta_sensi),3)[3],round(quantile(x = eta_sensi[,3], probs = c(0.025, 0.975)), 3)[1],round(quantile(x = eta_sensi[,3], probs = c(0.025, 0.975)), 3)[2]),
              c(round(colMeans(eta_sensi),3)[4],round(quantile(x = eta_sensi[,4], probs = c(0.025, 0.975)), 3)[1],round(quantile(x = eta_sensi[,4], probs = c(0.025, 0.975)), 3)[2]),
              c(round(colMeans(eta_sensi),3)[5],round(quantile(x = eta_sensi[,5], probs = c(0.025, 0.975)), 3)[1],round(quantile(x = eta_sensi[,5], probs = c(0.025, 0.975)), 3)[2]),
              c(round(colMeans(eta_sensi),3)[6],round(quantile(x = eta_sensi[,6], probs = c(0.025, 0.975)), 3)[1],round(quantile(x = eta_sensi[,6], probs = c(0.025, 0.975)), 3)[2]))
colnames(tab3) <- c("Previa 1", "Previa 2","Previa 3", "Previa 4","Previa 5","Previa 6")
rownames(tab3) <- c("Media","Q2.5"," Q97.5")
print(round(t(tab3), 3))
#########################################################
####### gráfico
###############################################
par(mfrow = c(2,3))
for(i in 1:6){
      plot(NA, NA, xlim = c(-0.05,0.65), ylim = c(0,4.5), xlab = expression(eta), 
           ylab = expression(paste("p","(",eta," | ",y,")",sep="")), 
           main = paste("posterior ",i,sep=""))
      lines(density(eta_sensi[,i]), col = i, lwd = 2)
      abline(v = quantile(x = eta_sensi[,i], probs = c(0.025, 0.975)),
             lty = 2, lwd = 1.5,col = 1)
      abline(v = mean(eta_sensi[,i]),lty = 4 ,lwd = 1, col = 2)
      legend("topright",legend = c("Posterior","IC 95%","Media"),
             bty = "n",col= c(i,1,2),lty=c(1,2,4),lwd=2)
}
###################################################################
##################################################################
#### Bondad de ajuste para el modelo propuesto
####################################################################
####### población hombres
### estadisticos observados
# media
tmean_ob_1 <- mean(y1)
# desviacion
tsd_ob_1 <- sd(y1)
####################################################################
####### población mujeres
### estadisticos observados
# media
tmean_ob_2 <- mean(y2)
# desviacion
tsd_ob_2 <- sd(y2)
############################################
# distribucion predictiva posterior hombre
MC_tmean_1 <- NULL
MC_tsd_1 <- NULL
set.seed(1984)
for (i in 1:B) {
      # datos
      y1_rep  <- rpois(n = n1, lambda = MC_theta1[i])
      MC_tmean_1[i] <- mean(y1_rep)
      MC_tsd_1[i] <- sd(y1_rep)
}
############################################
# distribucion predictiva posterior mujeres
MC_tmean_2 <- NULL
MC_tsd_2 <- NULL
set.seed(1984)
for (i in 1:B) {
      # datos
      y2_rep  <- rpois(n = n2, lambda = MC_theta2[i])
      MC_tmean_2[i] <- mean(y2_rep)
      MC_tsd_2[i] <- sd(y2_rep)
}
####################################################
# grafico para la media
par(mfrow = c(1,2))
######### para hombres
plot(NA, NA, xlim = c(1.2,2.5), ylim = c(0,3), xlab = "t", 
     ylab = "p(t | y)",main = "Media post Niños")
lines(density(MC_tmean_1), col = "skyblue", lwd = 2)
abline(v = tmean_ob_1 , col = 2, lwd = 2, lty = 4)
abline(v = quantile(x = MC_tmean_1, probs = c(0.025, 0.975)),
       lty = 2, lwd = 2,col = 1)
legend("topright",legend = c("Posterior","Media","IC 95%"),
       bty = "n",col= c("skyblue",2,1),lty=c(1,2,4),lwd=2)
'''
lines(density(MC_tmean_2), col = 6, lwd = 2)
abline(v = tmean_ob_2 , col = 2, lwd = 2, lty = 4)
abline(v = quantile(x = MC_tmean_2, probs = c(0.025, 0.975)),
       lty = 2, lwd = 2,col = 1)
       '''
######### para mujeres
plot(NA, NA, xlim = c(1.7,2.8), ylim = c(0,3), xlab = "t", 
     ylab = "p(t | y)",main = "Media post Niñas")
lines(density(MC_tmean_2), col = "skyblue", lwd = 2)
abline(v = tmean_ob_2 , col = 2, lwd = 2, lty = 4)
abline(v = quantile(x = MC_tmean_2, probs = c(0.025, 0.975)),
       lty = 2, lwd = 2,col = 1)
legend("topright",legend = c("Posterior","Media","IC 95%"),
       bty = "n",col= c("skyblue",2,1),lty=c(1,2,4),lwd=2)
# ppp (valor p predictivo posterior) media
mean(MC_tmean_1  > tmean_ob_1)
mean(MC_tmean_2  > tmean_ob_2)
#######################################################
##########################################################
tab3 <- cbind(c(tmean_ob_1, quantile(x = MC_tmean_1, probs = c(0.025, 0.975))[1],
                quantile(x = MC_tmean_1, probs = c(0.025, 0.975))[2], mean(MC_tmean_1  > tmean_ob_1)),
              c(tmean_ob_2, quantile(x = MC_tmean_2, probs = c(0.025, 0.975))[1],
                quantile(x = MC_tmean_2, probs = c(0.025, 0.975))[2],mean(MC_tmean_2  > tmean_ob_2)))
colnames(tab3) <- c("Masculino", "Femenino") 
rownames(tab3) <- c("Media","Q2.5","Q97.5", "ppp")
print(round(t(tab3), 3))
###########################################################
####################################################
# grafico para la desviación
par(mfrow = c(1,2))
######### para hombres
plot(NA, NA, xlim = c(0.95,1.8), ylim = c(0,4), xlab = "t", 
     ylab = "p(t | y)",main = "Desviación post Niños")
lines(density(MC_tsd_1), col = "skyblue", lwd = 2)
abline(v = tsd_ob_1 , col = 2, lwd = 2, lty = 4)
abline(v = quantile(x = MC_tsd_1, probs = c(0.025, 0.975)),
       lty = 2, lwd = 2,col = 1)
######### para mujeres
plot(NA, NA, xlim = c(1.19,2.15), ylim = c(0,5), xlab = "t", 
     ylab = "p(t | y)",main = "Desviación post Niñas")
lines(density(MC_tsd_2), col = "skyblue", lwd = 2)
abline(v = tsd_ob_2 , col = 2, lwd = 2, lty = 4)
abline(v = quantile(x = MC_tsd_2, probs = c(0.025, 0.975)),
       lty = 2, lwd = 2,col = 1)
legend("topright",legend = c("Posterior","Media","IC 95%"),
       bty = "n",col= c("skyblue",2,1),lty=c(1,2,4),lwd=2)
# ppp (valor p predictivo posterior) desviación
mean(MC_tsd_1  > tsd_ob_1)
mean(MC_tsd_2  > tsd_ob_2)
##########################################################
##########################################################
tab3 <- cbind(c(tsd_ob_1, quantile(x = MC_tsd_1, probs = c(0.025, 0.975))[1],
                quantile(x = MC_tsd_1, probs = c(0.025, 0.975))[2], mean(MC_tsd_1  > tsd_ob_1)),
              c(tsd_ob_2, quantile(x = MC_tsd_2, probs = c(0.025, 0.975))[1],
                quantile(x = MC_tsd_2, probs = c(0.025, 0.975))[2],mean(MC_tsd_2  > tsd_ob_2)))
colnames(tab3) <- c("Masculino", "Femenino") 
rownames(tab3) <- c("Desviación estandar","Q2.5","Q97.5", "ppp")
print(round(t(tab3), 3))