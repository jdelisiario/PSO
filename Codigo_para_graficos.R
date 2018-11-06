#CÓDIGO PARA GERAÇÃO DOS GRÁFICOS - Jaciara Domingos Elisiário

#limpar workspace
rm(list = ls())

#limpar tela
cat('\014')

#biblioteca para gráficos tipo violino
library(ggplot2)

#BOXPLOTS
bx_esfera <- read.table("bx_esferaSemOut.csv",header=T,sep=";")
boxplot(bx_esfera)

bx_rastr <- read.table("bx_rastriginSemOut.csv",header=T,sep=";")
boxplot(bx_rastr)

bx_rosen <- read.table("bx_rosenbrockSemOut.csv",header=T,sep=";")
boxplot(bx_rosen)

#VIOLINOS
esfera <- read.table("esfera.csv",header=T,sep=";")
teste = ggplot(esfera, aes(x=factor(Dimensoes), y=Fitness))
teste + geom_violin(trim = FALSE, scale="width", inherit.aes = TRUE)

rastrigin <- read.table("rastrigin.csv",header=T,sep=";")
teste = ggplot(rastrigin, aes(x=factor(Dimensoes), y=Fitness))
teste + geom_violin(trim = FALSE, scale="width", inherit.aes = TRUE)

rosenbrock <- read.table("rosenbrock.csv",header=T,sep=" ")
teste = ggplot(rosenbrock, aes(x=factor(Dimensoes), y=Fitness))
teste + geom_violin(trim = FALSE, scale="width", inherit.aes = TRUE)

#EQM ESFERA
esfera3 <- read.table("esfera_particulas_3dim.csv",header=T,sep=" ")
esfera10 <- read.table("esfera_particulas_10dim.csv",header=T,sep=" ")
esfera20 <- read.table("esfera_particulas_20dim.csv",header=T,sep=" ")
esfera30 <- read.table("esfera_particulas_30dim.csv",header=T,sep=" ")

eqm3_esfera <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 3 dim
eqm10_esfera <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 10 dim
eqm20_esfera <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 20 dim
eqm30_esfera <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 30 dim

j<-1
for(i in 1:30){
  eqm3_esfera[i] <- mean(sqrt((esfera3[i,j]-0)^2 + (esfera3[i,j+1]-0)^2 + (esfera3[i,j+2]-0)^2))
}
plot(eqm3_esfera, type='l')
write.table(eqm3_esfera, file="eqm3_esfera.csv")

j<-1
for(i in 1:30){
  eqm10_esfera[i] <- mean(sqrt(
                        (esfera10[i,j]-0)^2 + (esfera10[i,j+1]-0)^2 + (esfera10[i,j+2]-0)^2 +
                          (esfera10[i,j+3]-0)^2 + (esfera10[i,j+4]-0)^2 + (esfera10[i,j+5]-0)^2 +
                          (esfera10[i,j+6]-0)^2 + (esfera10[i,j+7]-0)^2 + (esfera10[i,j+8]-0)^2 +
                          (esfera10[i,j+9]-0)^2
                        )
                   )
}
plot(eqm10_esfera, type='l')
write.table(eqm10_esfera, file="eqm10_esfera.csv")

j<-1
for(i in 1:30){
  eqm20_esfera[i] <- mean(sqrt(
    (esfera20[i,j]-0)^2 + (esfera20[i,j+1]-0)^2 + (esfera20[i,j+2]-0)^2 +
      (esfera20[i,j+3]-0)^2 + (esfera20[i,j+4]-0)^2 + (esfera20[i,j+5]-0)^2 +
      (esfera20[i,j+6]-0)^2 + (esfera20[i,j+7]-0)^2 + (esfera20[i,j+8]-0)^2 +
      (esfera20[i,j+9]-0)^2 + (esfera20[i,j+10]-0)^2 + (esfera20[i,j+11]-0)^2 +
      (esfera20[i,j+12]-0)^2 + (esfera20[i,j+13]-0)^2 + (esfera20[i,j+14]-0)^2 +
      (esfera20[i,j+15]-0)^2 + (esfera20[i,j+16]-0)^2 + (esfera20[i,j+17]-0)^2 +
      (esfera20[i,j+18]-0)^2 + (esfera20[i,j+19]-0)^2
      )
  )
}
plot(eqm20_esfera, type='l')
write.table(eqm20_esfera, file="eqm20_esfera.csv")

j<-1
for(i in 1:30){
  eqm30_esfera[i] <- mean(sqrt(
    (esfera30[i,j]-0)^2 + (esfera30[i,j+1]-0)^2 + (esfera30[i,j+2]-0)^2 +
      (esfera30[i,j+3]-0)^2 + (esfera30[i,j+4]-0)^2 + (esfera30[i,j+5]-0)^2 +
      (esfera30[i,j+6]-0)^2 + (esfera30[i,j+7]-0)^2 + (esfera30[i,j+8]-0)^2 +
      (esfera30[i,j+9]-0)^2 + (esfera30[i,j+10]-0)^2 + (esfera30[i,j+11]-0)^2 +
      (esfera30[i,j+12]-0)^2 + (esfera30[i,j+13]-0)^2 + (esfera30[i,j+14]-0)^2 +
      (esfera30[i,j+15]-0)^2 + (esfera30[i,j+16]-0)^2 + (esfera30[i,j+17]-0)^2 +
      (esfera30[i,j+18]-0)^2 + (esfera30[i,j+19]-0)^2 + (esfera30[i,j+20]-0)^2 +
      (esfera30[i,j+21]-0)^2 + (esfera30[i,j+22]-0)^2 + (esfera30[i,j+23]-0)^2 +
      (esfera30[i,j+24]-0)^2 + (esfera30[i,j+25]-0)^2 + (esfera30[i,j+26]-0)^2 +
      (esfera30[i,j+27]-0)^2 + (esfera30[i,j+28]-0)^2 + (esfera30[i,j+29]-0)^2
  )
  )
}
plot(eqm30_esfera,type='l')
write.table(eqm30_esfera, file="eqm30_esfera.csv")

#EQM RASTRIGIN
rastrigin3 <- read.table("rastrigin_particulas_3dim.csv",header=T,sep=" ")
rastrigin10 <- read.table("rastrigin_particulas_10dim.csv",header=T,sep=" ")
rastrigin20 <- read.table("rastrigin_particulas_20dim.csv",header=T,sep=" ")
rastrigin30 <- read.table("rastrigin_particulas_30dim.csv",header=T,sep=" ")

eqm3_rastrigin <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 3 dim
eqm10_rastrigin <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 10 dim
eqm20_rastrigin <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 20 dim
eqm30_rastrigin <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 30 dim

j<-1
for(i in 1:30){
  eqm3_rastrigin[i] <- mean(sqrt((rastrigin3[i,j]-0)^2 + (rastrigin3[i,j+1]-0)^2 + (rastrigin3[i,j+2]-0)^2))
}
plot(eqm3_rastrigin, type='l')
write.table(eqm3_rastrigin, file="eqm3_rastrigin.csv")

j<-1
for(i in 1:30){
  eqm10_rastrigin[i] <- mean(sqrt(
    (rastrigin10[i,j]-0)^2 + (rastrigin10[i,j+1]-0)^2 + (rastrigin10[i,j+2]-0)^2 +
      (rastrigin10[i,j+3]-0)^2 + (rastrigin10[i,j+4]-0)^2 + (rastrigin10[i,j+5]-0)^2 +
      (rastrigin10[i,j+6]-0)^2 + (rastrigin10[i,j+7]-0)^2 + (rastrigin10[i,j+8]-0)^2 +
      (rastrigin10[i,j+9]-0)^2
  )
  )
}
plot(eqm10_rastrigin,type='l')
write.table(eqm10_rastrigin, file="eqm10_rastrigin.csv")

j<-1
for(i in 1:30){
  eqm20_rastrigin[i] <- mean(sqrt(
    (rastrigin20[i,j]-0)^2 + (rastrigin20[i,j+1]-0)^2 + (rastrigin20[i,j+2]-0)^2 +
      (rastrigin20[i,j+3]-0)^2 + (rastrigin20[i,j+4]-0)^2 + (rastrigin20[i,j+5]-0)^2 +
      (rastrigin20[i,j+6]-0)^2 + (rastrigin20[i,j+7]-0)^2 + (rastrigin20[i,j+8]-0)^2 +
      (rastrigin20[i,j+9]-0)^2 + (rastrigin20[i,j+10]-0)^2 + (rastrigin20[i,j+11]-0)^2 +
      (rastrigin20[i,j+12]-0)^2 + (rastrigin20[i,j+13]-0)^2 + (rastrigin20[i,j+14]-0)^2 +
      (rastrigin20[i,j+15]-0)^2 + (rastrigin20[i,j+16]-0)^2 + (rastrigin20[i,j+17]-0)^2 +
      (rastrigin20[i,j+18]-0)^2 + (rastrigin20[i,j+19]-0)^2
  )
  )
}
plot(eqm20_rastrigin,type='l')
write.table(eqm20_rastrigin, file="eqm20_rastrigin.csv")

j<-1
for(i in 1:30){
  eqm30_rastrigin[i] <- mean(sqrt(
    (rastrigin30[i,j]-0)^2 + (rastrigin30[i,j+1]-0)^2 + (rastrigin30[i,j+2]-0)^2 +
      (rastrigin30[i,j+3]-0)^2 + (rastrigin30[i,j+4]-0)^2 + (rastrigin30[i,j+5]-0)^2 +
      (rastrigin30[i,j+6]-0)^2 + (rastrigin30[i,j+7]-0)^2 + (rastrigin30[i,j+8]-0)^2 +
      (rastrigin30[i,j+9]-0)^2 + (rastrigin30[i,j+10]-0)^2 + (rastrigin30[i,j+11]-0)^2 +
      (rastrigin30[i,j+12]-0)^2 + (rastrigin30[i,j+13]-0)^2 + (rastrigin30[i,j+14]-0)^2 +
      (rastrigin30[i,j+15]-0)^2 + (rastrigin30[i,j+16]-0)^2 + (rastrigin30[i,j+17]-0)^2 +
      (rastrigin30[i,j+18]-0)^2 + (rastrigin30[i,j+19]-0)^2 + (rastrigin30[i,j+20]-0)^2 +
      (rastrigin30[i,j+21]-0)^2 + (rastrigin30[i,j+22]-0)^2 + (rastrigin30[i,j+23]-0)^2 +
      (rastrigin30[i,j+24]-0)^2 + (rastrigin30[i,j+25]-0)^2 + (rastrigin30[i,j+26]-0)^2 +
      (rastrigin30[i,j+27]-0)^2 + (rastrigin30[i,j+28]-0)^2 + (rastrigin30[i,j+29]-0)^2
  )
  )
}
plot(eqm30_rastrigin,type='l')
write.table(eqm30_rastrigin, file="eqm30_rastrigin.csv")

#EQM ROSENBROCK
rosenbrock3 <- read.table("rosenbrock_particulas_3dim.csv",header=T,sep=" ")
rosenbrock10 <- read.table("rosenbrock_particulas_10dim.csv",header=T,sep=" ")
rosenbrock20 <- read.table("rosenbrock_particulas_20dim.csv",header=T,sep=" ")
rosenbrock30 <- read.table("rosenbrock_particulas_30dim.csv",header=T,sep=" ")

eqm3_rosenbrock <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 3 dim
eqm10_rosenbrock <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 10 dim
eqm20_rosenbrock <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 20 dim
eqm30_rosenbrock <- vector(mode = "numeric", length = 30) #armazena a distância euclidiana / erro quadrático médio de 30 dim

j<-1
for(i in 1:30){
  eqm3_rosenbrock[i] <- mean(sqrt((rosenbrock3[i,j]-1)^2 + (rosenbrock3[i,j+1]-1)^2 + (rosenbrock3[i,j+2]-1)^2))
}
plot(eqm3_rosenbrock,type='l')
write.table(eqm3_rosenbrock, file="eqm3_rosenbrock.csv")

j<-1
for(i in 1:30){
  eqm10_rosenbrock[i] <- mean(sqrt(
    (rosenbrock10[i,j]-1)^2 + (rosenbrock10[i,j+1]-1)^2 + (rosenbrock10[i,j+2]-1)^2 +
      (rosenbrock10[i,j+3]-1)^2 + (rosenbrock10[i,j+4]-1)^2 + (rosenbrock10[i,j+5]-1)^2 +
      (rosenbrock10[i,j+6]-1)^2 + (rosenbrock10[i,j+7]-1)^2 + (rosenbrock10[i,j+8]-1)^2 +
      (rosenbrock10[i,j+9]-1)^2
  )
  )
}
plot(eqm10_rosenbrock,type='l')
write.table(eqm10_rosenbrock, file="eqm10_rosenbrock.csv")

j<-1
for(i in 1:30){
  eqm20_rosenbrock[i] <- mean(sqrt(
    (rosenbrock20[i,j]-1)^2 + (rosenbrock20[i,j+1]-1)^2 + (rosenbrock20[i,j+2]-1)^2 +
      (rosenbrock20[i,j+3]-1)^2 + (rosenbrock20[i,j+4]-1)^2 + (rosenbrock20[i,j+5]-1)^2 +
      (rosenbrock20[i,j+6]-1)^2 + (rosenbrock20[i,j+7]-1)^2 + (rosenbrock20[i,j+8]-1)^2 +
      (rosenbrock20[i,j+9]-1)^2 + (rosenbrock20[i,j+10]-1)^2 + (rosenbrock20[i,j+11]-1)^2 +
      (rosenbrock20[i,j+12]-1)^2 + (rosenbrock20[i,j+13]-1)^2 + (rosenbrock20[i,j+14]-1)^2 +
      (rosenbrock20[i,j+15]-1)^2 + (rosenbrock20[i,j+16]-1)^2 + (rosenbrock20[i,j+17]-1)^2 +
      (rosenbrock20[i,j+18]-1)^2 + (rosenbrock20[i,j+19]-1)^2
  )
  )
}
plot(eqm20_rosenbrock,type='l')
write.table(eqm20_rosenbrock, file="eqm20_rosenbrock.csv")

j<-1
for(i in 1:30){
  eqm30_rosenbrock[i] <- mean(sqrt(
    (rosenbrock30[i,j]-1)^2 + (rosenbrock30[i,j+1]-1)^2 + (rosenbrock30[i,j+2]-1)^2 +
      (rosenbrock30[i,j+3]-1)^2 + (rosenbrock30[i,j+4]-1)^2 + (rosenbrock30[i,j+5]-1)^2 +
      (rosenbrock30[i,j+6]-1)^2 + (rosenbrock30[i,j+7]-1)^2 + (rosenbrock30[i,j+8]-1)^2 +
      (rosenbrock30[i,j+9]-1)^2 + (rosenbrock30[i,j+10]-1)^2 + (rosenbrock30[i,j+11]-1)^2 +
      (rosenbrock30[i,j+12]-1)^2 + (rosenbrock30[i,j+13]-1)^2 + (rosenbrock30[i,j+14]-1)^2 +
      (rosenbrock30[i,j+15]-1)^2 + (rosenbrock30[i,j+16]-1)^2 + (rosenbrock30[i,j+17]-1)^2 +
      (rosenbrock30[i,j+18]-1)^2 + (rosenbrock30[i,j+19]-1)^2 + (rosenbrock30[i,j+20]-1)^2 +
      (rosenbrock30[i,j+21]-1)^2 + (rosenbrock30[i,j+22]-1)^2 + (rosenbrock30[i,j+23]-1)^2 +
      (rosenbrock30[i,j+24]-1)^2 + (rosenbrock30[i,j+25]-1)^2 + (rosenbrock30[i,j+26]-1)^2 +
      (rosenbrock30[i,j+27]-1)^2 + (rosenbrock30[i,j+28]-1)^2 + (rosenbrock30[i,j+29]-1)^2
  )
  )
}
plot(eqm30_rosenbrock,type='l')
write.table(eqm30_rosenbrock, file="eqm30_rosenbrock.csv")

#GRÁFICOS DE EVOLUÇÃO
evo_esfera <- read.table("Evo_Esfera_1a30.csv",header=F,sep=" ")
evo_rastrigin <- read.table("Evo_Rastrigin_1a30.csv",header=F,sep=" ")
evo_rosenbrock <- read.table("Evo_Rosenbrock_1a30.csv",header=F,sep=" ")

plot(evo_esfera[,2]~evo_esfera[,1], xlim=c(0,30),type="l",cex.axis = 0.8)
axis(1,c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30))
plot(evo_rastrigin[,2]~evo_rastrigin[,1], xlim=c(0,30),type="l")

axis(1,c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30))
plot(evo_rosenbrock[,2]~evo_rosenbrock[,1], xlim=c(0,30), type="l")
axis(1,c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30))

