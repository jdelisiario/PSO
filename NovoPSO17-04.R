#IMPLEMENTAÇÃO PSO - JACIARA DOMINGOS ELISIÁRIO

set.seed(10)

#limpar workspace
rm(list = ls())

#limpar tela
cat('\014')

#usada para plotar graficos do tipo scatter em 3d
library("rgl")

#bibliotecas
library("plot3D")

#FUNÇÕES
#Esfera
spheref <- function(x){  
  sum <- sum(x^2)
  y <- sum
  return(y)
}

#Rastrigin
rastr <- function(x){
  d <- length(x)
  sum <- sum(x^2 - 10*cos(2*pi*x))
  y <- 10*d + sum
  return(y)
}

#ROSENBROCK
rosenbrock <- function(x){
  d <- length(x)
  xi <- x[1:(d-1)]
  xnext <- x[2:d]
  
  sum <- sum(100*(xnext-xi^2)^2 + (xi-1)^2)
  
  y <- sum
  return(y)
}

qtdadeExecucoes <- 1

qtdadeParticulas <- 100
dimensoes <- 3
fitness_gbest <- vector(mode = "numeric", length = qtdadeExecucoes) 
particulas <- matrix(rep(0), qtdadeExecucoes, dimensoes)

for(exec in 1:qtdadeExecucoes){
#PARAMETROS PARA INICIALIZAÇÃO
maxIter <- 1000
w <- 0.9                   
c1 <- 1
c2 <- 1


contador <- 1
fi1 <- runif(1,0.0,1.0)
fi2 <- runif(1,0.0,1.0)
funcao <- spheref #função objetivo

#EXIBICAO DE GRAFICOS
plotaGraficos <- FALSE

#INICIALIZAÇÃO DO ENXAME E DA VELOCIDADE
matriz <- matrix(runif(qtdadeParticulas*dimensoes,-10,10), 
                 qtdadeParticulas, 
                 dimensoes)
velocidade <- matrix(runif(qtdadeParticulas*dimensoes,0,1), 
                    qtdadeParticulas, 
                    dimensoes)
pbest <- matriz #na primeira execução pbest e matriz são iguais
y_fitness <- vector(mode = "numeric", length = qtdadeParticulas) #armazena a fitness das partículas atuais
ypBest <- vector(mode = "numeric", length = qtdadeParticulas) #armazena a fitness das melhores partículas pessoais
ygBest <- vector(mode = "numeric", length = 1) #armazena a fitness da melhor partícula global
inercia <- matrix(data = NA, nrow = qtdadeParticulas, ncol = (dimensoes), byrow = FALSE, dimnames = NULL)
cognitivo <- matrix(data = NA, nrow = qtdadeParticulas, ncol = (dimensoes), byrow = FALSE, dimnames = NULL)
social <- matrix(data = NA, nrow = qtdadeParticulas, ncol = (dimensoes), byrow = FALSE, dimnames = NULL)

#PLOT ENXAME INICIAL
plot(matriz,type="p", xlim=c(-20,20),ylim=c(-20,20))
plot3d(matriz,  size=4, xlim=c(-20,20),ylim=c(-20,20),zlim=c(-1,20))

#CÁLCULO DO FITNESS / APTIDÃO PARA CADA PARTÍCULA
#Aplicação das partículas na função
for (i in 1:qtdadeParticulas) {
  y_fitness[i] <- funcao(matriz[i,])
  ypBest[i] <- funcao(pbest[i,])
  if(y_fitness[i] < ypBest[i]){
    pbest[i,] <- matriz[i,]
  }
}

#PARA CALCULAR O GBEST DEVEMOS CALCULAR O ÍNDICE CORRESPONDENTE A PARTÍCULA 
#DE MENOR VALOR EM TODO O qtdadeParticulas

#calculo do índice da menor saída
yMinimo <- min(y_fitness)
indice <- 1
i <- 1
while (i < qtdadeParticulas) {
  if (y_fitness[i] != yMinimo) {
    indice <- indice + 1
    i <- i + 1
  }
  else{
    i <- qtdadeParticulas
  }
}

#CÁLCULO DO GBEST
for (i in 1:dimensoes) {
  gbest <- matriz[indice,]
  ygBest <- funcao(gbest)
}

ftmedia <- vector(mode = "numeric", length = maxIter)
ftgbest <- vector(mode = "numeric", length = maxIter)
#PARA EXIBIÇÃO DOS GRÁFICOS
if (plotaGraficos == TRUE)
{
  #plotando a funcoes
  x <- seq(-10, 10, 0.6)
  y <- x
  z <- matrix(NA, length(x), length(y))
  for (i in 1:length(x))
  {
    for (j in 1:length(y))
    {
      z[i,j] <- funcao(c(x[i],y[j]))
    }
  }
  
  persp3D(x,y,z,theta=45,phi=30)
  
  contour(x,y,z)
  par(new=T)
  
  #plotando as particulas
  plot(matriz, 
       xlim=c(-10,10), 
       ylim=c(-10,10), 
       pch=20, 
       xlab="", ylab="", col="blue", xaxt='n', yaxt='n')
}

for(iter in 1:maxIter){
  for(i in 1:qtdadeParticulas){
      inercia[i,] <- w * velocidade[i,]
      cognitivo[i,] <- ((pbest[i,] - matriz[i,]) * fi1 * c1)
      social[i,]  <- ((gbest - matriz[i,]) * fi2 * c2)
      
      velocidade[i,] <- inercia[i,] + cognitivo[i,] + social[i,]
      
  }
  
  for(a in 1:qtdadeParticulas){
    matriz[a,] <- velocidade[a,] + matriz[a,]
  }
  
  for (c in 1:qtdadeParticulas) {
    y_fitness[c] <- funcao(matriz[c,])
    ypBest[c] <- funcao(pbest[c,])
  }
  
  for (c in 1:qtdadeParticulas) {
    y_fitness[c] <- funcao(matriz[c,])
    ypBest[c] <- funcao(pbest[c,])
    if(y_fitness[c] < ypBest[c]){
      pbest[c,] <- matriz[c,]
    }
    if(y_fitness[c] < ygBest){
      gbest <- matriz[c,]
      ygBest <- funcao(gbest)
    }
        }
   
  ftmedia[iter] <- mean(y_fitness)
  ftgbest[iter]<- ygBest
 
  
  #plot(matriz,xlim=c(-20,20),ylim=c(-20,20))
  #plotando as particulas
  if(iter == 1 || iter == 200 || iter == 400 ||
     iter == 600 || iter == 800 || iter == 1000){
  if (plotaGraficos == TRUE)
  {
    
    contour(x,y,z)
    par(new=T)
    plot(matriz, 
         xlim=c(-10,10), 
         ylim=c(-10,10), 
         pch=20, 
         xlab="", ylab="", col="blue")
    #dev.off()  
    }
  }
  

 
  iter <- iter + 1
}
#plot(ftmedia, type="l")
#plot(ftgbest, type="l")
plot3d(matriz, size=4, xlim=c(-20,20),ylim=c(-20,20),zlim=c(-1,20))


#print(matriz)

#print(gbest)
#print(ygBest)

#print(qtdadeExecucoes)

particulas[exec,] <- gbest
fitness_gbest[exec] <- ygBest

mediaFitness <- mean(fitness_gbest)
desvio_padrao <- sd(fitness_gbest)
pior <- max(fitness_gbest)
melhor <- min(fitness_gbest)
mediana <- median(fitness_gbest)

write.table(particulas, file="rosenbrock_particulas_30dim_ANOVA.csv")
write.table(fitness_gbest, file="rosenbrock_ftgbest_30dim_ANOVA.csv")
write.table(mediaFitness, file="rosenbrock_mediagbest_30dim_ANOVA.csv")
write.table(desvio_padrao, file="rosenbrock_desvpad_30dim_ANOVA.csv")
write.table(pior, file="rosenbrock_pior_30dim_ANOVA.csv")
write.table(melhor, file="rosenbrock_melhor_30dim_ANOVA.csv")
write.table(mediana, file="rosenbrock_mediana_30dim_ANOVA.csv")

}
