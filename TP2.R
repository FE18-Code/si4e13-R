#!/usr/bin/Rscript

colors <- c('red', 'green', 'blue', 'purple', 'orange', 'black', 'pink', 'cyan', 'yellow', 'white')

portes <- c("chevre","chevre","voiture")

nb.exp <- 100
plusieurs.exp <- c()
for (j in 1:nb.exp){
  
  resultats <- c()
  nb.tirages <- 50
  for (k in 1:nb.tirages){
    exp <- sample(portes,size = 3,replace = FALSE)
    if (exp[2]=="chevre") 
      autre.porte <- 3
    else
      autre.porte = 2
    resultats= c(resultats,exp[autre.porte])
  }
  plusieurs.exp=c(plusieurs.exp, table(resultats)["voiture"]/nb.tirages)
}

boxplot(plusieurs.exp)

### le modele de maxwell boltzmann

max.boltz <- function(n,N)
  sample.int(n,N,replace=TRUE)

compte.urnes.vides <- function(omega,n){
  a <- rep(1,n)
  a[omega]=0
  sum(a)
}

n<- 10
N<-20
omega <- max.boltz(n,N)
compte.urnes.vides(omega,n)

estim.R <- function(k,n,N,nb.experiences){
  res <- c()
  for (l in 1:nb.experiences) {  
    omega <- max.boltz(n,N)
    res <- c(res, compte.urnes.vides(omega,n))
  }
  sum(res == k)/nb.experiences
}

R0.th <- function(n,N){
  aux <- 1
  eps <- 1
  for (l in 1:n){
    aux <- aux -eps*choose(n,l)*(1-l/n)^N
    eps <- -eps}
  aux
}

res <- c()
nb.experiences <- 1000
for (l in 1:1000)
  res <- c(res, estim.R(0,n,N,nb.experiences))

### un diagramme à moustaches centré sur la valeur théorique
#boxplot(res-R0.th(n,N))
boxplot(res)
plot(res, col = sample(colors))
hist(res, col = sample(colors, 1), probability=TRUE)

x <- seq(from=0.05, to=0.40, by=0.001)
densite.normale <- dnorm(x, mean=mean(res), sd=sd(res))
points(x, densite.normale, pch='.', col=sample(colors, 1))
