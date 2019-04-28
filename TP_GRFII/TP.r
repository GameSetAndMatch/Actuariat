## TP GRF 2 ##

setwd("C:/Users/matis/Desktop/ACT-2011 GRF 2/TP")

library(derivmkts)

## 1 ####

data <- as.numeric(unlist(read.csv2("Data.csv", sep = ";"))) ## équipe 21


##Estime volatilité


ContReturn <- numeric(length(data)-1) ## 1 de moins

for(i in seq(length(data)-1))
    {
                    ContReturn[i] <- log(data[i+1]/data[i])

}
ContReturn
sd <- sd(ContReturn)*sqrt(12) ##Annuelle
sd

##Estime rf

Rfree <- read.csv("lookup-2.csv", skip = 11)[,2]/100
Rfree
length(Rfree)#60 chill
Rfree_est <- log(1+mean(Rfree))

t <- c(0.5, 2)

## 2 ####

# 4 périodes

call05 <- binomopt(s=100, k=120, v=sd, r=Rfree_est, tt=0.5, d=0, nstep=4, american=F, putopt=F, 
                  returntrees=T)
call05$price
call05$oppricetree
call05$deltatree
call05$bondtree
binomplot(s=100, k=120, v=sd, r=Rfree_est, tt=0.5, d=0, nstep=4, american=F, putopt=F, 
          returnprice=T, plotarrows=T, drawstrike=T, plotvalues=T, titles=T)

call2 <- binomopt(s=100, k=120, v=sd, r=Rfree_est, tt=2, d=0, nstep=4, american=T, putopt=F, 
                  returntrees=T)
call2$price
call2$oppricetree
call2$deltatree
call2$bondtree
binomplot(s=100, k=120, v=sd, r=Rfree_est, tt=2, d=0, nstep=4, american=F, putopt=F, 
          returnprice=T, plotarrows=T, drawstrike=T, plotvalues=T, titles=T)

put05 <- binomopt(s=100, k=90, v=sd, r=Rfree_est, tt=0.5, d=0, nstep=4, american=F, putopt=T, 
         returntrees=T)
put05$price
put05$oppricetree
put05$deltatree
put05$bondtree
binomplot(s=100, k=90, v=sd, r=Rfree_est, tt=0.5, d=0, nstep=4, american=F, putopt=T, 
          returnprice=T, plotarrows=T, drawstrike=T, plotvalues=T, titles=T)

put2 <- binomopt(s=100, k=90, v=sd, r=Rfree_est, tt=2, d=0, nstep=4, american=F, putopt=T, 
                  returntrees=T)
put2$price
put2$oppricetree
put2$deltatree
put2$bondtree
binomplot(s=100, k=90, v=sd, r=Rfree_est, tt=2, d=0, nstep=4, american=F, putopt=T, 
          returnprice=T, plotarrows=T, drawstrike=T, plotvalues=T, titles=T)

# 100 périodes

call05 <- binomopt(s=100, k=120, v=sd, r=Rfree_est, tt=0.5, d=0, nstep=100, american=F, putopt=F, 
                   returntrees=T)
call05$price

hist(pmax(call05$stree[,101]-120, 0))

call05$deltatree[, c(1, 26, 51, 76)]
call05$greeks[1]
hist(call05$deltatree[1:26, 26])
hist(call05$deltatree[1:51, 51])
hist(call05$deltatree[1:76, 76])

call05$bondtree[, c(1, 26, 51, 76)]
call05$bondtree[1, 1]
hist(call05$bondtree[1:26, 26])
hist(call05$bondtree[1:51, 51])
hist(call05$bondtree[1:76, 76])

call2 <- binomopt(s=100, k=120, v=sd, r=Rfree_est, tt=2, d=0, nstep=100, american=F, putopt=F, 
                  returntrees=T)
call2$price

hist(pmax(call2$stree[,101]-120, 0))

call2$deltatree[, c(1, 26, 51, 76)]
call2$greeks[1]
hist(call2$deltatree[1:26, 26])
hist(call2$deltatree[1:51, 51])
hist(call2$deltatree[1:76, 76])

call2$bondtree[, c(1, 26, 51, 76)]
call2$bondtree[1, 1]
hist(call2$bondtree[1:26, 26])
hist(call2$bondtree[1:51, 51])
hist(call2$bondtree[1:76, 76])

put05 <- binomopt(s=100, k=90, v=sd, r=Rfree_est, tt=0.5, d=0, nstep=100, american=F, putopt=T, 
                  returntrees=T)
put05$price

hist(pmax(90 - put05$stree[,101], 0))

put05$deltatree[, c(1, 26, 51, 76)]
put05$greeks[1]
hist(put05$deltatree[1:26, 26])
hist(put05$deltatree[1:51, 51])
hist(put05$deltatree[1:76, 76])

put05$bondtree[, c(1, 26, 51, 76)]
put05$bondtree[1, 1]
hist(put05$bondtree[1:26, 26])
hist(put05$bondtree[1:51, 51])
hist(put05$bondtree[1:76, 76])

put2 <- binomopt(s=100, k=90, v=sd, r=Rfree_est, tt=2, d=0, nstep=100, american=F, putopt=T, 
                 returntrees=T)
put2$price

hist(pmax(90 - put2$stree[,101], 0))

put2$deltatree[, c(1, 26, 51, 76)]
put2$greeks[1]
hist(put2$deltatree[1:26, 26])
hist(put2$deltatree[1:51, 51])
hist(put2$deltatree[1:76, 76])

put2$bondtree[, c(1, 26, 51, 76)]
put2$bondtree[1, 1]
hist(put2$bondtree[1:26, 26])
hist(put2$bondtree[1:51, 51])
hist(put2$bondtree[1:76, 76])


## 3 ####

par(mfrow=c(1,2))
k <- seq(0, 200, by = 0.1)

#### 4 périodes


#Put2

Res <- sapply(k, function(i) binomopt(100, i, sd, Rfree_est, 2, 0, nstep = 4, putopt = TRUE, american = F))

plot(k, Res, type = "l", xlab = "Prix d'exercice", ylab = "Prix de l'option", main = "Put 2 ans, 4 périodes")

#Call2

Res <- sapply(k, function(i) binomopt(100, i, sd, Rfree_est, 2, 0, nstep = 4, american = F))

plot(k, Res, type = "l", xlab = "Prix d'exercice", ylab = "Prix de l'option", main = "Call 2 ans, 4 périodes")



#### 100 périodes


#Put2


Res <- sapply(k, function(i) binomopt(100, i, sd, Rfree_est, 2, 0, nstep = 100, putopt = TRUE, american = F))

plot(k, Res, type = "l", xlab = "Prix d'exercice", ylab = "Prix de l'option", main = "Put 2 ans, 100 périodes")


#Call2


Res <- sapply(k, function(i) binomopt(100, i, sd, Rfree_est, 2, 0, nstep = 100, american = F))

plot(k, Res, type = "l", xlab = "Prix d'exercice", ylab = "Prix de l'option", main = "Call 2 ans, 100 périodes")




## 4 ####

par(mfrow=c(1,1))

# 4 périodes
put2 <- binomopt(s=100, k=90, v=sd, r=Rfree_est, tt=2, d=0, nstep=4, american=T, putopt=T, 
                 returntrees=T)
put2$price
put2$oppricetree
put2$deltatree
put2$bondtree
binomplot(s=100, k=90, v=sd, r=Rfree_est, tt=2, d=0, nstep=4, american=T, putopt=T, 
          returnprice=T, plotarrows=T, drawstrike=T, plotvalues=T, titles=T)

Res <- sapply(k, function(i) binomopt(100, i, sd, Rfree_est, 2, 0, nstep = 4, putopt = TRUE, american = T))

plot(k, Res, type = "l", xlab = "Prix d'exercice", ylab = "Prix de l'option", main = "Put 2 ans, 4 périodes")

# 100 périodes
put2 <- binomopt(s=100, k=90, v=sd, r=Rfree_est, tt=2, d=0, nstep=100, american=T, putopt=T, 
                 returntrees=T)
put2$price

hist(pmax(90-put2$stree[,101],0))

put2$deltatree[, c(1, 26, 51, 76)]
put2$greeks[1]
hist(put2$deltatree[1:26, 26])
hist(put2$deltatree[1:51, 51])
hist(put2$deltatree[1:76, 76])

put2$bondtree[, c(1, 26, 51, 76)]
put2$bondtree[1, 1]
hist(put2$bondtree[1:26, 26])
hist(put2$bondtree[1:51, 51])
hist(put2$bondtree[1:76, 76])

Res <- sapply(k, function(i) binomopt(100, i, sd, Rfree_est, 2, 0, nstep = 100, putopt = TRUE, american = T))

plot(k, Res, type = "l", xlab = "Prix d'exercice", ylab = "Prix de l'option", main = "Put 2 ans, 100 périodes")

put2$exertree

sum(put2$exertree[, 26]) ## Le nbre d'embranchement
sum(put2$exertree[, 51])
sum(put2$exertree[, 76])
sum(put2$exertree[, 101])

## 5 ####

bs <- function(S0, K, t, r, delta = 0, sd, Call = TRUE) 
{
    
    FPS <- S0*exp(-delta*t)
    FPK <- K*exp(-r*t)
    
    
    d1 <- (log(FPS/FPK) + 0.5*sd^2*t)/(sd*sqrt(t))
    d2 <- d1 - sd*sqrt(t)
    
    if(Call)
    {
        V <- FPS*pnorm(d1) - FPK*pnorm(d2)
        Delta <- exp(-delta*t)*pnorm(d1)
        Gamma <- exp(-delta*t)*dnorm(d1)/S0/sd/sqrt(t)
        Theta <- 0
        Vega <- 0
        Rho <- 0
        Psi <- 0
    }
    
    else
    {
        V <- FPK*pnorm(-d2) - FPS*pnorm(-d1)
        Delta <- -exp(-delta*t)*pnorm(-d1)
        Gamma <- exp(-delta*t)*dnorm(d1)/S0/sd/sqrt(t)
        Theta <- 0 
        Vega <- 0
        Rho <- 0
        Psi <- 0
    }
        
    
    list(Value = V, Delta = Delta, Gamma = Gamma, Vega = Vega, Rho = Rho, Psi = Psi)
}


P <- lapply(seq_along(t), function(i) bs(S0 = 100, K = 90, t = t[i], r = Rfree_est, sd = sd, Call = F))
C <- lapply(seq_along(t), function(i) bs(S0 = 100, K = 120, t = t[i], r = Rfree_est, sd = sd))

P[[1]]$Value
P[[2]]$Value
C[[1]]$Value
C[[2]]$Value

## Facon plus easy de tout avoir

greeks2(bsput, list(s=100, k=90, v=sd, r=Rfree_est, tt=0.5, d=0)) ## Formule package derivmkts
greeks2(bsput, list(s=100, k=90, v=sd, r=Rfree_est, tt=2, d=0))
greeks2(bscall, list(s=100, k=120, v=sd, r=Rfree_est, tt=0.5, d=0))
greeks2(bscall, list(s=100, k=120, v=sd, r=Rfree_est, tt=2, d=0))

## 6 ####


P[[2]][-1]
C[[2]][-1]

greeks2(bsput, list(s=100, k=90, v=sd, r=Rfree_est, tt=2, d=0))
greeks2(bscall, list(s=100, k=120, v=sd, r=Rfree_est, tt=2, d=0))

