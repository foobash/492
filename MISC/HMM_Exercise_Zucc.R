# Playing with WalMart data on Zucchini exercise
# Richard Shea 
# Math 625.492 Project

# Chapter 6, R-code for exercise 5

# !!! MUST RUN 'function-project.R' FIRST!!!

# SET WORKING DIRCTORY
setwd("F:/personal/JHU/625.492/492_project/Time_Series_project/R") #home

# Chapter 6, R-code for exercise 5

library(HiddenMarkov)

############### Functions #########################################

pois.HMM.mle <- function(x,m,lambda0 ,gamma0 ,...)
{
 parvect0 <- pois.HMM.pn2pw(m,lambda0 ,gamma0)
 mod <- nlm(pois.HMM.mllk ,parvect0 ,x=x,m=m)
 pn <- pois.HMM.pw2pn(m, mod$estimate)
 mllk <- mod$minimum
 np <- length(parvect0)
 AIC <- 2*( mllk+np)
 n <- sum(!is.na(x))
 BIC <- 2* mllk+np*log(n)
 list(lambda=pn$lambda ,gamma=pn$gamma ,delta=pn$delta ,
             code=pn$code ,mllk=mllk ,AIC=AIC ,BIC=BIC)
}
 
####################################################################

pois.HMM.pn2pw <- function(m,lambda ,gamma)
{
 tlambda <- log(lambda)
 tgamma <- NULL
 if(m>1)
   {
   foo <- log(gamma/diag(gamma))
   tgamma <- as.vector(foo[! diag(m)])
   }
 parvect <- c(tlambda ,tgamma)
 parvect 
}

####################################################################

pois.HMM.mllk <- function(parvect ,x,m ,...)
{
 if(m==1) return(-sum(dpois(x,exp(parvect),log=TRUE)))
 n <- length(x)
 pn <- pois.HMM.pw2pn(m,parvect)
 allprobs <- outer(x,pn$lambda ,dpois)
 allprobs <- ifelse (!is.na(allprobs),allprobs ,1)
 lscale <- 0
 foo <- pn$delta
 for (i in 1:n)
   {
   foo <- foo %*% pn$gamma*allprobs[i,]
   sumfoo <- sum(foo)
   lscale <- lscale+log(sumfoo)
   foo <- foo/sumfoo
   }
 mllk <- -lscale
 mllk
} 
 
####################################################################

pois.HMM.pw2pn <- function(m,parvect)
{
 epar <- exp(parvect)
 lambda <- epar [1:m]
 gamma <- diag(m)
 if(m>1)
   {
   gamma [! gamma] <- epar [(m+1):(m*m)]
   gamma <- gamma/apply(gamma ,1,sum)
   }
 delta <- solve(t(diag(m)-gamma +1),rep(1,m))
 list(lambda=lambda ,gamma=gamma ,delta=delta)
}



####################################################################

pois.HMM.pseudo_residuals <-
 function(x,m,lambda ,gamma , delta=NULL ,...)
{
 if(is.null(delta))delta <-solve(t(diag(m)-gamma +1),rep(1,m))
 n <- length(x)
 cdists <- pois.HMM.conditionals(x,m,lambda , gamma ,
               delta=delta ,xrange =0: max(x))$cdists
 cumdists <- rbind(rep(0,n),apply(cdists ,2,cumsum))
 ul <- uh <- rep(NA ,n)
 for (i in 1:n)
   {
   ul[i] <- cumdists[x[i]+1,i]
   uh[i] <- cumdists[x[i]+2,i]
   }
 um <- 0.5*(ul+uh)
 npsr <- qnorm(rbind(ul ,um ,uh))
 npsr
}

####################################################################

statdist <- function(gamma){
  m = dim(gamma)[1]
  matrix(1,1,m) %*% solve(diag(1,m) - gamma + matrix(1,m,m))
}

####################################################################


x <- read.csv("Train_Store_33.csv")
x <- data.matrix(x$Weekly_Sales)
#x <- data.matrix(x)
x <- as.vector(x)
x <- round(log(x), digits = 0) #need to round and log sales to get algo to work.

# case m=1

m1= 1
lambda1= c(5)
gamma1= 1
delta1= 1
res1 <- pois.HMM.mle(x,m1,lambda1,gamma1)

# case m=2
m2= 2
lambda2= c(3,7)
gamma2= rbind(c(0.9,0.1),c(0.1,0.9))
delta2= statdist(gamma2)
res2 <- pois.HMM.mle(x,m2,lambda2,gamma2)

# case m=3
m3 = 3
lambda3 = c(3,5,7)
gamma3 = rbind(c(0.9,0.05,0.05),c(0.05,0.9,0.05),c(0.05,0.05,0.9))
delta3 = statdist(gamma3)
res3 <- pois.HMM.mle(x,m3,lambda3,gamma3)

# case m=4
m4 = 4
lambda4 = c(1,3,7,14)
gamma4 = rbind(c(0.85,0.05,0.05,0.05),c(0.05,0.85,0.05,0.05),
               c(0.05,0.05,0.85,0.05),c(0.05,0.05,0.05,0.85))
delta4 = statdist(gamma4)
res4 <- pois.HMM.mle(x,m4,lambda4,gamma4)



