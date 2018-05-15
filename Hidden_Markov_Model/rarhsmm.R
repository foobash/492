# Markov Chain Model for Walmat Data.
# Richard Shea 
# Math 625.492 Project

# !!! MUST RUN 'function-project.R' FIRST!!!

# SET WORKING DIRCTORY

setwd("F:/personal/JHU/625.492/492_project/Time_Series_project/R")  #home

library("rarhsmm")

set.seed(332213)

x1 <- read.csv("Train_Store_20.csv")
test <-read.csv("Test_Store_20.csv")
x <- x1$nWeekly_sales  # collect our normalized data
x <- data.matrix(x)  # this R package wants it in matrix form.

min <- min(x1$rWeekly_Sales) #minum of min/max normalization
max <- max(x1$rWeekly_Sales) #max of min/max normalization 

y <- x
#first, fit a Gaussian HMM
m <- 3 #<--- set number of states!

#initialize the list of means

#mu <- list(apply(y,2,mean), apply(y,2,mean)) # for 2 state
mu <- list(apply(y,2,mean), apply(y,2,mean), apply(y,2,mean)) # for 3 state

#initialize the list of covariance matrices
#sigma <- list(cov(y)*1.2,cov(y)*0.8)  # 2 state
sigma <- list(cov(y)*1.25,cov(y)*0.75,cov(y)*1) # 3 state

#initialize the prior probability
#delta <- c(0.5,0.5) # for 2 state.
delta <- c(0.33,0.33,0.33)  # 3 state

#initialize the transition probabilities
#gamma <- matrix(c(0.9,0.1,0.2,0.8),2,2,byrow=TRUE) # for 2 state
gamma <- matrix(c(0.9,0.05,0.05,0.2,0.4,0.4,0.33,0.33,0.33),3,3,byrow=TRUE) # for 3 states.
mod1 <- list(m=m,mu=mu,sigma=sigma,delta=delta,gamma=gamma)

#will not run without a shrinkage on the covariance matrices because the
#series is not long enough to reliably estimate the covariance structure

fit1 <- em.hmm(y=y,mod=mod1,cov.shrink=0.0001)
st1 <- viterbi.hmm(y=y,mod=fit1)  # The best fitted state sequence of each observation. 
sp1 <- smooth.hmm(y=y,mod=fit1) # For each week, we have the probability of being in each state.

yf <- hmm.predict(y = y, mod = fit1) #store one step ahead 
y1 <- yf$forecast # store forecast

y1 <- con2back(y1)# convert back to sales unormalized.

y1t <- test$rWeekly_Sales[1] # one step ahead true sales data.

MAPE <- function(y, yhat) {
  mean(abs((y - yhat)/y))
}

mape <- MAPE(y1t,y1)

mape

mae <- MAE(y1t,y1)


