# Markov Chain Model for Walmat Data.
# Richard Shea 
# Math 625.492 Project

# !!! MUST RUN 'function-project.R' FIRST!!!

# SET WORKING DIRCTORY

setwd("F:/personal/JHU/625.492/492_project/Time_Series_project/R")  #home

#load libraries 
library(markovchain)
library(diagram)
library(expm)
library(dplyr)


# Pick store to train/test here %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
train <- data.frame(read.csv("Train_Store_33.csv"))  # create data frame

test <- data.frame(read.csv("Test_Store_33.csv")) 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m = median(train$Weekly_Sales)

# add 2-state data where either low sales or high sales relative to median
train = within(train, {
  state = ifelse(Weekly_Sales <= m, 0, 1)
})

# Grab high sales
ones <- filter(train, state ==1)
mu_high <- mean(ones$Weekly_Sales) # Average of high sales

# Grab low sales
zeroes <- filter(train, state ==0)
mu_low <- mean(zeroes$Weekly_Sales) # Average of low sales


mysequence<-train$state

# [1] 1 1 1 1 1 0 0 0 1 0 0 0 1 1 0 0 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0
# [43] 0 1 1 1 1 0 0 0 0 0 1 0 1 0 1 1 0 0 0 1 1 1 0 1 1 1 0 1 1 0 1 1 0 1 0 0 0 0 1 0 1 0
# [85] 1 0 1 1 0 0 0 1 1 0 1 1 1 1 1 1 0 0 1 0 1 1 1 0

createSequenceMatrix(mysequence)

#   0  1
# 0 31 22
# 1 23 31

# It can be shown that this is equivalent to the maximum likelihood estimate of
# the transition probability matrix (see p. 21 in Zucchini09 for the proof).

myFit<-markovchainFit(data=mysequence,confidencelevel = .9,method = "mle")

# > myFit
# $estimate
# 0         1
# 0 0.5849057 0.4150943
# 1 0.4259259 0.5740741
# 
# 
# $standardError
# 0          1
# 0 0.10505216 0.08849841
# 1 0.08881169 0.10310675
# 
# $confidenceLevel
# [1] 0.9
# 
# $lowerEndpointMatrix
# 0         1
# 0 0.4502759 0.3016791
# 1 0.3121092 0.4419375
# 
# $upperEndpointMatrix
# 0         1
# 0 0.7195354 0.5285096
# 1 0.5397427 0.7062107
# 
# $logLikelihood
# [1] -72.8041

# We now store our Estimate

alofiMc<-myFit$estimate

# > alofiMc
# MLE Fit 
# A  2 - dimensional discrete Markov Chain defined by the following states: 
#   0, 1 
# The transition matrix  (by rows)  is defined as follows: 
#   0         1
# 0 0.5849057 0.4150943
# 1 0.4259259 0.5740741

#set up our Graph

a11=alofiMc[1,1]
a12=alofiMc[1,2]
a21=alofiMc[2,1]
a22=alofiMc[2,2]

## Hard code the transition matrix
stateNames <- c("Low Sale","High Sale")

ra <- matrix(c(a11,a12,a21,a22),nrow=2, byrow=TRUE)

dtmcA <- new("markovchain",transitionMatrix=ra, states=c("Low Sale","High Sale"), name="MarkovChain A")

# > dtmcA

# MarkovChain A 
# A  2 - dimensional discrete Markov Chain defined by the following states: 
#   Low Sale, High Sale 
# The transition matrix  (by rows)  is defined as follows: 
#   Low Sale High Sale
# Low Sale  0.5849057 0.4150943
# High Sale 0.4259259 0.5740741

# basic plot of graph
plot(dtmcA,edge.arrow.size=0.2,arr.length=.07)  #!!! Need to make arrows less length

#basic plot of plotmat
row.names(ra) <- stateNames; colnames(ra) <- stateNames
ra = round(ra,2)
plotmat(ra,pos = c(1,1), #c(1,1) set for 2x2; set to c(1,2) for 3x3. 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.5,
        box.col = "light blue",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        main = "Markov Chain Transition Matrix")

#low sale this week...

x1 <- matrix(c(1,0),nrow=1, byrow=TRUE)

# predict for next week

x1 %*% ra

# > x1 %*% ra
# Low Sale High Sale
# [1,]     0.58       0.42

#Forecast the states for the next 7 weeks, but will only use 3.

ra2 <- ra %^% 2
ra3 <- ra %^% 3
ra4 <- ra %^% 4
ra5 <- ra %^% 5
ra6 <- ra %^% 6
ra7 <- ra %^% 7

cat("Week 1 Forecast")

round(x1%*%ra,3)

# > round(x1%*%ra,3)
#       Low Sale  High Sale
# [1,]     0.58      0.42

cat("Week 2 Forecast")

#Week 2 Forecast

round(x1%*%ra2,3)


# > round(x1%*%ra2,3)
# Low Sale High Sale
# [1,]    0.517     0.483

cat("Week 3 Forecast")

#Week 3 Forecast

round(x1%*%ra3,3)

# > round(x1%*%ra3,3)

# Low Sale High Sale
# [1,]    0.508     0.492


ra2=round(ra2,2)

plotmat(ra2,pos = c(1,1), #c(1,1) set for 2x2; set to c(1,2) for 3x3.
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.8, 
        box.size = 0.1, 
        box.type = "circle", 
        box.prop = 0.15, #cirlce size.
        box.col = "light blue",
        arr.length=.10,
        arr.width=.10,
        self.cex = .2, # size of arc cirlces
        self.shifty = -.005,
        self.shiftx = .12,
        main = "Store 20 Transistion Matrix Week 2")

#Observe convergence

ra %^% 10000
# Low Sale High Sale
# Low Sale  0.5058824 0.4941176
# High Sale 0.5058824 0.4941176
ra %^% 1000000
# Low Sale High Sale
# Low Sale  0.5058824 0.4941176
# High Sale 0.5058824 0.4941176

# Forecast
current_state <- train$state[108]
#isTRUE(train$Weekly_Sales[108] > m)

# Setup next state forecast vector

i = 3 # <---- set time[i], forecast horizon here.  
fa = ra%^%i
f11 <- fa[1,1]*mu_low + fa[1,2]*mu_high
f21 <- fa[2,1]*mu_low + fa[2,2]*mu_high

for_v <- t(t(c(f11,f21))) # Our forward vector forecast

for_v

# > for_v
#         [,1]
# [1,] 1340158
# [2,] 1402841

# Then use functions to run MAPE and MAE to copy and paste into Excel.

