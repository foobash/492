# HMM for Walmat Data.
# Richard Shea 
# Math 625.492 Project
# Find and plot state probabilities  

# SET WORKING DIRCTORY

setwd("F:/personal/JHU/625.492/492_project/Time_Series_project/R")  #home


library(depmixS4)
library(ggplot2)
library(reshape2)

#Sys.setenv(tz = "UTC")
train <- read.csv("Train_Store_20.csv")
train$Date = as.Date(train$Date)

for ( i in 2:5){
  set.seed(1)
  fmx <- fit(depmix(nWeekly_sales ~ 1, family = gaussian(), nstates = i, data = train), verbose = FALSE)
  summary(fmx)
  print(fmx)
  
}


# Take i=5 an example.. The last one
probs <- posterior(fmx)             # Compute probability of being in each state
head(probs)

# Lets change the name
colnames(probs)[2:6] <- paste("P",1:5, sep="-")

# Create dta.frame
dfu <- cbind(train[,c(2,5)], probs[,2:6])

# melt format
dfu <- melt(dfu,id="Date", )


# Working Plot code...Run in Console
qplot(Date,value,data=dfu,geom="line", group = 1,
      #main = paste("States", paste(names(stts), stts, collapse=": "), collapse="; "),
      main = paste("Normalized Sales with 5 states"),
      ylab = "State Probabilities") + 
  facet_grid(variable ~ ., scales="free_y") + theme_bw() 
