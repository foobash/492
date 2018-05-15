# Markov Chain Model for Walmat Data.
# Richard Shea 
# Math 625.492 Project
# use this script to select number of states.  

setwd("F:/personal/JHU/625.492/492_project/Time_Series_project/R") #home

# using depmixs4.pdf

library("depmixS4")

x <- read.csv("Train_Store_20.csv")
#x <- data.matrix(x$Weekly_Sales)


#setup model object.  
mod <- depmix(response = nWeekly_sales ~ 1, data = x, nstates = 6, 
              trstart = runif(36)) # !!!!!trstart must be (nstates)^2!!!!!!!
# Now fit model.
fm <- fit(mod, emc=em.control(rand=TRUE)) 

