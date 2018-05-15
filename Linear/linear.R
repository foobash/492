# Linear Regression Model for Walmat Data.
# Richard Shea 
# Math 625.492 Project

# !!! MUST RUN 'function-project.R' FIRST!!!

# SET WORKING DIRCTORY
setwd("E:/personal/JHU/625.492/492_project/Time_Series_project/R") #work
#setwd("F:/personal/JHU/625.492/492_project/Time_Series_project/R")  #home


#time slicing for training data and test data

#Store 20 has the highest sales
#Store 33 has lowest sales

train <- data.frame(read.csv("Train_Store_20.csv"))  # create data frame

test <- data.frame(read.csv("Test_Store_20.csv")) 

min <- min(train$rWeekly_Sales) #min of min/max normalization
max <- max(train$Weekly_Sales) #max of min/max normalization 

store <- train$Store[2]  # grab store number for plotting

library(astsa)


# create time series object with dates

train.ts = ts(data=train$nWeekly_sales, frequency = 52, # 52 weeks a year.
             start=c(2010,5))


test.ts = ts(data=test$Weekly_Sales, frequency = 52,
              start=c(2012,3)) 

length(train.ts)  # check for number of weeks.  

#[1] 108

# fit data to liner model
fit_train.ts <- lm(train.ts~time(train.ts)) # Regress sales on time.

summary(fit_train.ts)

# Call:
#   lm(formula = train.ts ~ time(train.ts))
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -666594 -194773     493  126451 1403832 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -178151405  110501839  -1.612    0.110
# time(train.ts)      89284      54949   1.625    0.108
# 
# Residual standard error: 286900 on 94 degrees of freedom
# Multiple R-squared:  0.02732,	Adjusted R-squared:  0.01697 
# F-statistic:  2.64 on 1 and 94 DF,  p-value: 0.1075


summary(aov(fit_train.ts))  

# Df Sum Sq Mean Sq F value Pr(>F)  
# time(train.ts)   1 0.0693 0.06927   3.769 0.0549 .
# Residuals      106 1.9479 0.01838                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

h = 3 # set forecast horizon here!!!

#Plot our forecast
forecast <- sarima.for(train.ts, h, 1, 0, 0) #forecast out 3 weeks at 1 parameter
abline(fit_train.ts, col="blue")
title(main = toString(c("Store",(store))))

# forecast$pred[1:h]
# [1] 1357479 1369957 1372956

store_forecast <- forecast$pred[1:h] # this stores as row vector

#> store_forecast
# [1] 0.3223909 0.3283635 0.3297989

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Metrics for base forecast.

y_hat = con2back(store_forecast) # covert back estimated forecast

# > y_hat
# [1] 1357478 1369957 1372956

yt = test.ts[1:h] #true sales.  

# > yt
# [1] 1619602 1423172 1538238

mape <- MAPE(y_hat,yt)


cat("\n","MAPE for" ,h, "week horizon =", mape)

#MAPE for 3 week horizon = 0.1174411

mae <- MAE(y_hat,yt)

cat("\n","MAE for" ,h, "week horizon =", mae)

# MAE for 3 week horizon = 160206.5



# Begin Plots ####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Plot our time series
plot(train.ts, ylab="Weekly Sales", main = c("Store",toString(store)))

trainr.ts = ts(data=train$rWeekly_Sales, frequency = 52,
   start=c(2010,5))

plot(trainr.ts, ylab="Weekly Sales", main = c("Store",toString(store)))


tsplot(train.ts, ylab="Weekly Sales", main = c("Store",toString(store))) # <-- liking tsplot better
abline(fit_train.ts, col = "blue") # add the fitted regression line to the plot

#plot stats
qqnorm(train.ts, main = c("Store",toString(store)))  #QQ Normal PLots
qqline(train.ts, col = "blue")  # with QQ Line
acf(train.ts,main = c("Store",toString(store))) #Auto Corr. Function
pacf(train.ts,main = c("Store",toString(store))) # Partial ACF

#main model
mod <- lm(train.ts ~1)

# Stats
  
AIC(mod)

BIC(mod)
#[1] -114.0315


