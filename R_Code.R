#Importing the libraries
library(raster)
library(tseries)
library(forecast)
library(ggplot2)

## Importing the Data
Global_superstore <- read.csv("Global Superstore.csv", header = T, stringsAsFactors = F);
View(Global_superstore)
str(Global_superstore)

##Checking NA
sapply(Global_superstore, function(x) sum(is.na(x)))
##Postal.code has 41296 Missing Values, let us replace this with Not Available

Global_superstore$Postal.Code[which(is.na(Global_superstore$Postal.Code))]<-"Not Available"

##Checking NA
sapply(Global_superstore, function(x) sum(is.na(x)))

#Checking Duplicates
nrow(unique(Global_superstore))
nrow(Global_superstore)

#Total rows -> 51290 and none of them are duplicate

##Checking Unique Segments
unique(Global_superstore$Segment)
#"Consumer" "Corporate" "Home Office"

##Checking Unique Markets
unique(Global_superstore$Market)
#US APAC EU Africa EMEA LATAM Canada

#Changing the Order.Date column to Date
Global_superstore$Order.Date <- as.Date(Global_superstore$Order.Date, format = "%d-%m-%Y");
Global_superstore$Order.Date <- format(Global_superstore$Order.Date,"%m-%Y")

str(Global_superstore$Order.Date)

Global_superstore$Segment <- as.factor(Global_superstore$Segment)
Global_superstore$Market <- as.factor(Global_superstore$Market)

### Subsets ###

##Creating a list having dataframes for different segments and markets combination
Superstore_subset <- list()

Superstore_Market <- unique(Global_superstore$Market)
Superstore_Segment <- unique(Global_superstore$Segment)

str(Superstore_Market)
str(Superstore_Segment)

##Creating different dataframes for different markets and segment
k <- 1

for (i in 1:length(Superstore_Market))
{
  for(j in 1:length(Superstore_Segment))
  {
    Superstore_subset[[k]] <- subset(Global_superstore,Global_superstore$Market == Superstore_Market[i]
                                     & Global_superstore$Segment == Superstore_Segment[j] )
    k <- k + 1
    
  }
}

## Aggregation

## Creating the list of dataframes which are aggregated at the Market, Segment and month level
SuperStore_subset_aggregate <- list()

for(i in 1:length(Superstore_subset[]))
{ SuperStore_subset_aggregate[[i]] <- aggregate(list(Sales=Superstore_subset[[i]]$Sales,Quantity=Superstore_subset[[i]]$Quantity,Profit=Superstore_subset[[i]]$Profit),
                                                list(Date=Superstore_subset[[i]]$Order.Date,Market=Superstore_subset[[i]]$Market,Segment=Superstore_subset[[i]]$Segment), sum)
}

##### Metric: Coe-efficient of variation
for(i in 1:length(Superstore_subset[]))
{
  profit_CV <- cv(SuperStore_subset_aggregate[[i]]$Profit)
  print(unique(SuperStore_subset_aggregate[[i]]$Market))
  print(unique(SuperStore_subset_aggregate[[i]]$Segment))
  print(i)
  print("___")
  print(profit_CV)
  print("END")
}

## Least COV is EU Consumer - 62.4,index-7 and APAC Consumer- 63.2, index 4

## _1 series denotes EU-Consumer segment & _2 series denotes APAC-Consumer series
## Most consitently profitables time series
Superstore_final_1 <- as.data.frame(SuperStore_subset_aggregate[[7]])
Superstore_final_2 <- as.data.frame(SuperStore_subset_aggregate[[4]])

summary(Superstore_final_1)
summary(Superstore_final_2)

#Taking indata for model building for the 1st 42 months, outdata for testing which would be last 6 months

indata_1 <- Superstore_final_1[1:42,]
outdata_1 <- Superstore_final_1[43:48,]

indata_2 <- Superstore_final_2[1:42,]
outdata_2 <- Superstore_final_2[43:48,]

#Full Time series for sales and quantity

total_timeser_sales_1 <- ts(Superstore_final_1$Sales)
total_timeser_sales_2 <- ts(Superstore_final_2$Sales)

total_timeser_Quantity_1 <- ts(Superstore_final_1$Quantity)
total_timeser_Quantity_2 <- ts(Superstore_final_2$Quantity)

timeser_sales_1 <- ts(indata_1$Sales)
plot(timeser_sales_1)
timeser_sales_2 <- ts(indata_2$Sales)
plot(timeser_sales_2)

timeser_Quantity_1 <- ts(indata_1$Quantity)
plot(timeser_Quantity_1)
timeser_Quantity_2 <- ts(indata_2$Quantity)
plot(timeser_Quantity_2)

## Auto Arima fitting

#Auto Arima for Sales_1
autoarima_sales_1 <- auto.arima(timeser_sales_1)
autoarima_sales_1
tsdiag(autoarima_sales_1)
plot(autoarima_sales_1$x, col="black")
lines(fitted(autoarima_sales_1), col="red")

#Auto Arima for Sales_2
autoarima_sales_2 <- auto.arima(timeser_sales_2)
autoarima_sales_2
plot(autoarima_sales_2$x, col="black")
lines(fitted(autoarima_sales_2), col="red")

#Auto Arima for Quantity_1
autoarima_quantity_1 <- auto.arima(timeser_Quantity_1)
autoarima_quantity_1
plot(autoarima_quantity_1$x, col="black")
lines(fitted(autoarima_quantity_1), col="red")

#Auto Arima for Quantity_2
autoarima_quantity_2 <- auto.arima(timeser_Quantity_2)
autoarima_quantity_2
plot(autoarima_quantity_2$x, col="black")
lines(fitted(autoarima_quantity_2), col="red")

#Again, let's check if the residual series is white noise

#Subtracting the Quantity_1 fitted using Auto Arima from the original time series to get the residual time series
resi_autoarima_quantity_1 <- timeser_Quantity_1 - fitted(autoarima_quantity_1)

#Plotting the residual series and checking the acf
plot(resi_autoarima_quantity_1)
acf(resi_autoarima_quantity_1)

#Performing the ADF and KPSS tests to check if the residual series is stationary or not 
adf.test(resi_autoarima_quantity_1,alternative = "stationary") ## p= .3592 # Cannot reject null # Series not stationary
kpss.test(resi_autoarima_quantity_1) ## p-value .1 cannot reject Null. Series is stationary

#Subtracting the Quantity_2 fitted using Auto Arima from the original time series to get the residual time series
resi_autoarima_quantity_2 <- timeser_Quantity_2 - fitted(autoarima_quantity_2)

#Plotting the residual series and checking the acf
plot(resi_autoarima_quantity_2)
acf(resi_autoarima_quantity_2)

#Performing the ADF and KPSS tests to check if the residual series is stationary or not 
adf.test(resi_autoarima_quantity_2,alternative = "stationary") ## p= .01 # Can reject null # Series is stationary
kpss.test(resi_autoarima_quantity_2) ## p-value .1 cannot reject Null. Series is stationary

#Subtracting the Sales_1 fitted using Auto Arima from the original time series to get the residual time series
resi_autoarima_sales_1 <- timeser_sales_1 - fitted(autoarima_sales_1)

#Plotting the residual series and checking the acf
plot(resi_autoarima_sales_1)
acf(resi_autoarima_sales_1)

#Performing the ADF and KPSS tests to check if the residual series is stationary or not 
adf.test(resi_autoarima_sales_1,alternative = "stationary") ## p= .2104 # Cannot reject null # Series not stationary
kpss.test(resi_autoarima_sales_1) ## p-value .1 cannot reject Null. Series is stationary

#Subtracting the Sales_2 fitted using Auto Arima from the original time series to get the residual time series
resi_autoarima_sales_2 <- timeser_sales_2 - fitted(autoarima_sales_2)

#Plotting the residual series and checking the acf
plot(resi_autoarima_sales_2)
acf(resi_autoarima_sales_2)

#Performing the ADF and KPSS tests to check if the residual series is stationary or not 
adf.test(resi_autoarima_sales_2,alternative = "stationary") ## p= .01 # Can reject null # Series is stationary
kpss.test(resi_autoarima_sales_2) ## p-value .1 cannot reject Null. Series is stationary


## Prediction
in_data_pred_quantity_1 <-fitted(autoarima_quantity_1)
plot(autoarima_quantity_1$x, col="black")
lines(in_data_pred_quantity_1, col="red")

#Let's evaluate the Quantity _1 model using MAPE

fcast_autoarima_quantity_1 <- predict(autoarima_quantity_1, n.ahead = 6)

MAPE_auto_arima_quantity_1 <- accuracy(fcast_autoarima_quantity_1$pred,outdata_1$Quantity)[5]
MAPE_auto_arima_quantity_1
## MAPE is 30%

auto_arimapred_quantity_1 <- c(fitted(autoarima_quantity_1),ts(fcast_autoarima_quantity_1$pred))
plot(total_timeser_Quantity_1,col = "black")
lines(auto_arimapred_quantity_1, col = "red")

in_data_pred_quantity_1 <-fitted(autoarima_quantity_1)
plot(autoarima_quantity_1$x, col="black")
lines(in_data_pred_quantity_1, col="red")

#Let's evaluate the Quantity _2 model using MAPE

fcast_autoarima_quantity_2 <- predict(autoarima_quantity_2, n.ahead = 6)
MAPE_auto_arima_quantity_2 <- accuracy(fcast_autoarima_quantity_2$pred,outdata_2$Quantity)[5]
MAPE_auto_arima_quantity_2
## MAPE is 23.4%

auto_arimapred_quantity_2 <- c(fitted(autoarima_quantity_2),ts(fcast_autoarima_quantity_2$pred))
plot(total_timeser_Quantity_2,col = "black")
lines(auto_arimapred_quantity_2, col = "red")

#Let's evaluate the Sales _1 model using MAPE

fcast_autoarima_sales_1 <- predict(autoarima_sales_1, n.ahead = 6)
MAPE_auto_arima_sales_1 <- accuracy(fcast_autoarima_sales_1$pred,outdata_1$Sales)[5]
MAPE_auto_arima_sales_1
## MAPE is 33.18%

auto_arimapred_quantity_1 <- c(fitted(autoarima_sales_1),ts(fcast_autoarima_sales_1$pred))
plot(total_timeser_sales_1,col = "black")
lines(auto_arimapred_quantity_1, col = "red")

#Let's evaluate the Sales _2 model using MAPE

fcast_autoarima_sales_2 <- predict(autoarima_sales_2, n.ahead = 6)
MAPE_auto_arima_sales_2 <- accuracy(fcast_autoarima_sales_2$pred,outdata_2$Sales)[5]
MAPE_auto_arima_sales_2
## MAPE is 17.8%

auto_arimapred_quantity_2 <- c(fitted(autoarima_sales_2),ts(fcast_autoarima_sales_2$pred))
plot(total_timeser_sales_2,col = "black")
lines(auto_arimapred_quantity_2, col = "red")

########Classical Decomposition

#### Sales1

##Obtaining the smoothed series for Sales_1
w <-1
smoothedseries_sales_1 <- filter(timeser_sales_1,
                                 rep(1/(2*w+1),(2*w+1)),
                                 method='convolution', sides=2)

#Smoothing left end of the time series
diff_sales_1 <- smoothedseries_sales_1[w+2] - smoothedseries_sales_1[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_sales_1[i] <- smoothedseries_sales_1[i+1] - diff_sales_1
}

#Smoothing right end of the time series
plot(smoothedseries_sales_1)
n <- length(timeser_sales_1)
diff_sales1 <- smoothedseries_sales_1[n-w] - smoothedseries_sales_1[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_sales_1[i] <- smoothedseries_sales_1[i-1] + diff_sales1
}

#Plot the smoothed time series

timevals_in_sales_1 <- indata_1$Date
lines(smoothedseries_sales_1, col="blue", lwd=2)

########Holtwinter
#plot(timeser_sales_1, main='title', xlab = 'xlab1', ylab = 'ylab1')

#cols <- c("red","blue", "black")
#alphas <- c( 0.9,0.8)
#labels <- c(paste("alpha =", alphas), "Original")
#for (i in seq(1,length(alphas))) {
#  smootheddf_sales_2 <- HoltWinters(timeser_sales_1, alpha=alphas[i],
#                                    beta=FALSE, gamma=FALSE)
#  
#  lines(fitted(smootheddf_sales_1)[,1], col=cols[i], lwd=2)
#}

#legend("bottomleft", labels, col=cols, lwd=2)
###### END Holtwinter


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_sales_1 <- as.data.frame(cbind(c(1:42), as.vector(smoothedseries_sales_1)))
colnames(smootheddf_sales_1) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_sales_1 <- lm(Sales ~ sin(0.5*Month)* poly(Month,3) + cos(0.5*Month)*poly(Month,3)
                    + Month, data=smootheddf_sales_1)
global_pred_sales_1 <- predict(lmfit_sales_1, Month=c(1:42))
summary(global_pred_sales_1)
lines(c(1:42), global_pred_sales_1, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_sales_1 <- timeser_sales_1-global_pred_sales_1
plot(local_pred_sales_1, col='red', type = "l")
acf(local_pred_sales_1)
acf(local_pred_sales_1, type="partial")
armafit_sales_1 <- auto.arima(local_pred_sales_1)

tsdiag(armafit_sales_1)
armafit_sales_1

## No Auto regressive component is present 
#We'll check if the residual series is white noise

resi_sales_1 <- local_pred_sales_1 - fitted(armafit_sales_1)
adf.test(resi_sales_1,alternative = "stationary")# p-value 0.01 < 0.05 we reject it and the series is stationary
kpss.test(resi_sales_1)# p-value 0.1 > 0.05 we accept it and the series is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- c(42:48)

global_pred_out_sales_1 <- predict(lmfit_sales_1,data.frame(Month =timevals_out))
fcast_sales_1 <- global_pred_out_sales_1

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_sales_1 <- accuracy(fcast_sales_1,outdata_1$Sales)[5]
MAPE_class_dec_sales_1
## Mape Sales_1 is 25.59%

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_sales_1 <- c(ts(global_pred_sales_1),ts(global_pred_out_sales_1))
plot(total_timeser_sales_1, col = "black")
lines(class_dec_pred_sales_1, col = "red")

###################################################
### Sales 2

w <-1
smoothedseries_sales_2 <- filter(timeser_sales_2,
                                 rep(1/(2*w+1),(2*w+1)),
                                 method='convolution', sides=2)
#Smoothing left end of the time series

diff_sales_2 <- smoothedseries_sales_2[w+2] - smoothedseries_sales_2[w+1]
for (i in seq(w,1,-1)) 
{
  smoothedseries_sales_2[i] <- smoothedseries_sales_2[i+1] - diff_sales_2
}

#Smoothing right end of the time series
plot(smoothedseries_sales_2)
n <- length(timeser_sales_2)
diff_sales2 <- smoothedseries_sales_2[n-w] - smoothedseries_sales_2[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_sales_2[i] <- smoothedseries_sales_2[i-1] + diff_sales2
}

#Plot the smoothed time series

timevals_in_sales_2 <- indata_2$Date
lines(smoothedseries_sales_2, col="blue", lwd=2)

########Holtwinter
#plot(timeser_sales_2, main='title', xlab = 'xlab1', ylab = 'ylab1')

#cols <- c("red","blue", "black")
#alphas1 <- c( 0.9,0.8)
#labels <- c(paste("alpha =", alphas1), "Original")
#for (i in seq(1,length(alphas1))) {
#  smootheddf_sales_2 <- HoltWinters(timeser_sales_2, alpha=alphas1[i],
#                                    beta=FALSE, gamma=FALSE)
#  
#  lines(fitted(smootheddf_sales_2)[,1], col=cols[i], lwd=2)
#}

#legend("bottomleft", labels, col=cols, lwd=2)
###### END Holtwinter



#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_sales_2 <- as.data.frame(cbind(c(1:42), as.vector(smoothedseries_sales_2)))
colnames(smootheddf_sales_2) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_sales_2 <- lm(Sales ~ sin(0.5*Month)* poly(Month,3) + cos(0.5*Month)* poly(Month,3)
                    + Month, data=smootheddf_sales_2)
global_pred_sales_2 <- predict(lmfit_sales_2, Month=c(1:42))
summary(global_pred_sales_2)
lines(c(1:42), global_pred_sales_2, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_sales_2 <- timeser_sales_2-global_pred_sales_2
plot(local_pred_sales_2, col='red', type = "l")
acf(local_pred_sales_2)
acf(local_pred_sales_2, type="partial")
armafit_sales_2 <- auto.arima(local_pred_sales_2)
tsdiag(armafit_sales_2)
armafit_sales_2
# No AR part in the local_series

#We'll check if the residual series is white noise
resi_sales_2 <- local_pred_sales_2-fitted(armafit_sales_2)

adf.test(resi_sales_2,alternative = "stationary")# p-value 0.01 < 0.05 we reject it and the series is stationary
kpss.test(resi_sales_2)# p-value 0.1 > 0.05 we accept it and the series is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- c(42:48)

global_pred_out_sales_2 <- predict(lmfit_sales_2,data.frame(Month =timevals_out))

fcast_sales_2 <- global_pred_out_sales_2

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_sales_2 <- accuracy(fcast_sales_2,outdata_2$Sales)[5]
MAPE_class_dec_sales_2

#Sales_2 MAPE is 52%

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_sales_2 <- c(ts(global_pred_sales_2),ts(global_pred_out_sales_2))
plot(total_timeser_sales_2, col = "black")
lines(class_dec_pred_sales_2, col = "red")

####################################################

#QUANTITY_1

w <-1
smoothedseries_quantity_1 <- filter(timeser_Quantity_1,
                                    rep(1/(2*w+1),(2*w+1)),
                                    method='convolution', sides=2)
#Smoothing left end of the time series

diff_quantity_1 <- smoothedseries_quantity_1[w+2] - smoothedseries_quantity_1[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_quantity_1[i] <- smoothedseries_quantity_1[i+1] - diff_quantity_1
}

#Smoothing right end of the time series
plot(smoothedseries_quantity_1)
n <- length(timeser_Quantity_1)
diff_quantity_1 <- smoothedseries_quantity_1[n-w] - smoothedseries_quantity_1[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_quantity_1[i] <- smoothedseries_quantity_1[i-1] + diff_quantity_1
}

#Plot the smoothed time series

timevals_in_quantity_1 <- indata_1$Date
lines(smoothedseries_quantity_1, col="blue", lwd=2)

######## HW 
#plot(timeser_Quantity_1, main='title', xlab = 'xlab1', ylab = 'ylab1')

#cols <- c("red", "blue", "green", "black")
#alphas <- c(0.02, 0.1, 0.8)
#labels <- c(paste("alpha =", alphas), "Original")
#for (i in seq(1,length(alphas))) {
#  smootheddf_quantity_1 <- HoltWinters(timeser_Quantity_1, alpha=alphas[i],
#                                    beta=FALSE, gamma=FALSE)

#  lines(fitted(smootheddf_quantity_1)[,1], col=cols[i], lwd=2)
#}

#legend("bottomleft", labels, col=cols, lwd=2)
##### END HW

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_quantity_1 <- as.data.frame(cbind(c(1:42), as.vector(smoothedseries_quantity_1)))
colnames(smootheddf_quantity_1) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_quantity_1 <- lm(Quantity ~ sin(0.5*Month) *poly(Month,3) + cos(0.5*Month)* poly(Month,3)
                       + Month, data=smootheddf_quantity_1)
global_pred_quantity_1 <- predict(lmfit_quantity_1, Month=c(1:42))
summary(global_pred_quantity_1)
lines(c(1:42), global_pred_quantity_1, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_quantity_1 <- timeser_Quantity_1 - global_pred_quantity_1
plot(local_pred_quantity_1, col='red', type = "l")
acf(local_pred_quantity_1)
acf(local_pred_quantity_1, type="partial")
armafit_quantity_1 <- auto.arima(local_pred_quantity_1)

tsdiag(armafit_quantity_1)
armafit_quantity_1
# NO more AR component in the local_predictor

#We'll check if the residual series is white noise

resi_quantity_1 <- local_pred_quantity_1-fitted(armafit_quantity_1)

adf.test(resi_quantity_1,alternative = "stationary")# p-value 0.01 < 0.05 we reject it and the series is stationary
kpss.test(resi_quantity_1)# p-value 0.1 > 0.05 we accept it and the series is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- c(42:48)
global_pred_out_quantity_1 <- predict(lmfit_quantity_1,data.frame(Month =timevals_out))
fcast_quantity_1 <- global_pred_out_quantity_1

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_quantity_1 <- accuracy(fcast_quantity_1,outdata_1$Quantity)[5]
MAPE_class_dec_quantity_1

#MAPE quantity_1 is 32.55%
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_quantity_1 <- c(ts(global_pred_quantity_1),ts(global_pred_out_quantity_1))
plot(total_timeser_Quantity_1, col = "black")
lines(class_dec_pred_quantity_1, col = "red")


######################################
#QUANTITY_2
w <-1
smoothedseries_quantity_2 <- filter(timeser_Quantity_2,
                                    rep(1/(2*w+1),(2*w+1)),
                                    method='convolution', sides=2)
#Smoothing left end of the time series

diff_quantity_2 <- smoothedseries_quantity_2[w+2] - smoothedseries_quantity_2[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_quantity_2[i] <- smoothedseries_quantity_2[i+1] - diff_quantity_2
}

#Smoothing right end of the time series
plot(smoothedseries_quantity_2)
n <- length(timeser_Quantity_2)
diff_quantity_2 <- smoothedseries_quantity_2[n-w] - smoothedseries_quantity_2[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_quantity_2[i] <- smoothedseries_quantity_2[i-1] + diff_quantity_2
}

#Plot the smoothed time series

timevals_in_quantity_2 <- indata_2$Date
lines(smoothedseries_quantity_2, col="blue", lwd=2)

######## HW 
#plot(timeser_Quantity_2, main='title', xlab = 'xlab1', ylab = 'ylab1')

#cols <- c("red", "blue", "green", "black")
#alphas <- c(0.02, 0.1, 0.8)
#labels <- c(paste("alpha =", alphas), "Original")
#for (i in seq(1,length(alphas))) {
#  smootheddf_quantity_2 <- HoltWinters(timeser_Quantity_2, alpha=alphas[i],
#                                    beta=FALSE, gamma=FALSE)

#  lines(fitted(smootheddf_quantity_2)[,1], col=cols[i], lwd=2)
#}

#legend("bottomleft", labels, col=cols, lwd=2)
##### END HW


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_quantity_2 <- as.data.frame(cbind(c(1:42), as.vector(smoothedseries_quantity_2)))
colnames(smootheddf_quantity_2) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_quantity_2 <- lm(Quantity ~ sin(0.5*Month) *poly(Month,3) + cos(0.5*Month)* poly(Month,3)
                       + Month, data=smootheddf_quantity_2)
global_pred_quantity_2 <- predict(lmfit_quantity_2, Month=c(1:42))
summary(global_pred_quantity_2)
lines(c(1:42), global_pred_quantity_2, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_quantity_2 <- timeser_Quantity_2 - global_pred_quantity_2
plot(local_pred_quantity_2, col='red', type = "l")
acf(local_pred_quantity_2)
acf(local_pred_quantity_2, type="partial")
armafit_quantity_2 <- auto.arima(local_pred_quantity_2)

tsdiag(armafit_quantity_2)
armafit_quantity_2
# NO more AR component in the local_predictor

#We'll check if the residual series is white noise

resi_quantity_2 <- local_pred_quantity_2-fitted(armafit_quantity_2)

adf.test(resi_quantity_2,alternative = "stationary")# p-value 0.01 < 0.05 we reject it and the series is stationary
kpss.test(resi_quantity_2)# p-value 0.1 > 0.05 we accept it and the series is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- c(42:48)
global_pred_out_quantity_2 <- predict(lmfit_quantity_2,data.frame(Month =timevals_out))
fcast_quantity_2 <- global_pred_out_quantity_2

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_quantity_2 <- accuracy(fcast_quantity_2,outdata_2$Quantity)[5]
MAPE_class_dec_quantity_2

#MAPE quantity_2 is 25.53%
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_quantity_2 <- c(ts(global_pred_quantity_2),ts(global_pred_out_quantity_2))
plot(total_timeser_Quantity_2, col = "black")
lines(class_dec_pred_quantity_2, col = "red")

###########Final MAPE values of all the models ###########

########## Classical Decomposition #######
#Quantity
## QUantity_1 which is segment EU-Consumer  model have MAPE value 32.5%
## QUantity_2 which is segment APAC-Consumer model have MAPE value 25.5% 
#Sales
## Sales_1 which is segment EU-Consumer model have MAPE value 25.5%
## Sales_2 which is segment APAC-Consumer model have MAPE value 52%
########## AUTO ARIMA  #######
#Quantity
## QUantity_1 which is segment EU-Consumer  model have MAPE value 30%
## QUantity_2 which is segment APAC-Consumer model have MAPE value 23.4%
#Sales
## Sales_1 which is segment EU-Consumer model have MAPE value 33.1%
## Sales_2 which is segment APAC-Consumer model have MAPE value 17.8%

######### SMOOTHING METHOD #######
## Moving Average and Holtwinter both methods were giving same MAPE value so we used moving average method

###############################################################################################
## THus, best model for EU-COnsumer quantity is auto.arima model with 30% MAPE               ##
## Best models are for EU-COnsumer sales is Classical Decomposition model with 25.1% MAPE    ##
## Best Model for APAC-Consumer quantity is AutoA.Arima with MAPE of 23.4%                   ##  
## Best Model for APAC-Consumer sales is AutoA.Arima with MAPE of 17.8%                      ##
###############################################################################################
