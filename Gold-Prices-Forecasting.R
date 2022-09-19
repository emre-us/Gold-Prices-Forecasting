#first check if you have the following packages, if not then these will be instaled
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tibble)) install.packages("tibble")
if(!require(tsibble)) install.packages("tsibble")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(feasts)) install.packages("feasts")
if(!require(forecast)) install.packages("forecast")
if(!require(reshape2)) install.packages("reshapre2")
if(!require(fable)) install.packages("fable")
if(!require(tinytex)) install.packages("tinytex")

#next load up these packages

#helper packages
library(tibble) #to be able to convert data frames to tibble objects
library(tsibble) #tibble objects for time series
library(tidyverse) #for tidy data
library(lubridate) #for dealing with dates
library(reshape2) #for reshaping data to long format-useful in plotting multiple time series in same axis
library(tinytex) #for knitting to pdf

#packages for plots and graphics
library(ggplot2) #for visualisation
library(ggrepel) #for ensuring labels don't overlap in plots
library(gridExtra) #for plotting graphs in a grid or next to each other
library(feasts) #to be able to use autoplot function with tsibble objects
library(forecast) #to be able to use autoplot function with ts objects

#packages for modelling
library(fable)


#Load up the data:
gold_df <- read.csv("~/R Projects/Gold-Prices-Forecasting/Gold Price.csv")

#Check structure of data:
str(gold_df)

#convert data frame to a tsibble object
gold_df <- gold_df %>%
  mutate(Date = ymd(Date)) %>% #convert Date column from text to daily object with ymd() or with as_date().
  as_tsibble(index = Date) #convert to tsibble by identifying the index

#check the first few rows of the new tsibble object
head(gold_df)

#use tsibble package's count_gaps function to see where the gaps are
count_gaps(gold_df)

#add weekends to the time series
gold_df <- gold_df %>%
  complete(Date = seq.Date(min(Date), max(Date), by="day")) %>% #format is: seq.Date(start date, end date, by = unit of Date)
  fill("Price", "Open", "High", "Low", "Volume", "Chg.") #without fill function, complete() above would only fill the dates but we would get NAs for the rest of the columns corresponding to those weekends. With fill() we copy the same values for each column by the immediately preceding value to the NA.

head(gold_df)

#convert data frame to a tsibble object
gold_df <- as_tsibble(gold_df)
#check if there are gaps in the time series
count_gaps(gold_df)

#check dimensions of the larger data frame
dim(gold_df)

#we can use autoplot() function which produces an appropriate plot of whatever is passed in the first argument

autoplot(gold_df, Price) + #autoplot recognises gold_df as a time series and produces a time plot
  labs(title = "Gold Mini Futures Daily Closing Prices",
       subtitle = "(INR) From 01.01.14 to 05.08.22")

#Plot the % change column
autoplot(gold_df, Chg.)

gg_season() function from feasts package will help with plotting time series against each season.
gg_season(gold_df, labels = "both") #seasonal periods can be changed by including period="month" command within gg_season function

#check seasonality of daily trading volume
gold_df %>% gg_season(Volume)

#check relationship between Volume and Price
gold_df %>%
  filter(year(Date) == 2020) %>%
  ggplot(aes(x = Volume, y = Price)) +
  geom_point()

#Split data to training and test sets. We can use filter() or slice() functions for this and I will use both here as a way of illustrating how each can be used. There is also ts_split() function from TSStudio package but I did not see the need of installing that package just for splitting this data. tsibble and fable packages provide all the things we need for this work.

#Test set with slice()
gold_validation_df <- gold_df %>%
  slice(n() -313:0) #since we have 3139 rows, 10% of data is roughly equivalent to the last 314 rows

#We can see the head of test set
head(gold_validation_df)

#which shows that the test set begins with 26 September 2021

#For training set we can use filter() function to see different syntax
gold_train_df <- gold_df %>%
  filter(Date < "2021-09-26")

#We can check that training set ends on 25th of Sep 2021
tail(gold_train_df)

#check dimensions of training and validation sets:
tibble(training_set = dim(gold_train_df),
       validation_set = dim(gold_validation_df))

#a simpler approach than using slice() or filter() functions is as follows:
gold_train2 <- gold_train_df[1:2260,]
gold_test <- gold_train_df[-(1:2260),]

#we can check the dimensions of our smaller training set and the test set:
tibble(training_set_2 = dim(gold_train2),
       test_set = dim(gold_test))

#use model() function from fable package to fit the model to training data
avg_price_fit <- gold_train2 %>%
  model(Average = MEAN(Price))

#Generate forecasts for the next 565 days
avg_fc <- avg_price_fit %>%
  forecast(h = 565)

#Plot forecasts against actual values
avg_fc %>%
  autoplot(gold_train2, level = NULL) + #add training set's plot to our forecast. Setting level to NULL means it will not give us the default 95% and 80% intervals.
  autolayer(gold_test, Price, colour = "grey") + #add the layer of actual values from the test set
  labs(title = "Base Line Forecast for Gold Mini Closing Price") #add title

#get RMSE and MAE accuracy of forecast
base_accuracy <- accuracy(avg_fc, gold_test)
base_accuracy

#as before we can use tibble to create our results table:
fc_accuracy_results <- tibble("Base: Just the Average", base_accuracy[,4:5]) %>%
  set_names(c("Method", "RMSE", "MAE"))
fc_accuracy_results

#use model() function from fable package to fit the model to training data

naive_fit <- gold_train2 %>%
  model(Average = NAIVE(Price)) #we can either use NAIVE() function or RW() function for random walk

#Generate forecasts for the next 565 days
naive_fc <- naive_fit %>%
  forecast(h = 565)

#Plot forecasts against actual values
naive_fc %>%
  autoplot(gold_train2, level = NULL) + #add training set's plot to our forecast. Setting level to NULL means it will not give us the default 95% and 80% intervals.
  autolayer(gold_test, Price, colour = "grey") + #add the layer of actual values from the test set
  labs(title = "Naive Forecast for Gold Mini Closing Price") #add title

#get RMSE and MAE accuracy of forecast
drift_accuracy <- accuracy(drift_fc, gold_test)
drift_accuracy

#add accuracy to the results table
fc_accuracy_results <- bind_rows(fc_accuracy_results,
                                 tibble(Method = "Naive with Drift",
                                        drift_accuracy[,4:5]))
fc_accuracy_results

#create many training sets for cross-validation:

drift_cv <- gold_train_df %>% #we begin with the larger training set
  stretch_tsibble(.init = 100, .step = 1) %>%  #we use stretch_tsibble() with an initial training set of 100, increasing the size of successive training sets by 1.
  relocate(Date, Price, .id) #stretching introduces a new column ".id" and mutates it as a last column. We reorder it so it becomes the 3rd column after Date and Price

#check the tail of the new tsibble object we created
tail(drift_cv)

#running the following code will take a few minutes. It may be advisable to get some tea during this time
drift_cv_fc_accuracy <- drift_cv %>%
  model(NAIVE(Price ~ drift())) %>%
  forecast(h = 565) %>%
  accuracy(gold_train_df)

drift_cv_fc_accuracy

fc_accuracy_results <- bind_rows(fc_accuracy_results,
                                 tibble(Method = "Cross_Validated Naive with Drift",
                                        drift_cv_fc_accuracy[,4:5]))
fc_accuracy_results

#We will limit the lags to a maximum of 7 days for illustrative purposes
gold_train_df %>% ACF(Price, lag_max = 7)

#compare ACF Plot of Price and differenced prices
gold_train_df %>%
  ACF(Price) %>%
  autoplot() +
  labs(subtitle = "Closing Price Autocorrelation Coefficients")

gold_train_df %>%
  ACF(difference(Price)) %>% #transform time series by differencing
  autoplot() +
  labs(subtitle = "Changes in Closing Price Autocorrelation Coefficients")

#apply Ljung-Box test
gold_train_df %>%
  mutate(diff_price = difference(Price)) %>% #add a new column with the differences to the tsibble object
  features(diff_price, ljung_box, lag = 7) #use features() function from the feast package to apply the ljung-box test to the new column we created with h=7

#check if differencing is necessary for stationarity
gold_train_df %>%
  features(Price, unitroot_kpss) #features and unitroot_kpss functions from feasts package are applied to the Price column of the training set

#do the same for the differenced data:
gold_train_df %>%
  mutate(diff_price = difference(Price)) %>% #add a new column with the differences to the tsibble object
  features(diff_price, unitroot_kpss) #features and unitroot_kpss functions from feasts package are applied to the differenced price column we created

#check how many differences are needed
gold_train_df %>%
  features(Price, unitroot_ndiffs)

#Use ARIMA() argument within model() function within the fable package
AR_fit <- gold_train2 %>%
  model(AR1 = ARIMA(Price ~ pdq(p = 1, d = 1, q= 0)),
        AR2 = ARIMA(Price ~ pdq(p = 2, d = 1, q= 0))) #ARIMA stands for Autoregressive Integrated Moving Average model. p refers to order for AR, and d refers to differencing we need for stationarity. We are looking at AR(2) model so p is either 1 or 2, and stationarity analysis confirmed that we need differencing of 1, so d=1. q relates to moving average, or MA, and since we are only looking at AR model, we keep q as 0.

#we then ask for the report of this fit:
report(AR_fit)

#fit AR(2) model
AR2_fit <- gold_train2 %>%
  model(ARIMA(Price ~ pdq(p = 2, d=1, q=0)))

report(AR2_fit)

#gg_residuals() function will give us the residual plots and ACF plot

AR2_fit %>%
  gg_tsresiduals()

#forecast using the AR(4) fitted model:
AR_fc <- AR2_fit %>%
  forecast(h=565)

AR_fc %>%
  autoplot(gold_train2, level = NULL) + #add training set's plot to our forecast. Setting level to NULL means it will not give us the default 95% and 80% intervals.
  autolayer(gold_test, Price, colour = "grey") + #add the layer of actual values from the test set
  labs(title = "AR(2) Forecast for Gold Mini Closing Price") #add title

AR_fc_accuracy <- accuracy(AR_fc, gold_test)
AR_fc_accuracy

#add the accuracy to our results table:
fc_accuracy_results <- bind_rows(fc_accuracy_results,
                                 tibble(Method = "AR(2)",
                                        AR_fc_accuracy[,4:5]))
fc_accuracy_results

#Use ARIMA() argument within model() function within the fable package
MA_fit <- gold_train2 %>%
  model(MA1 = ARIMA(Price ~ pdq(p=0, d=1, q=1)), #this time we will leave p as 0 and try q = 1 and q=2, with differencing the price data by 1 day, ie d=1.
        MA2 = ARIMA(Price ~ pdq(p=0, d=1, q=2)))

#we then ask for the report of this fit:
report(MA_fit)

#model MA(2)
MA2_fit <- gold_train2 %>%
  model(ARIMA(Price ~ pdq(p=0, d=1, q=2)))

report(MA2_fit)

#gg_residuals() function will give us the residual plots and ACF plot
MA2_fit %>%
  gg_tsresiduals()

#forecast using the AR(4) fitted model:
MA_fc <- MA2_fit %>%
  forecast(h=565)

MA_fc %>%
  autoplot(gold_train2, level = NULL) + #add training set's plot to our forecast. Setting level to NULL means it will not give us the default 95% and 80% intervals.
  autolayer(gold_test, Price, colour = "grey") + #add the layer of actual values from the test set
  labs(title = "MA(2) Forecast for Gold Mini Closing Price") #add title

MA_fc_accuracy <- accuracy(MA_fc, gold_test)
MA_fc_accuracy

fc_accuracy_results <- bind_rows(fc_accuracy_results,
                                 tibble(Method = "MA(2)",
                                        MA_fc_accuracy[,4:5]))
fc_accuracy_results

#fit ARIMA(p,d,q) models for p=1,2 q=1,2 d=1
ARIMA_fit <- gold_train2 %>%
  model(arima111 = ARIMA(Price ~ pdq(1,1,1)), #ARIMA(1,1,1) model
        arima211 = ARIMA(Price ~ pdq(2,1,1)), #ARIMA(2,1,1) model
        arima112 = ARIMA(Price ~ pdq(1,1,2)), #ARIMA(1,1,2) model
        arima212 = ARIMA(Price ~ pdq(2,1,2))) #ARIMA(2,1,2) model

report(ARIMA_fit)

ARIMA212_fit <- gold_train2 %>%
  model(ARIMA(Price ~ pdq(p=2, d=1, q=2)))

report(ARIMA212_fit)

#gg_residuals() function will give us the residual plots and ACF plot

ARIMA212_fit %>%
  gg_tsresiduals()

ARIMA_fc <- ARIMA212_fit %>%
  forecast(h=565)

ARIMA_fc %>%
  autoplot(gold_train2, level = NULL) + #add training set's plot to our forecast. Setting level to NULL means it will not give us the default 95% and 80% intervals.
  autolayer(gold_test, Price, colour = "grey") + #add the layer of actual values from the test set
  labs(title = "ARIMA(2,1,2) Forecast for Gold Mini Closing Price") #add title

ARIMA_fc_accuracy <- accuracy(ARIMA_fc, gold_test)
ARIMA_fc_accuracy

fc_accuracy_results <- bind_rows(fc_accuracy_results,
                                 tibble(Method = "ARIMA(2,1,2)",
                                        ARIMA_fc_accuracy[,4:5]))
fc_accuracy_results



#Applying differentiated MA(2) = ARIMA(0,1,2) model to larger training set:

best_model_fc <- gold_train_df %>%
  model(ARIMA(Price ~ pdq(p = 0,d = 1, q= 2))) %>%
  forecast(h = 314) #validation set is 314 days

best_model_fc %>%
  autoplot(gold_train_df, level = NULL) +
  autolayer(gold_validation_df, Price, colour = "grey") +
  labs(title = "ARIMA(0,1,2) Final Forecast for Gold Mini Closing Price")

accuracy(best_model_fc, gold_validation_df)

#extend the forecast of MA(2) model trained on gold_train2 data set to 879 days:
ma2_extended_fc <- MA2_fit %>%
  forecast(h=879) #879 days = 565 days of test set + 314 days of validation set

ma2_extended_fc %>%
  autoplot(gold_train2, level = NULL) + #add training set's plot to our forecast. Setting level to NULL means it will not give us the default 95% and 80% intervals.
  autolayer(gold_test, Price, colour = "light grey") +
  autolayer(gold_validation_df, Price, colour = "dark grey") + #add the layer of actual values from the test set
  labs(title = "MA(2) Final Forecast for Gold Mini Closing Price") #add title

#create a larger validation set of last 879 days:
larger_validation_df <- gold_df %>%
  slice(n() -878:0)
accuracy(ma2_extended_fc, larger_validation_df)









