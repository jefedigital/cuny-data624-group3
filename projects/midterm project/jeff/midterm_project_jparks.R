# data 624 midterm project

# libraries
library(tidyverse)
library(fpp2)
library(readxl)
library(zoo)
library(seasonal)

library(MLmetrics)
library(imputeTS)

# load data
df <- read_xls('../data/Data Set for Class.xls', sheet='Set for Class') %>%
  mutate(Date = as.Date(SeriesInd-2, origin = '1900-01-01'))

# set up timeseries

df_s01 <- df %>%
  filter(category == 'S01') %>%
  mutate(Date_index = row_number()) %>%
  select(c(Date_index,Date,Var01,Var02)) %>%
  head(1622)

df_s02 <- df %>%
  filter(category == 'S02') %>%
  mutate(Date_index = row_number()) %>%
  select(c(Date_index,Date,Var02,Var03)) %>%
  head(1622)

df_s03 <- df %>%
  filter(category == 'S03') %>%
  mutate(Date_index = row_number()) %>%
  select(c(Date_index,Date,Var05,Var07)) %>%
  head(1622)

df_s04 <- df %>%
  filter(category == 'S04') %>%
  mutate(Date_index = row_number()) %>%
  select(c(Date_index,Date,Var01,Var02)) %>%
  head(1622)

df_s05 <- df %>%
  filter(category == 'S05') %>%
  mutate(Date_index = row_number()) %>%
  select(c(Date_index,Date,Var02,Var03)) %>%
  head(1622)

df_s06 <- df %>%
  filter(category == 'S06') %>%
  mutate(Date_index = row_number()) %>%
  select(c(Date_index,Date,Var05,Var07)) %>%
  head(1622)


# Steps:
#
# Data EDA
#
# - Observations
# - Dates
# - Missing Data
#
# Data Transformations
#
# - Center, Scale
# - Power / Log Transforms for normality
#
# Model Selection
#
# - Taking into account the decomp, what's recommended
# - auto.arima to identify ideal model
# - optim()
#
# Forecast and diagnostics
#
# - Forecast X periods
# - Test/Train or use tsCV (preferred) to measure accuracy and MAPE
#
# Output
#
# - write csv with forecast values


#
# S02
# Jeff

# v02
my_data <- df_s02 %>%
  select(values=Var02)

# v03
my_data <- df_s02 %>%
  select(values=Var03)

summary(my_data)

# missing data
my_data <- na_interpolation(my_data)

# make ts
my_ts <- ts(my_data)
autoplot(my_ts)

# decomp
autoplot(decompose(my_ts))

# distributions
hist(my_ts)
boxplot(my_ts)

# outliers
my_ts_out <- tsoutliers(my_ts)
my_ts_clean <- tsclean(my_ts)
autoplot(my_ts_clean) + ggtitle("Cleaned")

# my_ts_clean <- my_ts # for no-clean only

# autocorrelations
ggAcf(my_ts_clean)
autoplot(diff(my_ts_clean))
acf(diff(my_ts_clean), main = "ACF of S03_Var05 Difference")

# my_ts_clean = diff(my_ts_clean) # for diff only
my_ts_clean <- diff(my_ts_clean) # for no-clean only
autoplot(my_ts_clean)


# split
my_ts_train <- window(my_ts_clean, end = as.integer(length(my_ts_clean) * 0.80)) # 1297

# auto.arima
my_ts_fit <- auto.arima(my_ts_train, stepwise = FALSE)
my_ts_fit

my_ts_fit <- auto.arima(my_ts_train, lambda='auto', stepwise = FALSE)
my_ts_fit

my_ts_forecast <- forecast(my_ts_fit, h = 325)
autoplot(my_ts_forecast)

my_ts_acc <- accuracy(my_ts_forecast, my_ts_clean)["Test set", "MAPE"] # 3
my_ts_acc


# **checkresiduals()**


c(my_ts_arima_acc, my_ts_holtd_acc, my_ts_ses_acc) %>% nice_table()



# v02

# 1. no-clean: ARIMA(4,1,1) with drift > 52.23913
# 2. no-clean & lambda: ARIMA(2,1,3) > 28.21356
# 3. clean: ARIMA(2,1,3) with drift > 45.82757
# 4. clean & lambda: ARIMA(2,1,2) > 27.86043 **


# v03

# 1. no-clean: ARIMA(2,1,0) > 16.39621
# 2. no-clean & lambda: ARIMA(2,1,0) > 16.38604
# 3. clean: ARIMA(2,1,0) > 16.19648
# 4. clean & lambda: ARIMA(2,1,0) > 16.18627 **

# vo3 diff
## v03 is a candidate for making a stationary with diff

# 1. ARIMA(2,0,0) with zero mean > Inf ??
# 2. ARIMA(2,0,0) with non-zero mean & Lambda=1 > Inf ?



accuracy(my_ts_holtd)[5] # 24.76784
accuracy(my_ts_ses)[5] # 24.7958



# 1. simple timeseries plot of the raw data

my_data <- data_s02_v02
my_ts <- ts(my_data$values, start = c(year(min(my_data$dates)),yday(min(my_data$dates))), frequency=365)

autoplot(my_ts) +
  ggtitle('Series 2, Product 2 - Time Series')




#
# S04
# Matt

s04 <- subset(df, category == 'S04', select = c(SeriesInd, Var01, Var02)) %>%
  mutate(date = as.Date(SeriesInd, origin = "1900-01-01"))
summary(s04)
str(s04)

s04_1 <- s04 %>% select(Var01)
s04_1 <- s04_1[1:1622,]
ggplot(s04, aes(x=date, y=Var01))+geom_line()+ xlab("Time") + ylab("S04_V01")
s04_1 <- na_interpolation(s04_1)
s04_1 <- ts(s04_1)
s04_1 <- tsclean(s04_1)

V1train <- window(s04_1, end = as.integer(length(s04_1)*0.8)) # 1297 train

# ARIMA(2,1,2) with drift
farimafit_1 <- V1train %>% auto.arima(lambda = (BoxCox.lambda(s04_1)), stepwise = FALSE) %>% forecast(h = 325)
accuracy(farimafit_1, s04_1)["Test set", "MAPE"] # 37.4475

naive_1 <- naive(V1train, h=325)
accuracy(naive_1, s04_1)["Test set", "MAPE"] # 23.2743

# need to pick one and make final forecast


s04_2 <- s04 %>% select(Var02)
s04_2 <- s04_2[1:1622,]
ggplot(s04, aes(x=date, y=Var02))+geom_line()+ xlab("Time") + ylab("S05_V02")
s04_2 <- na_interpolation(s04_2)
s04_2 <- ts(s04_2)
s04_2 <- tsclean(s04_2)
V2train <- window(s04_2, end = as.integer(length(s04_2)*0.8))
farimafit_2 <- V2train %>% auto.arima(lambda = (BoxCox.lambda(s04_2)), stepwise = FALSE) %>% forecast(h = 325)
naive_2 <- naive(V2train, h=325)
accuracy(farimafit_2, s04_2)["Test set", "MAPE"]
accuracy(naive_2, s04_2)["Test set", "MAPE"]

# need to pick one and make final forecast


#
# S03
# Lwin
#


# data_S03 <- subset(DATA624_Project1_Data_S03, category == 'S03',
#                    select = c(SeriesInd, Var05, Var07)) %>%
#   mutate(date = as.Date(SeriesInd, origin = "1900-01-01"))
#
# summary(data_S03)
# str(data_S03)
# data_S03 <- filter(data_S03, SeriesInd <= 43221)
# summary(data_S03)
# str(data_S03)





## V05
# data_x03_v05 <- data_S03 %>% select(Var05)
# data_x03_v05 <- data_x03_v05[1:1622,]

# data_s03_v05 <- df_s03 %>%
#   select(dates=Date,values=Var05)

data_s03_v05 <- df_s03 %>%
  select(values=Var05)

summary(data_s03_v05)
str(data_s03_v05)

# missing data
data_s03_v05 <- na_interpolation(data_s03_v05)
summary(data_s03_v05)
str(data_s03_v05)

# make ts
data_s03_v05 <- ts(data_s03_v05)
str(data_s03_v05)
autoplot(data_s03_v05) + ggtitle("Time Series Data S03_Var05")

# distributions
# par(mfrow= c(1,2))
hist(data_s03_v05)
boxplot(data_s03_v05)

# outliers
data_s03_v05_out <- tsoutliers(data_s03_v05)
data_s03_v05 <- tsclean(data_s03_v05)
autoplot(data_s03_v05) + ggtitle("Cleaned Data_S03_Var05")

# autocorrelations
ggAcf(data_s03_v05)
autoplot(diff(data_s03_v05))
acf(diff(data_s03_v05), main = "ACF of S03_Var05 Difference")
# There is no seasonality due to the presence of one order autocorrelation.

# split
data_s03_v05_train <- window(data_s03_v05, end = as.integer(length(data_s03_v05) * 0.80)) # 1297
lambda5 <- BoxCox.lambda(data_s03_v05)

# Apply the selected model to training Set
f_horizon <- length(data_s03_v05) - as.integer(length(data_s03_v05) * 0.914) # 140 - incorrect

# ARIMA(1,1,1) with drift
data_s03_v05_farima_fit <- auto.arima(data_s03_v05_train, lambda = lambda5, stepwise = FALSE)

fresult_arima_V05 <- forecast(data_s03_v05_farima_fit, h = f_horizon)  # 140
fresult_arima_V05_2 <- forecast(data_s03_v05_farima_fit, h = 325)  # 325


DS03_Var05_farima_ac <- accuracy(fresult_arima_V05, data_s03_v05)["Test set", "MAPE"] # 13.3363
DS03_Var05_farima_ac_2 <- accuracy(fresult_arima_V05_2, data_s03_v05)["Test set", "MAPE"] # 38.94135



autoplot(fresult_arima_V05, alpha = 0.65)
checkresiduals(fresult_arima_V05)
#with p-value greater than 0.05, there is convincing evidence that residuals for Var05 are white noise. On ACF, the residuals are uncorrelated. The histogram shows that the residuals are normal distributed.

# ets
data_s03_v05_fets_fit <- ets(data_s03_v05_train, lambda = lambda5)
fresult_fets_V05<- forecast(data_s03_v05_fets_fit, h = f_horizon)
autoplot(fresult_fets_V05)+autolayer(fresult_fets_V05, alpha = 0.65)
checkresiduals(fresult_fets_V05)
#The p values for the ETS models residuals are less than 0.05.The residuals are not white noise. They ETS models have prediction interval to wide. The ETS models have the best accuracy with the test set

# naive
data_s03_v05_naive_fit <- naive(data_s03_v05_train, h = f_horizon)
autoplot(data_s03_v05_naive_fit)+autolayer(data_s03_v05_naive_fit, alpha = 0.65)
checkresiduals(data_s03_v05_naive_fit)

# accuracy
DS03_Var05_farima_ac <- accuracy(fresult_arima_V05, data_s03_v05)["Test set", "MAPE"] # 13.3363


DS03_Var05_fets_ac <- accuracy(fresult_fets_V05, data_s03_v05)["Test set", "MAPE"]
DS03_Var05_fnaive_ac <- accuracy(data_s03_v05_naive_fit, data_s03_v05)["Test set", "MAPE"] <- auto.arima(data_s03_v05_train, lambda = lambda5, stepwise = FALSE)
fresult_arima_V05 <- forecast(data_s03_v05_farima_fit, h = f_horizon)
autoplot(fresult_arima_V05)+autolayer(fresult_arima_V05, alpha = 0.65)
checkresiduals(fresult_arima_V05)
#with p-value greater than 0.05, there is convincing evidence that residuals for Var05 are white noise. On ACF, the residuals are uncorrelated. The histogram shows that the residuals are normal distributed.

# ets
data_s03_v05_fets_fit <- ets(data_s03_v05_train, lambda = lambda5)
fresult_fets_V05<- forecast(data_s03_v05_fets_fit, h = f_horizon)
autoplot(fresult_fets_V05)+autolayer(fresult_fets_V05, alpha = 0.65)
checkresiduals(fresult_fets_V05)
#The p values for the ETS models residuals are less than 0.05.The residuals are not white noise. They ETS models have prediction interval to wide. The ETS models have the best accuracy with the test set

# naive
data_s03_v05_naive_fit <- naive(data_s03_v05_train, h = f_horizon)
autoplot(data_s03_v05_naive_fit)+autolayer(data_s03_v05_naive_fit, alpha = 0.65)
checkresiduals(data_s03_v05_naive_fit)

# accuracy
DS03_Var05_farima_ac <- accuracy(fresult_arima_V05, data_s03_v05)["Test set", "MAPE"]
DS03_Var05_fets_ac <- accuracy(fresult_fets_V05, data_s03_v05)["Test set", "MAPE"]
DS03_Var05_fnaive_ac <- accuracy(data_s03_v05_naive_fit, data_s03_v05)["Test set", "MAPE"]


## V07
# data_s03_v07 <- data_S03 %>% select(Var07)
# data_s03_v07 <- data_s03_v07[1:1622,]

data_s03_v07 <- df_s03 %>%
  select(values=Var07)

summary(data_s03_v07)
str(data_s03_v07)

# missing data
data_s03_v07 <- na_interpolation(data_s03_v07)
summary(data_s03_v07)
str(data_s03_v07)

# make ts
data_s03_v07 <- ts(data_s03_v07)
str(data_s03_v07)
autoplot(data_s03_v07) + ggtitle("Time Series Data S03_Var07")

# distributions
par(mfrow= c(1,2))
hist(data_s03_v07)
boxplot(data_s03_v07)

# outliers
data_s03_v07_out <- tsoutliers(data_s03_v07)
data_s03_v07 <-tsclean(data_s03_v07)
autoplot(data_s03_v07) + ggtitle("Cleaned Data_S03_Var07")

# autocorrelations
ggAcf(data_s03_v07)
autoplot(diff(data_s03_v07))
acf(diff(data_s03_v07), main = "ACF of S03_Var07 Difference")
# There is no seasonality due to the presence of one order autocorrelation.

# split
data_s03_v07_train <- window(data_s03_v07, end = as.integer(length(data_s03_v07) * 0.85))
lambda7 <- BoxCox.lambda(data_s03_v07)

# arima
data_s03_v07_farima_fit <- auto.arima(data_s03_v07_train, lambda = lambda7, stepwise = FALSE)
fresult_arima_V07 <- forecast(data_s03_v07_farima_fit, h = f_horizon)
autoplot(fresult_arima_V07) + autolayer(fresult_arima_V07, alpha = 0.65)
checkresiduals(fresult_arima_V07)

# ets
data_s03_v07_fets_fit <- ets(data_s03_v07_train, lambda = lambda7)
fresult_fets_V07<- forecast(data_s03_v07_fets_fit, h = f_horizon)
autoplot(fresult_fets_V07) + autolayer(fresult_fets_V07, alpha = 0.65)
checkresiduals(fresult_fets_V07)

# naive
data_s03_v07_naive_fit <- naive(data_s03_v07_train, h = f_horizon)
autoplot(data_s03_v07_naive_fit) + autolayer(data_s03_v07_naive_fit, alpha = 0.65)
checkresiduals(data_s03_v07_naive_fit)

# accuracy
DS03_Var07_farima_ac <- accuracy(fresult_arima_V07, data_s03_v07)["Test set", "MAPE"]
DS03_Var07_fets_ac <- accuracy(fresult_fets_V07, data_s03_v07)["Test set", "MAPE"]
DS03_Var07_fnaive_ac <- accuracy(data_s03_v07_naive_fit, data_s03_v07)["Test set", "MAPE"]



# compare models
DS03_Var05_MAPE <- c(DS03_Var05_fnaive_ac, DS03_Var05_farima_ac, DS03_Var05_fets_ac)
DS03_Var07_MAPE <- c(DS03_Var07_fnaive_ac, DS03_Var07_farima_ac, DS03_Var07_fets_ac)

S03_MAPE <- matrix(rbind(DS03_Var05_MAPE, DS03_Var07_MAPE), nrow = 2, ncol = 3)

rownames(S03_MAPE) <- c("S03_Var05", "S03_Var07")
colnames(S03_MAPE) <- c("Naive", "ARIMA", "ETS")
data.frame(S03_MAPE)




#
# correlation tests of the two datasets
correlation_test <- cor.test(data_s03_v05, data_s03_v07)
cor_coefficient <- correlation_test$estimate
p_value <- correlation_test$p.value
print(correlation_test)

lmodel <- lm(data_s03_v05 ~ data_s03_v07)
plot(data_s03_v05, data_s03_v07)
abline(lmodel)
title(main = "Scatter plot of S03_Var05 Vs S03_Var07")

correlation <- cor(data_s03_v05, data_s03_v07)
squared_correlation <- correlation^2
print(squared_correlation)
