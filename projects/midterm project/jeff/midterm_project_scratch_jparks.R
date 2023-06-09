# data 624 midterm project

# libraries
library(tidyverse)
library(fpp2)
library(readxl)
library(zoo)
library(seasonal)

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

# questions ..

# how to handle missing dates?  SeriesInd are not always consecutive.
# how to handle missing data? non-ARIMA decomp functions won't handle 'internal NAs'


## setup single analysis

my_data <- df_s01 %>%
  select(dates=Date,values=Var01) %>%
  drop_na()

## dates option 1 - use date_index and force new start date '2000-01-01'

my_ts <- ts(my_data$values, start = c(2000,1), frequency=365)

# skip from here to analysis



## dates option 2: use actual dates, expand dataset to include gaps, impute NAs

# create full date sequence
date_seq <- seq(min(my_data$dates), max(my_data$dates), by = "day")

# Convert original data frame to a zoo object
zoo_obj <- zooreg(my_data$values, order.by = my_data$dates, frequency=365)

# Merge with a zoo object containing consecutive dates
merged_obj <- merge(zoo_obj, zooreg(, order.by = date_seq, frequency=365))

# Impute missing values (carry forward last observed)
merged_obj_na <- na.locf(merged_obj)

# Convert to a time series object
# as.ts seems to drop dates and/or frequency
my_ts <- ts(coredata(merged_obj_na)[,1],
            start = c(year(min(my_data$dates)),
                      yday(min(my_data$dates))), frequency=365)


##
## analysis
##

# check stats
start(my_ts)
end(my_ts)
frequency(my_ts)

# simple graphs
autoplot(my_ts)

# seasonality
ggAcf(my_ts, lag=30)
ggseasonplot(my_ts)
ggsubseriesplot(my_ts)
gglagplot(my_ts)


# decomposition

decompose(my_ts) %>%
  autoplot()

seas(x=my_ts) %>%
  autoplot()

stl(my_ts) %>%
  autoplot()


# moving averages
my_ts_ma15 <- ma(my_ts, order=15, centre=TRUE)
autoplot(my_ts_ma15)


# exponential smoothing


# forecasts

forecast(my_ts, h=140) %>%
  autoplot()

forecast(my_ts_ma15, h=140) %>%
  autoplot()



# seasonally-adjusted forecasts

#
## TEST
#

df_s02 <- df %>%
  filter(category == 'S02') %>%
  mutate(Date_index = row_number()) %>%
  select(c(Date_index,Date,Var02,Var03)) %>%
  head(1622)

my_data <- df_s02 %>%
  select(dates=Date,values=Var02) %>%
  drop_na()

# create full date sequence
date_seq <- seq(min(my_data$dates), max(my_data$dates), by = "day")

# Convert original data frame to a zoo object
zoo_obj <- zooreg(my_data$values, order.by = my_data$dates, frequency=365)

# Merge with a zoo object containing consecutive dates
merged_obj <- merge(zoo_obj, zooreg(, order.by = date_seq, frequency=365))

# Impute missing values
#merged_obj_na <- na.locf(merged_obj) # carry-forward last observed
merged_obj_na <- merged_obj %>% replace(is.na(.), 0) # replace na with 0

# Convert to a time series object
# as.ts seems to drop dates and/or frequency
my_ts <- ts(coredata(merged_obj_na)[,1],
            start = c(year(min(my_data$dates)),
                      yday(min(my_data$dates))), frequency=365)

start(my_ts)
end(my_ts)
frequency(my_ts)


my_ts_ma7 <- ma(my_ts, order=7, centre=TRUE)

autoplot(my_ts)
autoplot(my_ts_ma7)

ggAcf(my_ts, lag=30)
ggAcf(my_ts_ma7, lag=30)

decompose(my_ts) %>% autoplot()
decompose(my_ts_ma7) %>% autoplot()

mstl(my_ts) %>% autoplot()
mstl(my_ts_ma7) %>% autoplot()

forecast(my_ts, h=140) %>% autoplot()
forecast(my_ts_ma7, h=140) %>% autoplot()

f <- forecast(my_ts_ma7, h=140)
accuracy(f)
