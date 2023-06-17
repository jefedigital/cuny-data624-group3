library(mlbench)
library(tidyverse)

data(Soybean)
df_soybean <- Soybean

table(df_soybean$Class)

prop.table(table(df_soybean$Class))

ggplot(df_soybean, aes(x=Class)) + geom_bar()

# factors: ordered to regular; drop NAs and 'Class'
df_soybean_freq <- df_soybean %>%
  dplyr::select(!Class) %>%
  drop_na() %>%
  mutate_if(is.ordered, ~ factor(., ordered = FALSE)) 

df_soybean_freq %>%
  pivot_longer(everything(), names_to = c('variables'), values_to = c('values')) %>%
  ggplot() +
  geom_bar(aes(x=values)) +
  facet_wrap(vars(variables), scales="free")


## missing

library(naniar)
library(visdat)
library(skimr)

summary(df_soybean)
str(df_soybean)

skim(df_soybean)
vis_dat(df_soybean)
vis_miss(df_soybean)

# bingo
gg_miss_var(df_soybean, show_pct=TRUE)
gg_miss_var(df_soybean, facet = Class, show_pct=TRUE)

df_soybean_missing_one <- df_soybean %>% 
  filter(Class == '2-4-d-injury')

df_soybean_missing_one %>%
  gg_miss_var(facet = Class, show_pct=TRUE)

table(df_soybean$Class) %>% sort() #53


## HA 7.1

library(fpp2)
data(pigs)
ts_pigs<- pigs

# a.  Use the `ses()` function in R to find the optimal values of $\alpha$ and $\ell_0$, and generate forecasts for the next four months.

ts_pigs
length(ts_pigs) # 188

pigs_ses <- ses(ts_pigs, h=4)

summary(pigs_ses)


# alpha = 0.2971 
pigs_ses$model$par['alpha']

# l = 77260.0561 
pigs_ses$model$par['l']

pigs_ses %>% autoplot()
forecast(pigs_ses)       

# Compute a 95% prediction interval for the first forecast using $\hat{y} \pm 1.96s$ where s is the standard deviation of the residuals. Compare your interval with the interval produced by R.

pigs_ses$mean[1] # point forecast

# PI from model
pigs_ses$lower[,'95%'][1] # 78611.97
pigs_ses$upper[,'95%'][1] # 119020.8

# PI calculated
# these are estimates .. more accurate PIs provided in the model
pigs_ses_sd <- sd(pigs_ses$residuals)

pigs_ses$mean[1] - pigs_ses_sd # 88542.71
pigs_ses$mean[1] + pigs_ses_sd # 109090.1



## 7.2 Write your own function to implement simple exponential smoothing. The function should take arguments `y` (the time series), `alpha` (the smoothing parameter α) and `level`(the initial level ℓ0). It should return the forecast of the next observation in the series. Does it give the same forecast as `ses()`?

#library(fpp2)
data(pigs)
ts_pigs<- pigs

ts_pigs

# pseudo
# function(ts,alpha,level){
#   levels = vector, start with level 
#   
#   # create levels
#   for y in ts{ 
#     l = (alpha times y) + ((1-alpha) times levels[last])
#     append l to levels
#   }
#   forecast = levels[last]
#   return forecast
# }
# 
# Smoothing parameters:
#   alpha = 0.2971 
# 
# Initial states:
#   l = 77260.0561




exp_smooth <- function(ts,alpha,lambda){
  levels = as.vector(lambda)
  
  for (y in ts){
    l <- (alpha * y) + ((1-alpha) * last(levels))
    levels <- append(levels,l)
  }
  
  forecast = last(levels)
  return(forecast)
}


exp_smooth(pigs, 0.2971, 77260.0561)
# 98816.45  pretty damn close


## 7.2 Modify your function from the previous exercise to return the sum of squared errors rather than the forecast of the next observation. Then use the optim() function to find the optimal values of α and ℓ0. Do you get the same values as the ses() function?

exp_smooth_sse <- function(ts,alpha,lambda){
  levels = as.vector(lambda)
  
  for (y in ts){
    l <- (alpha * y) + ((1-alpha) * last(levels))
    levels <- append(levels,l)
  }
  
  residuals <- ts[1:length(ts)] - levels[1:length(ts)]
  return(sum(residuals^2))
}

exp_smooth_sse(pigs, 0.2971, 77260.0561) # 19765613579




# Define the objective function
objective_function <- function(params, data) {
  alpha <- params[1]
  lambda <- params[2]
  
  levels = as.vector(lambda)
  
  for (y in data){
    l <- (alpha * y) + ((1-alpha) * last(levels))
    levels <- append(levels,l)
  }
  
  residuals <- ts[1:length(ts)] - levels[1:length(ts)]
  return(sum(residuals^2))
}

# Use optim() to find the optimal values for alpha and lambda
result <- optim(c(0.5, 75000), objective_function, data = pigs)

# Retrieve the optimal values
optimal_alpha <- result$par[1] # 1.0001
optimal_lambda <- result$par[2] # 0.6647968

pigs


## 8.1

# white noise
autoplot(ts(rnorm(50)))

ggAcf(ts(rnorm(36)))

ggAcf(ts(rnorm(360)))

ggAcf(ts(rnorm(1000))) 


## 8.2

autoplot(ibmclose)

ggAcf(ibmclose)
ggPacf(ibmclose)

df_ibm_diff <- diff(ibmclose)

autoplot(df_ibm_diff)

autoplot(ibmclose) + 
  autolayer(diff(ibmclose))

ggAcf(diff(ibmclose))
ggPacf(diff(ibmclose))

Box.test(diff(ibmclose), lag=10, type="Ljung-Box") # p-value = 0.1701


## 8.6  Use R to simulate and plot some data from simple ARIMA models.

ar1 <- function(y,e,phi){
  
  for(i in 2:100){
    y[i] <- phi * y[i-1] + e[i]
  }
  
  autoplot(y, ylim=c(-5,5))
}

y <- ts(numeric(100))
e <- rnorm(100)

ar1(y,e,0.2)
ar1(y,e,0.5)
ar1(y,e,0.9)


# Write your own code to generate data from an MA(1) model

# need forecasts and to compute errors
# use a simple exponential smoothing?
  

exp_smooth_levels <- function(ts,alpha,lambda){
  levels = as.vector(lambda)
  
  for (y in ts){
    l <- (alpha * y) + ((1-alpha) * last(levels))
    levels <- append(levels,l)
  }
  
  return(levels[2:length(levels)])
}

ts <- ts(rnorm(50))
ts_ses <- ts(exp_smooth_levels(ts, 0.6, 0))

autoplot(ts) +
  autolayer(ts_ses)

ts[1:10]
ts_ses[1:10]
ts - ts_ses

# not this ^^


y <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100){
  y[i] <- y[i-1] + 0.6*e[i]
}

autoplot(y)

# Generate data from an ARMA(1,1) model with  ϕ1=0.6, θ1=0.6 and σ2=1.

y <- ts(numeric(100))
e <- rnorm(100)

for(i in 2:100){
  y[i] <- 0.6*y[i-1] + 0.6*e[i]
}

autoplot(y)




