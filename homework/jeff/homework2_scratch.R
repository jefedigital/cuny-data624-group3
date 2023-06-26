# Homework 2
library(tidyverse)
library(AppliedPredictiveModeling)
library(naniar)
library(simputation)

data(ChemicalManufacturingProcess)
df <- ChemicalManufacturingProcess


# plot missing data
gg_miss_upset(df)

ggplot(df, aes(x=ManufacturingProcess03, y=Yield)) +
  geom_miss_point()


# simputation
cols_missing <- df %>%
  select(names(df)[sapply(df, anyNA)]) %>%
  colnames() # 28 cols

df_imputed <- df

for(col in cols_missing){
  f <- as.formula(paste(col, '~ Yield'))
  df_imputed <- impute_lm(df_imputed, f)
}

# plot after imputation
ggplot(df_imputed, aes(x=ManufacturingProcess03, y=Yield)) +
  geom_point(aes(alpha=0.1))


# other transformations

# distributions
df_imputed %>%
  pivot_longer(cols=!Yield, names_to='variables', values_to='values') %>%
  ggplot(aes(x=values)) +
  geom_histogram() + facet_wrap(~ variables, scales='free')

# scale and center

## some of these are bimodal


# remove pairwise correlations




# split
sample <- sample(c(TRUE, FALSE), nrow(df_imputed), replace=TRUE, prob=c(0.85,0.15))
df_train  <- df_imputed[sample,]
df_test   <- df_imputed[!sample,]

# small dataset!


# Model 1: OLS






