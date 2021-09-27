library("readxl")
library("tidyverse")
library("broom")
library("lmtest")
library("stargazer")
library("estimatr")

# File name with path
file <- "C://Users/svenw/Downloads/NYSE.xlsx"

#Reading the file
data <- read_excel(file)

View(data)

# Calculating lagged returns (t-1)
data$lag <- dplyr::lag(data$Return)
#squared returns
data$returns_sq <- data$Return^2

#lagged squared returns
data$lag_sq <- dplyr::lag(data$returns_sq)


nyse <- na.omit(data)
view(nyse)
#1a:

#Model 1
mod1 <- lm(Return~ lag, data = nyse)
summary(mod1)
deviance(mod1)

stargazer(mod1,
          type= "html",
          out= "emh_mod1.doc", 
          title= "Model 1 Results", 
          align = TRUE)

#Model 2
mod2 <- lm(Return~ lag + lag_sq, data = nyse)
summary(mod2)
deviance(mod2)

stargazer(mod2,
          type= "html",
          out= "emh_mod2.doc", 
          title= "Model 2 Results", 
          align = TRUE)

#Model 3: USing robust standard errors
mod3 <- lm_robust(Return ~ lag, data = nyse)
summary(mod3)
deviance(mod3)

stargazer(mod3,
          type= "html",
          out= "emh_mod3.doc", 
          title= "Model 3 Results", 
          align = TRUE)

#Model 4: Using Breusch-Pagan Test
mod4 <- lm(mod1$residuals^2~ lag, data = nyse)
summary(mod4)
deviance(mod4)

stargazer(mod4,
          type= "html",
          out= "emh_mod4.doc", 
          title= "Model 4 Results", 
          align = TRUE)

#Model 5: Using Breusch-Pagan Test on all variables
mod5 <- lm(mod2$residuals^2~ lag+lag_sq, data = nyse)
summary(mod5)
deviance(mod5)

stargazer(mod5,
          type= "html",
          out= "emh_mod5.doc", 
          title= "Model 5 Results", 
          align = TRUE)

#1b) Running unrestructed regression to get RSS
rss_r <- lm(Return~1, data = nyse)
summary(rss_r)
deviance(rss_r)


# 1c) Is non-linear specification necessary? 
# Performing RESET test using squared terms only.

reset_1c <- resettest(mod1, power=2, type='fitted', data = nyse)
reset_1c

# 1e) Performing Breush-Pagan test for heteroskedasticity by running regression (4). 

bp_1e <- lm(mod4$residuals^2 ~ lag, data = nyse)
bp_1e
summary(bp_1e)

# 1h) Compute the fitted values: hhat,from regressions (4) and (5). 
# checking for any negative fitted h(x) values in both regressions.

#Negative fitted values in regression(4)?
unique(mod4$fitted.values < 0)

#Negative fitted values in regression(5)?
unique(mod5$fitted.values < 0)

#1i) Using the hhat(x) from regression (5) to estimate regression (1) by weighted least squares h(x) 
lres5 <- log(mod5$residuals^2)
weights_reg5 <- lm(lres5~lag, data = nyse)

hhat <- exp(weights_reg5$fitted.values)

wls_1i <- lm(Return~ lag, weights = 1/hhat, data = nyse)
wls_1i