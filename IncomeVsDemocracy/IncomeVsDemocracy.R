library("readxl")
library("tidyverse")
library("broom")
library("lmtest")
library("stargazer")

library("knitr")
library("plm")
library("plyr")

# File name with path
file <- "C://Users/svenw/Downloads/democracy.csv"

#Reading the file
data <- read.csv(file)
View(data)

# Lag calculations except socialism column
data$fhpolr.lag <- ifelse(data$year == 1960, NA, lag(data$fhpolr))
data$lrgdppc.lag <- ifelse(data$year == 1960, NA, lag(data$lrgdppc))
data$laborshare.lag <- ifelse(data$year == 1960, NA, lag(data$laborshare))
data$lpop.lag <- ifelse(data$year == 1960, NA, lag(data$lpop))

#1a) fhpolr = dependent variable

#Model 1: OLS, run a regression with lagged values of fhpolr and lrgdppc

#Removing NA values
democracy <- na.omit(data)
View(democracy)

mod1 <- lm(fhpolr~fhpolr.lag + lrgdppc.lag, data = democracy)
summary(mod1)

deviance(mod1)

stargazer(mod1,
          type= "html",
          out= "ivd_mod1.doc", 
          title= "Model 1 Results", 
          align = TRUE)

#Model 2: Add year dummies. Fixed Effects, used within.
year_dummies <- pdata.frame(democracy, index=c("year"), 
                            stringsAsFactors = FALSE)


mod2 <- plm(fhpolr~fhpolr.lag + lrgdppc.lag + factor(year), 
            data = year_dummies, model = 'within')

coeftest(mod2, vcov. = vcovHC, type ='HC1')

summary(mod2)

stargazer(mod2, 
          type = "html", 
          out = "ivd_mod2.doc", 
          title = "Model 2 Results", 
          align=TRUE)

#Model 3: Add BOTH year and country dummies. Fixed Effects.
year_country_dummies <- pdata.frame(democracy, index=c("year","code_numeric"), 
                               stringsAsFactors = FALSE)

mod3 <- plm(fhpolr~fhpolr.lag + lrgdppc.lag, data = year_country_dummies, 
            model = "within", effect = "twoways")

coeftest(mod3, vcov. = vcovHC, type ='HC1')

summary(mod3)

stargazer(mod3, 
          type = "html", 
          out = "ivd_mod3.doc", 
          title = "Model 3 Results", 
          align=TRUE)

# Unique country code values in democracy
countries <-(unique(democracy$code_numeric))
length(countries)

#Model 4: Add further controls-socialism.

mod4 <- plm(fhpolr~fhpolr.lag + lrgdppc.lag + laborshare.lag + lpop.lag
            + socialism, data = year_country_dummies, 
            model = "within", effect = "twoways")

coeftest(mod4, vcov. = vcovHC, type ='HC1')

summary(mod4)

stargazer(mod4, 
          type = "html", 
          out = "ivd_mod4.doc", 
          title = "Model 4 Results", 
          align=TRUE)


# Combining into 1a table
stargazer(mod1, mod2, mod3, mod4,
          type="html",
          out="ivd_1a_table.doc", 
          title="Results",
          notes.append =  F,
          add.lines = list(c("RSS", 14.98, 14.066, 9.4164, 9.3858)),
          notes = c(" Significant at the 10% level",
                    "** Significant at the 5% level",
                    "*** Significant at the 1% level"),
          align=TRUE)

#1e 
# Running unrestricted regression
e_unres_reg <- lm(fhpolr~fhpolr.lag + lrgdppc.lag, data = democracy)

summary(e_unres_reg)