library(MASS)
library(ROCR)
library(MKmisc)

require(ff)
require(ffbase)
require(LaF)
require(ETLUtils)
require(biglm)
source('LogisticLib.R')

#constants
industry <- ""
bigdata <- FALSE
bigdataFileIndicator <- ifelse(bigdata, "big", "small")
outsample_start <- 2011 * 12 #Dec 2010
outsample_end <- 2015 * 12 #Dec 2014
forecast_horizon <- 24
update_interval <- 48
end_month <- 2015 * 12

# process raw data
if(bigdata) {
  load('AggData_zero_substitute.RData')
} else {
  load('AggData.RData')
}

data <- data[data$term > 0,]
data <- data[which(data$terminmonths > 0),]
data <- data[!is.na(data$stategroup),]
data <- data[!is.na(data$industry),]
data <- droplevels(data)
data$keyword <- NULL
data$fb_exists <- NULL
data$exist <- NULL
dataExpanded <- Expand(data)
dataExpanded <- IncorporateExternalData(dataExpanded)
data$monthindex <- 0
data <- IncorporateExternalData(data)

cutoff_month <- outsample_start + 1
MonthlyInSample <- dataExpanded[dataExpanded$start_month_num + dataExpanded$monthindex < cutoff_month,]
MonthlyOutSample <- data[data$start_month_num >= cutoff_month & data$start_month_num < cutoff_month + update_interval & data$start_month_num <= end_month - forecast_horizon & data$terminmonths >= forecast_horizon,]

pdf("roc_one_by_one.pdf")
title <- 'Monthly Forecast Model (Adding Traditional Covariates One-By-One)'

#curve 1
mod <- glm(default ~ log(grossamountinthousand),
             family = binomial(),
           data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = 'False Positive Rate', 
     ylab = 'True Positive Rate', col = 'red', main = title)
par(new=TRUE)

#curve 2
mod <- glm(default ~ log(grossamountinthousand) + 
             guaranteedratio,
           family = binomial(),
           data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'green')
par(new=TRUE)

#curve 3
mod <- glm(default ~ log(grossamountinthousand) + 
             guaranteedratio + 
             terminmonths,
           family = binomial(),
           data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'blue')
par(new=TRUE)

#curve 4
mod <- glm(default ~ log(grossamountinthousand) + 
             guaranteedratio + 
             terminmonths + 
             borrowerbanksamestate,
           family = binomial(),
           data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'red', lty = 2)
par(new=TRUE)

#curve 5
mod <- glm(default ~ log(grossamountinthousand) + 
             guaranteedratio + 
             terminmonths + 
             borrowerbanksamestate + 
             termisfullyear,
           family = binomial(),
           data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'green', lty = 2)
par(new=TRUE)

#curve 6
mod <- glm(default ~ log(grossamountinthousand) + 
             guaranteedratio + 
             terminmonths + 
             borrowerbanksamestate + 
             termisfullyear + 
             stategroup,
           family = binomial(),
           data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'blue', lty = 2)
par(new=TRUE)

#curve 7
mod <- glm(default ~ log(grossamountinthousand) + 
             guaranteedratio + 
             terminmonths + 
             borrowerbanksamestate + 
             termisfullyear + 
             stategroup + 
             unemployment_by_state,
           family = binomial(),
           data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'red', lty = 3)
par(new=TRUE)

#curve 8
mod <- glm(default ~ log(grossamountinthousand) + 
                          guaranteedratio + 
                          terminmonths + 
                          borrowerbanksamestate + 
                          termisfullyear + 
                          stategroup + 
                          unemployment_by_state +
                          gdp_by_state_diff,
                        family = binomial(),
                        data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'green', lty = 3)
par(new=TRUE)

#curve 9
mod <- glm(default ~ log(grossamountinthousand) + 
                          guaranteedratio + 
                          terminmonths + 
                          borrowerbanksamestate + 
                          termisfullyear + 
                          stategroup + 
                          unemployment_by_state +
                          gdp_by_state_diff +
                          gdp_by_industry_diff,                            
                        family = binomial(),
                        data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'blue', lty = 3)
par(new=TRUE)

#curve 10
mod <- glm(default ~ log(grossamountinthousand) + 
                          guaranteedratio + 
                          terminmonths + 
                          borrowerbanksamestate + 
                          termisfullyear + 
                          stategroup + 
                          unemployment_by_state +
                          gdp_by_state_diff +
                          gdp_by_industry_diff +                            
                          cs_housing_1yr,
                        family = binomial(),
                        data = MonthlyInSample)
defaultMonthly <- GetDefault(MonthlyOutSample, forecast_horizon, FALSE)
predEconMonthly <- GetPred(mod, MonthlyOutSample, forecast_horizon, FALSE)
rocEconMonthly  <- GetRoc(predEconMonthly, defaultMonthly)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'red', lty = 4)

#end picture
legend(0.5, 0.5, c("log(gross amount in thousand)", "+ guaranteed ratio", "+ term in months", 
                   "+ borrower bank same state", "+ term is full year", "+ state group", 
                   "+ unemployment by state", "+ GDP by state", "+ GDP by industry", "+ CS Housing Index"), 
       lty = c(1,1,1,2,2,2,3,3,3,4), lwd = rep(2.5,9), col = c('red', 'green', 'blue', 'red', 'green', 'blue', 'red', 'green', 'blue', 'red'))
dev.off()

