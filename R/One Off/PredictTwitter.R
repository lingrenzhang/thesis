library(MASS)
library(ROCR)
library(MKmisc)

require(ff)
require(ffbase)
require(LaF)
require(ETLUtils)
require(biglm)
source('LogisticLib.R')
source('LogisticLibArchive.R')

#constants
bigdata <- FALSE
outsample_start <- 2011 * 12 #Dec 2010
outsample_end <- 2015 * 12 #Dec 2014
forecast_horizon <- 24
end_month <- 2015 * 12
update_interval <- 48

load('AggData_zero_substitute.RData')

data <- data[data$term > 0,]
data <- data[which(data$terminmonths > 0),]
data <- data[!is.na(data$stategroup),]
data <- data[!is.na(data$industry),]
data <- droplevels(data)
data$keyword <- NULL
data$fb_exists_logic <- (data$fb_exists == 'true')

cutoff_month <- outsample_start + 1
print(cutoff_month)
BernoulliInSample <- data[data$start_month_num + data$terminmonths < cutoff_month,]
BernoulliOutSample <- data[data$start_month_num >= cutoff_month & data$start_month_num < cutoff_month + update_interval & data$start_month_num + data$terminmonths <= end_month,]

year <- (cutoff_month - 1) %/% 12
month <- cutoff_month - year * 12

mod.twtr <- glm(exist ~ log(grossamountinthousand) + 
                             stategroup + 
                             industry,
                           family = binomial(),
                           data = BernoulliInSample)
mod.fb <- glm(fb_exists_logic ~ log(grossamountinthousand) + 
                  stategroup + 
                  industry,
                family = binomial(),
                data = BernoulliInSample)

PrintCoef(summary(mod.twtr)$coefficients, "Tables\\TwitterAccountPred.txt")
PrintCoef(summary(mod.fb)$coefficients, "Tables\\FacebookAccountPred.txt")

predTwitterIS <- predict(mod.twtr, BernoulliInSample, type = "response")
predFBIS <- predict(mod.twtr, BernoulliInSample, type = "response")

print(getLogLikelihood(predTwitterIS, BernoulliInSample$exist) / length(BernoulliInSample$exist))
print(getLogLikelihood(predFBIS, BernoulliInSample$fb_exists_logic) / length(BernoulliInSample$fb_exists_logic))

