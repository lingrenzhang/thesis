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
industryTag <- ""
#industry <- c(  "Accommodation and Food Services", "Arts, Entertainment, and Recreation", "Educational Services", "Finance and Insurance",                                                   
#                "Health Care and Social Assistance", "Information", "Real Estate Rental and Leasing", "Retail Trade")                                                            
#industryTag <- "B2C"
#industry <- c(  "Administrative and Support and Waste Management and Remediation Services", "Agriculture, Forestry, Fishing and Hunting",                              
#                "Construction", "Management of Companies and Enterprises", "Manufacturing", "Mining", "Other Services (except Public Administration)",                           
#                "Professional, Scientific, and Technical Services", "Public Administration", "Transportation and Warehousing", "Utilities", "Wholesale Trade")              
#industryTag <- "B2B"
bigdata <- TRUE
bigdataFileIndicator <- ifelse(bigdata, "big", "small")
outsample_start <- 2011 * 12 #Dec 2010
outsample_end <- 2015 * 12 #Dec 2014
forecast_horizon <- 24
update_interval <- ifelse(bigdata, 48, 6) # parameters update every 6 months
end_month <- 2015 * 12

# process raw data
if(bigdata) {
  load('AggData_zero_substitute.RData')
#  load('AggData.RData')
} else {
  load('AggData.RData')
}
#load("MonthlyTweets.RData")
load("MonthlyTweetsGrowth.RData")

data <- data[data$term > 0,]
data <- data[which(data$terminmonths > 0),]
data <- data[!is.na(data$stategroup),]
data <- data[!is.na(data$industry),]
data <- droplevels(data)

NROWS <- 26796
if(nrow(monthlyTweets) != NROWS) {
  stop('unexpected number of rows') # sanity check
}
if('fb_exists' %in% colnames(data)) {
  if(sum(data$fb_exists == 'true') != NROWS) {
    stop('unexpected number of rows') # sanity check  
  }
  data$rowIndex <- 0
  data$rowIndex[data$fb_exists == 'true'] <- 1:NROWS
} else {
  if(nrow(data) != NROWS) {
    stop('unexpected number of rows') # sanity check  
  }
  data$rowIndex <- 1:NROWS
}

data$keyword <- NULL
data$screenname <- NULL
data$fb_exists <- NULL
data$exist <- NULL

if(industryTag != "") {
  data <- data[data$industry %in% industry, ]
}

#save some memory
if(bigdata) {
  dataExpanded <- Expand(data[data$start_month_num < outsample_start + 1,])
  dataExpanded <- dataExpanded[dataExpanded$start_month_num + dataExpanded$monthindex < outsample_start + 1,]
} else {
  dataExpanded <- Expand(data)
}
dataExpanded <- IncorporateExternalData(dataExpanded)

data$monthindex <- 0
data <- IncorporateExternalData(data)

#replace adjustedstatuscount with monthly tweet #, both in data and dataExpanded
colIndexOffset <- 2006*12 + 1 - 1 # 2006*12+1 corresponds to Jan2006, start of the monthly tweet time series, -1 for the lag
affectedRows <- data$rowIndex > 0
data$adjustedstatuscount <- 0.0
data$adjustedstatuscount[affectedRows] <- as.numeric(monthlyTweets[cbind(data$rowIndex[affectedRows], data$start_month_num[affectedRows] - colIndexOffset)])
affectedRows <- dataExpanded$rowIndex > 0
dataExpanded$adjustedstatuscount <- 0.0
dataExpanded$adjustedstatuscount[affectedRows] <- as.numeric(monthlyTweets[cbind(dataExpanded$rowIndex[affectedRows], dataExpanded$start_month_num[affectedRows] + dataExpanded$monthindex[affectedRows] - colIndexOffset)])
data$rowIndex <- NULL
dataExpanded$rowIndex <- NULL
affectedRows <- NULL
monthlyTweets <- NULL

if(bigdata) {
  data <- data[order(getFactorOrders(data$stategroup)), ]
  dataExpanded <- dataExpanded[order(getFactorOrders(dataExpanded$stategroup)), ]
  data <- as.ffdf(data)
  dataExpanded <- as.ffdf(dataExpanded)
}

defaultBernoulli <- c()
defaultMonthly <- c()
predSocialBernoulli <- c()
predSocialMonthly <- c()
predEconBernoulli <- c()
predEconMonthly <- c()
predHybridBernoulli <- c()
predHybridMonthly <- c()

for(cutoff_month in seq(outsample_start+1, outsample_end, update_interval)) {
  print(cutoff_month)
  if(bigdata) {
    BernoulliInSample <- subset.ffdf(data, start_month_num + terminmonths < cutoff_month)
    MonthlyInSample <- subset.ffdf(dataExpanded, start_month_num + monthindex < cutoff_month)
    BernoulliOutSample <- subset.ffdf(data, start_month_num >= cutoff_month & start_month_num < cutoff_month + update_interval & start_month_num + terminmonths <= end_month)
    MonthlyOutSample <- subset.ffdf(data, start_month_num >= cutoff_month & start_month_num < cutoff_month + update_interval & start_month_num <= end_month - forecast_horizon & terminmonths >= forecast_horizon)
    BernoulliOutSample <- as.data.frame(BernoulliOutSample)
    MonthlyOutSample <- as.data.frame(MonthlyOutSample)
  } else {
    BernoulliInSample <- data[data$start_month_num + data$terminmonths < cutoff_month,]
    MonthlyInSample <- dataExpanded[dataExpanded$start_month_num + dataExpanded$monthindex < cutoff_month,]
    BernoulliOutSample <- data[data$start_month_num >= cutoff_month & data$start_month_num < cutoff_month + update_interval & data$start_month_num + data$terminmonths <= end_month,]
    MonthlyOutSample <- data[data$start_month_num >= cutoff_month & data$start_month_num < cutoff_month + update_interval & data$start_month_num <= end_month - forecast_horizon & data$terminmonths >= forecast_horizon,]
  }
  
  if(nrow(BernoulliOutSample) == 0 & nrow(MonthlyOutSample) == 0) {
    next
  }
  year <- (cutoff_month - 1) %/% 12
  month <- cutoff_month - year * 12

  models <- GetSelectedModelsAIC(BernoulliInSample, MonthlyInSample, bigdata)
  print('AIC model done')
  if(cutoff_month == outsample_start+1) {
    fullModels <- GetFullModels(BernoulliInSample, MonthlyInSample, bigdata)
    PrintInSampleLogLikelihood()
    PrintTablesComparison(fullModels, models, bigdata, sprintf('Comparison_%d_%02d_%s%s', year, month, ifelse(bigdata, 'Big', 'Small'), industryTag))
    stop()
  }
  
  if(nrow(BernoulliOutSample) > 0) {
    defaultBernoulli <- c(defaultBernoulli, GetDefault(BernoulliOutSample, forecast_horizon, TRUE))
    predEconBernoulli <- c(predEconBernoulli, GetPred(models$econ_bernoulli, BernoulliOutSample, forecast_horizon, TRUE))
    predHybridBernoulli <- c(predHybridBernoulli, GetPred(models$hybrid_bernoulli, BernoulliOutSample, forecast_horizon, TRUE))
    predSocialBernoulli <- c(predSocialBernoulli, GetPred(models$social_bernoulli, BernoulliOutSample, forecast_horizon, TRUE))
  }
  if(nrow(MonthlyOutSample) > 0) {
    defaultMonthly <- c(defaultMonthly, GetDefault(MonthlyOutSample, forecast_horizon, FALSE))
    predSocialMonthly <- c(predSocialMonthly, GetPred(models$social_monthly, MonthlyOutSample, forecast_horizon, FALSE))
    predEconMonthly <- c(predEconMonthly, GetPred(models$econ_monthly, MonthlyOutSample, forecast_horizon, FALSE))
    predHybridMonthly <- c(predHybridMonthly, GetPred(models$hybrid_monthly, MonthlyOutSample, forecast_horizon, FALSE))
  }
}

rocSocialBernoulli  <- GetRoc(predSocialBernoulli, defaultBernoulli)
aucSocialBernoulli  <- GetAuc(predSocialBernoulli, defaultBernoulli)
rocEconBernoulli   <- GetRoc(predEconBernoulli, defaultBernoulli)
aucEconBernoulli   <- GetAuc(predEconBernoulli, defaultBernoulli)
rocHybridBernoulli  <- GetRoc(predHybridBernoulli, defaultBernoulli)
aucHybridBernoulli  <- GetAuc(predHybridBernoulli, defaultBernoulli)

rocSocialMonthly  <- GetRoc(predSocialMonthly, defaultMonthly)
aucSocialMonthly  <- GetAuc(predSocialMonthly, defaultMonthly)
rocEconMonthly   <- GetRoc(predEconMonthly, defaultMonthly)
aucEconMonthly   <- GetAuc(predEconMonthly, defaultMonthly)
rocHybridMonthly  <- GetRoc(predHybridMonthly, defaultMonthly)
aucHybridMonthly  <- GetAuc(predHybridMonthly, defaultMonthly)

pdf(paste('Plots/roc_bernoulli_', bigdataFileIndicator, industryTag, '.pdf', sep = ''))
title <- 'Binary Forecast Model'
plot(unlist(rocSocialBernoulli@x.values), unlist(rocSocialBernoulli@y.values), type = 'l', xlab = 'False Positive Rate', ylab = 'True Positive Rate', main = title)
par(new=TRUE)
plot(unlist(rocEconBernoulli@x.values), unlist(rocEconBernoulli@y.values), type = 'l', xlab = '', ylab = '', col = 'blue')
par(new=TRUE)
plot(unlist(rocHybridBernoulli@x.values), unlist(rocHybridBernoulli@y.values), type = 'l', xlab = '', ylab = '', col = 'red')

labelSocial <- paste('Internet AUC=', as.character(round(aucSocialBernoulli,4)), sep = '')
labelEcon   <- paste('Traditional AUC=', as.character(round(aucEconBernoulli,4)), sep = '')
labelHybrid <- paste('Hybrid AUC=', as.character(round(aucHybridBernoulli,4)), sep = '')

legend(0.5, 0.2, c(labelSocial, labelEcon, labelHybrid), 
       lty = c(1,1,1), lwd = c(2.5, 2.5, 2.5), col = c('black', 'blue', 'red'))
dev.off()

pdf(paste('Plots/roc_', bigdataFileIndicator, industryTag, '.pdf', sep = ''))
title <- paste('Monthly Forecast Model with ', as.character(forecast_horizon), '-month Forecast Horizon', sep = '')
plot(unlist(rocSocialMonthly@x.values), unlist(rocSocialMonthly@y.values), type = 'l', xlab = 'False Positive Rate', ylab = 'True Positive Rate', main = title)
par(new=TRUE)
plot(unlist(rocEconMonthly@x.values), unlist(rocEconMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'blue')
par(new=TRUE)
plot(unlist(rocHybridMonthly@x.values), unlist(rocHybridMonthly@y.values), type = 'l', xlab = '', ylab = '', col = 'red')

labelSocial <- paste('Internet AUC=', as.character(round(aucSocialMonthly,4)), sep = '')
labelEcon   <- paste('Traditional AUC=', as.character(round(aucEconMonthly,4)), sep = '')
labelHybrid <- paste('Hybrid AUC=', as.character(round(aucHybridMonthly,4)), sep = '')

legend(0.5, 0.2, c(labelSocial, labelEcon, labelHybrid), 
       lty = c(1,1,1), lwd = c(2.5, 2.5, 2.5), col = c('black', 'blue', 'red'))
dev.off()

AUC.test(predEconBernoulli, defaultBernoulli, predHybridBernoulli, defaultBernoulli)
AUC.test(predEconMonthly, defaultMonthly, predHybridMonthly, defaultMonthly)
#print(getLogLikelihood(predSocialBernoulli, defaultBernoulli) / length(defaultBernoulli))
#print(getLogLikelihood(predEconBernoulli, defaultBernoulli) / length(defaultBernoulli))
#print(getLogLikelihood(predHybridBernoulli, defaultBernoulli) / length(defaultBernoulli))
#print(getLogLikelihood(predSocialMonthly, defaultMonthly) / length(defaultMonthly))
#print(getLogLikelihood(predEconMonthly, defaultMonthly) / length(defaultMonthly))
#print(getLogLikelihood(predHybridMonthly, defaultMonthly) / length(defaultMonthly))
