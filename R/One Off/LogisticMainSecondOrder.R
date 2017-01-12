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
bigdata <- FALSE
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

defaultMonthly <- c()
predFull <- c()
predAIC <- c()
predAICOld <- c()
for(cutoff_month in seq(outsample_start+1, outsample_end, update_interval)) {
  print(cutoff_month)
  MonthlyInSample <- dataExpanded[dataExpanded$start_month_num + dataExpanded$monthindex < cutoff_month,]
  MonthlyOutSample <- data[data$start_month_num >= cutoff_month & data$start_month_num < cutoff_month + update_interval & data$start_month_num <= end_month - forecast_horizon & data$terminmonths >= forecast_horizon,]
  
  if(nrow(MonthlyOutSample) == 0) {
    next
  }
  year <- (cutoff_month - 1) %/% 12
  month <- cutoff_month - year * 12

  mod.full <-   glm(default ~ unemployment + 
                      google_trend +
                      log(fb_likes+1) +
                      log(adjustedfollowercount+1) + 
                      log(adjustedfriendcount+1) +
                      adjustedstatuscount +
                      I(unemployment ^2) +
                      I(google_trend ^2) +
                      I(log(fb_likes+1) ^2) +
                      I(log(adjustedfollowercount+1) ^2)+ 
                      I(log(adjustedfriendcount+1) ^2)+
                      I(adjustedstatuscount ^2) +
                      unemployment * google_trend +
                      unemployment * log(fb_likes+1) +
                      unemployment * log(adjustedfollowercount+1) +
                      unemployment * log(adjustedfriendcount+1) +
                      unemployment * adjustedstatuscount +
                      google_trend * log(fb_likes+1)+
                      google_trend * log(adjustedfollowercount+1)+
                      google_trend * log(adjustedfriendcount+1)+
                      google_trend * adjustedstatuscount+
                      log(fb_likes+1) * log(adjustedfollowercount+1)+
                      log(fb_likes+1) * log(adjustedfriendcount+1)+
                      log(fb_likes+1) * adjustedstatuscount+
                      log(adjustedfollowercount+1) * log(adjustedfriendcount+1) + 
                      log(adjustedfollowercount+1) * adjustedstatuscount +
                      log(adjustedfriendcount+1) * adjustedstatuscount,                    
                    family = binomial(),
                    data = MonthlyInSample)

  mod.aic <- glm(default ~unemployment + 
                   google_trend +
                   log(fb_likes + 1) + 
                   log(adjustedfollowercount + 1) +
                   log(adjustedfriendcount + 1) +
                   I(unemployment^2) +
                   I(log(adjustedfollowercount + 1)^2) +
                   I(google_trend*log(fb_likes + 1)) + 
                   I(log(adjustedfollowercount + 1)*log(fb_likes + 1)),
                 family = binomial(),
                 data = MonthlyInSample)
  
  mod.aic.old <- glm(default ~ unemployment + log(adjustedfollowercount + 1),
                     family = binomial(),
                     data = MonthlyInSample)
  
  if(cutoff_month == outsample_start+1) {
    mod.full.first <- mod.full
    mod.aic.first <- mod.aic
    mod.aic.old.first <- mod.aic.old
    
    defaultMonthlyIS <- GetDefault(MonthlyInSample, forecast_horizon, FALSE)
    predFullIS <- GetPred(mod.full, MonthlyInSample, forecast_horizon, FALSE)
    predAICIS <- GetPred(mod.aic, MonthlyInSample, forecast_horizon, FALSE)

    print(getLogLikelihood(predFullIS, defaultMonthlyIS) / length(defaultMonthlyIS))
    print(getLogLikelihood(predAICIS, defaultMonthlyIS) / length(defaultMonthlyIS))
  }
  
  if(nrow(MonthlyOutSample) > 0) {
    defaultMonthly <- c(defaultMonthly, GetDefault(MonthlyOutSample, forecast_horizon, FALSE))
    predFull <- c(predFull, GetPred(mod.full, MonthlyOutSample, forecast_horizon, FALSE))
    predAIC <- c(predAIC, GetPred(mod.aic, MonthlyOutSample, forecast_horizon, FALSE))
    predAICOld <- c(predAICOld, GetPred(mod.aic.old, MonthlyOutSample, forecast_horizon, FALSE))
  }
}

rocFull  <- GetRoc(predFull, defaultMonthly)
aucFull  <- GetAuc(predFull, defaultMonthly)
rocAIC  <- GetRoc(predAIC, defaultMonthly)
aucAIC  <- GetAuc(predAIC, defaultMonthly)
rocAICOld  <- GetRoc(predAICOld, defaultMonthly)
aucAICOld  <- GetAuc(predAICOld, defaultMonthly)

pdf('roc_second_order.pdf')
title <- 'Monthly Forecast Model'
plot(unlist(rocFull@x.values), unlist(rocFull@y.values), type = 'l', xlab = 'False Positive Rate', ylab = 'True Positive Rate', main = title)
par(new=TRUE)
plot(unlist(rocAIC@x.values), unlist(rocAIC@y.values), type = 'l', xlab = '', ylab = '', col = 'blue')
par(new=TRUE)
plot(unlist(rocAICOld@x.values), unlist(rocAICOld@y.values), type = 'l', xlab = '', ylab = '', col = 'red')

labelFull <- paste('1st+2nd order, full, AUC=', as.character(round(aucFull,4)), sep = '')
labelAIC   <- paste('1st+2nd order, AIC, AUC=', as.character(round(aucAIC,4)), sep = '')
labelAICOld <- paste('1st order only, AIC, AUC=', as.character(round(aucAICOld,4)), sep = '')

legend(0.4, 0.2, c(labelFull, labelAIC, labelAICOld), 
       lty = c(1,1,1), lwd = c(2.5, 2.5, 2.5), col = c('black', 'blue', 'red'))
dev.off()