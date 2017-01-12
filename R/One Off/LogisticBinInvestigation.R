library(MASS)
library(ROCR)
library(MKmisc)
source('LogisticLib.R')

# process raw data
load('AggData.RData')
data <- data[which(data$term > 0),]
data <- data[which(data$terminmonths > 0),]
data <- data[!is.na(data$stategroup),]

dataExpanded <- Expand(data)
dataExpanded <- IncorporateExternalData(dataExpanded)
data$monthindex <- 0
data <- IncorporateExternalData(data)
data <- AppendTSAverages(data, dataExpanded)

outsample_start <- 2011 * 12 #Dec 2010
outsample_end <- 2015 * 12 #Dec 2014
forecast_horizon <- 24
end_month <- 2015 * 12

cutoff_month <- outsample_start+1
BernoulliInSample <- data[data$start_month_num + data$terminmonths < cutoff_month,]
BernoulliOutSample <- data[data$start_month_num >= cutoff_month & data$start_month_num + data$terminmonths <= end_month,]
mod.social.bernoulli <- glm(default ~ avg_unemployment + 
                              log(adjustedfollowercount+1), 
                            family = "binomial",
                            data = BernoulliInSample)
  
defaultBernoulli <- GetDefault(BernoulliOutSample, forecast_horizon, TRUE)
predSocialBernoulli <- GetPred(mod.social.bernoulli, BernoulliOutSample, forecast_horizon, TRUE)

rocSocialBernoulli  <- GetRoc(predSocialBernoulli, defaultBernoulli)
aucSocialBernoulli  <- GetAuc(predSocialBernoulli, defaultBernoulli)
plot(unlist(rocSocialBernoulli@x.values), unlist(rocSocialBernoulli@y.values), type = 'l', xlab = 'False Positive Rate', ylab = 'True Positive Rate', main = title)

