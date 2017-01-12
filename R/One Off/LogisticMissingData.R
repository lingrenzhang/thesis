library(MASS)
library(ROCR)
library(MKmisc)
source('LogisticLib.R')

# process raw data
load('AggData_zero_substitute.RData')

data <- data[which(data$term > 0),]
data <- data[which(data$terminmonths > 0),]
data <- data[!is.na(data$stategroup),]
data <- data[!is.na(data$industry),]
data$monthindex <- 0
data <- IncorporateExternalData(data)

outsample_start <- 2011 * 12 #Dec 2010
cutoff_month <- outsample_start + 1
BernoulliInSample <- data[data$start_month_num + data$terminmonths < cutoff_month,]

mod.social.bernoulli.missing <- glm(default ~ unemployment + 
                              google_trend +
                              log(fb_likes+1) +
                              log(adjustedfollowercount+1) + 
                              log(adjustedfriendcount+1) +
                              log(adjustedstatuscount+1) +
                              exist,
                            family = "binomial",
                            data = BernoulliInSample)

BernoulliInSample <- BernoulliInSample[BernoulliInSample$fb_exist=='true',]
mod.social.bernoulli <- glm(default ~ unemployment + 
                                      google_trend +
                                      log(fb_likes+1) +
                                      log(adjustedfollowercount+1) + 
                                      log(adjustedfriendcount+1) +
                                      log(adjustedstatuscount+1),
                                    family = "binomial",
                                    data = BernoulliInSample)
PrintCoefComparison(summary(mod.social.bernoulli.missing)$coefficients, 
                    summary(mod.social.bernoulli)$coefficients, 
                    'Tables\\Social_Binary_Imputation.txt')
  