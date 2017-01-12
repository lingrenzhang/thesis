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

outsample_start <- 2011 * 12 #Dec 2010
outsample_end <- 2015 * 12 #Dec 2014
forecast_horizon <- 24
update_interval <- 6 # parameters update every 6 months
end_month <- 2015 * 12

defaultBernoulli <- c()
defaultMonthly <- c()
predSocialBernoulli <- c()
predSocialMonthly <- c()
predEconBernoulli <- c()
predEconMonthly <- c()
predHybridBernoulli <- c()
predHybridMonthly <- c()

cutoff_month <- outsample_start + 1

BernoulliInSample <- data[data$start_month_num + data$terminmonths < cutoff_month,]
MonthlyInSample <- dataExpanded[dataExpanded$start_month_num + dataExpanded$monthindex < cutoff_month,]
BernoulliOutSample <- data[data$start_month_num >= cutoff_month & data$start_month_num < cutoff_month + update_interval & data$start_month_num + data$terminmonths <= end_month,]
MonthlyOutSample <- data[data$start_month_num >= cutoff_month & data$start_month_num < cutoff_month + update_interval & data$start_month_num <= end_month - forecast_horizon & data$terminmonths >= forecast_horizon,]
  
fullModels <- GetFullModels(BernoulliInSample, MonthlyInSample)

mod.social.bernoulli <- stepAIC(fullModels$social_bernoulli, direction = "backward", k = log(nrow(BernoulliInSample)))
mod.econ.bernoulli   <- stepAIC(fullModels$econ_bernoulli,   direction = "backward", k = log(nrow(BernoulliInSample)))
mod.hybrid.bernoulli <- stepAIC(fullModels$hybrid_bernoulli, direction = "backward", k = log(nrow(BernoulliInSample)))
mod.social.monthly <- stepAIC(fullModels$social_monthly, direction = "backward", k = log(nrow(MonthlyInSample)))
mod.econ.monthly   <- stepAIC(fullModels$econ_monthly,   direction = "backward", k = log(nrow(MonthlyInSample)))
mod.hybrid.monthly <- stepAIC(fullModels$hybrid_monthly, direction = "backward", k = log(nrow(MonthlyInSample)))

mod.social.bernoulli.aic <- stepAIC(fullModels$social_bernoulli, direction = "backward", k = 2)
mod.econ.bernoulli.aic   <- stepAIC(fullModels$econ_bernoulli,   direction = "backward", k = 2)
mod.hybrid.bernoulli.aic <- stepAIC(fullModels$hybrid_bernoulli, direction = "backward", k = 2)
mod.social.monthly.aic <- stepAIC(fullModels$social_monthly, direction = "backward", k = 2)
mod.econ.monthly.aic   <- stepAIC(fullModels$econ_monthly,   direction = "backward", k = 2)
mod.hybrid.monthly.aic <- stepAIC(fullModels$hybrid_monthly, direction = "backward", k = 2)

mod.social.bernoulli[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.econ.bernoulli[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.hybrid.bernoulli[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.social.monthly[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.econ.monthly[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.hybrid.monthly[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.social.bernoulli.aic[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.econ.bernoulli.aic[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.hybrid.bernoulli.aic[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.social.monthly.aic[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.econ.monthly.aic[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL
mod.hybrid.monthly.aic[c("residuals", "weights", "fitted.values", "prior.weights", "na.action", "linear.predictors", "fitted.values", "effects", "data")] <- NULL


models <- list(
    social_bernoulli_BIC = mod.social.bernoulli,
    econ_bernoulli_BIC = mod.econ.bernoulli,
    hybrid_bernoulli_BIC = mod.hybrid.bernoulli,
    social_monthly_BIC = mod.social.monthly,
    econ_monthly_BIC = mod.econ.monthly,
    hybrid_monthly_BIC = mod.hybrid.monthly,
    social_bernoulli_AIC = mod.social.bernoulli.aic,
    econ_bernoulli_AIC = mod.econ.bernoulli.aic,
    hybrid_bernoulli_AIC = mod.hybrid.bernoulli.aic,
    social_monthly_AIC = mod.social.monthly.aic,
    econ_monthly_AIC = mod.econ.monthly.aic,
    hybrid_monthly_AIC = mod.hybrid.monthly.aic
  )
saveRDS(models, file = "ModelSelection.rds")
