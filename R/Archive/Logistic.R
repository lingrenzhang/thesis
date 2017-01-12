library(MASS)
library(ROCR)

load('AggData.RData')
data <- data[which(data$term > 0),]
data <- data[which(data$terminmonths > 0),]

#expand into monthly data points
expand <- function(input) {
  output <- input[rep(row.names(input), input$term), ]
  monthIndexList <- lapply(input$term, function(n){0:(n-1)})
  output$monthindex <- unlist(monthIndexList)
  defaultIdx <- which((output$default == 1) & (output$monthindex == (output$term - 1)))
  output$default <- 0
  output$default[defaultIdx] <- 1
  output$term_ratio <- output$monthindex / output$terminmonths
  output
}

PrintCoef <- function(coefficients, filename) {
  fileConn <- file(filename)
  lines <- c('\\begin{table}[h]', '\\begin{center}', '\\begin{tabular}{|l|r|r|}', '\\hline');
  lines <- c(lines, 'Feature & Coefficient & p-value \\\\ \\hline');
  
  rows <- rownames(coefficients);
  for(i in 1:length(rows)) {
    lines <- c(lines, paste(
      rows[i], ' & ',
      as.character(round(coefficients[i,1],4)), ' & ',
      format(coefficients[i,4], scientific =  TRUE), ' \\\\ \\hline',
      sep = ''))
  }
  
  lines <- c(lines, '\\end{tabular}', '\\end{center}', '\\end{table}');
  writeLines(lines, fileConn)
  close(fileConn)  
}

GetRoc <- function(mod, data, forecast_horizon) {
  pred <- predict(mod, data, type = "response")
  default_within_horizon <- data$default & (data$term <= forecast_horizon)
  
  # convert from monthly default rate to default rate within forecast_horizon
  pred <- pmin(pred,1)
  end_month <- 2014 * 12
  num_months <- pmin(end_month - data$start_month_num, data$terminmonths, forecast_horizon)
  pred_within_horizon <- 1 - (1-pred)^num_months
    
  predObj <- prediction(pred_within_horizon, default_within_horizon)
  performance(predObj, "tpr", "fpr")
}

GetAuc <- function(mod, data, forecast_horizon) {
  pred <- predict(mod, data, type = "response")
  default_within_horizon <- data$default & (data$term <= forecast_horizon)
  
  # convert from monthly default rate to default rate within forecast_horizon
  pred <- pmin(pred,1) # probably do not need this guy
  end_month <- 2014 * 12
  num_months <- pmin(end_month - data$start_month_num, data$terminmonths, forecast_horizon)
  pred_within_horizon <- 1 - (1-pred)^num_months
  
  predObj <- prediction(pred_within_horizon, default_within_horizon)
  auc <- performance(predObj, "auc")
  auc@y.values[[1]]
}

IncorporateExternalData <- function(dataExpanded) {
  # incorporate unemployment rate
  raw_data <- read.csv("../Data/unemployment_estimates.csv", colClasses = c("character", rep("numeric", 3)), header = TRUE)
  
  date <- as.Date(raw_data$date, '%m/%d/%Y')
  month_num <- as.numeric(format(date, '%m')) + as.numeric(format(date, '%Y')) * 12
  idx <- match(dataExpanded$start_month_num + dataExpanded$monthindex, month_num)
  unemployment <- raw_data$prediction[idx]
  dataExpanded$unemployment <- unemployment
  
  # incorporate google trend (by industry)
  raw_data <- read.csv("../Data/Google Trend Monthly.csv", colClasses = c("character", rep("numeric", 20)), header = TRUE)
  year <- as.numeric(substr(raw_data$Date, 1, 4))
  month <- as.numeric(substr(raw_data$Date, 6, 7))
  month_num <- year * 12 + month
  
  row_idx <- match(dataExpanded$start_month_num + dataExpanded$monthindex, month_num)
  col_names <- c('Date', 'Agriculture, Forestry, Fishing and Hunting', 'Mining', 'Utilities',
                 'Construction', 'Manufacturing', 'Wholesale Trade', 'Retail Trade',
                 'Transportation and Warehousing', 'Information', 'Finance and Insurance',
                 'Real Estate Rental and Leasing', 'Professional, Scientific, and Technical Services',
                 'Management of Companies and Enterprises', 
                 'Administrative and Support and Waste Management and Remediation Services',
                 'Educational Services', 'Health Care and Social Assistance', 'Arts, Entertainment, and Recreation',
                 'Accommodation and Food Services', 'Other Services (except Public Administration)',
                 'Public Administration')
  
  dataExpanded$google_trend <- 0
  
  cols <- unique(dataExpanded$industry)
  for(col_name in cols) {
    col_idx <- match(col_name, col_names)
    col_value <- raw_data[,col_idx]
    destination_row_idx_vec <- which(dataExpanded$industry == col_name)
    dataExpanded$google_trend[destination_row_idx_vec] <- col_value[row_idx[destination_row_idx_vec]]
  }
  
  dataExpanded
}

dataExpanded <- expand(data)
dataExpanded <- IncorporateExternalData(dataExpanded)

# cutoff is 1/1/2011
cutoff_year <- 2011
cutoff_month <- cutoff_year * 12
inSampleIdx <- which(dataExpanded$start_month_num + dataExpanded$monthindex <= cutoff_month)
inSampleExpanded <- dataExpanded[inSampleIdx,]

# don't expand out sample
forecast_horizon <- 36
#end_month <- 2014 * 12
outSampleIdx <- which(data$start_month_num > cutoff_month)# & data$start_month_num <= end_month - forecast_horizon)
outSample <- data[outSampleIdx,]
outSample$monthindex <- 0
outSample <- IncorporateExternalData(outSample)

#outSampleIdx <- sample(1:nrow(dataExpanded), round(nrow(dataExpanded)/5), replace = FALSE)
#inSampleExpanded <- dataExpanded[-outSampleIdx, ]
#outSampleExpanded <- dataExpanded[outSampleIdx, ]

#social
mod.social <- glm(default ~ log(adjustedfollowercount+1) + 
                            log(adjustedfriendcount+1) +
                            log(adjustedstatuscount+1) +
                            unemployment + 
                            google_trend,
                family = "binomial",
                data = inSampleExpanded)

#economical
mod.econ <- glm(default ~ log(grossamountinthousand) + 
                    guaranteedratio + 
                    terminmonths + 
                    borrowerbanksamestate + 
                    termisfullyear + 
                    stategroup,
                  family = "binomial",
                  data = inSampleExpanded)
#mod.econ.cv <- stepAIC(mod.econ, direction = "backward", k = log(nrow(data)))
PrintCoef(summary(mod.econ)$coefficients, 'Tables\\LogisticEcon.txt')

#hybrid
mod.hybrid <- glm(default ~ log(grossamountinthousand) + 
                      guaranteedratio + 
                      terminmonths + 
                      borrowerbanksamestate + 
                      termisfullyear + 
                      stategroup +
                      log(adjustedfollowercount+1) + 
                      log(adjustedfriendcount+1) +
                      log(adjustedstatuscount+1) +
                      unemployment + 
                      google_trend,
                  family = "binomial",
                  data = inSampleExpanded)
#mod.hybrid.cv <- stepAIC(mod.hybrid, direction = "backward", k = log(nrow(data)))
PrintCoef(summary(mod.hybrid)$coefficients, 'Tables\\LogisticHybrid.txt')

rocSocial <- GetRoc(mod.social, outSample, forecast_horizon)
aucSocial <- GetAuc(mod.social, outSample, forecast_horizon)
rocEcon   <- GetRoc(mod.econ,   outSample, forecast_horizon)
aucEcon   <- GetAuc(mod.econ,   outSample, forecast_horizon)
rocHybrid <- GetRoc(mod.hybrid, outSample, forecast_horizon)
aucHybrid <- GetAuc(mod.hybrid, outSample, forecast_horizon)

png('roc.png')
plot(unlist(rocSocial@x.values), unlist(rocSocial@y.values), type = 'l', xlab = 'False Positive Rate', ylab = 'True Positive Rate')
par(new=TRUE)
plot(unlist(rocEcon@x.values), unlist(rocEcon@y.values), type = 'l', xlab = '', ylab = '', col = 'blue')
par(new=TRUE)
plot(unlist(rocHybrid@x.values), unlist(rocHybrid@y.values), type = 'l', xlab = '', ylab = '', col = 'red')

labelSocial <- paste('Internet AUC=', as.character(round(aucSocial,4)), sep = '')
labelEcon   <- paste('Traditional AUC=', as.character(round(aucEcon,4)), sep = '')
labelHybrid <- paste('Hybrid AUC=', as.character(round(aucHybrid,4)), sep = '')

legend(0.5, 0.2, c(labelSocial, labelEcon, labelHybrid), 
       lty = c(1,1,1), lwd = c(2.5, 2.5, 2.5), col = c('black', 'blue', 'red'))
dev.off()
