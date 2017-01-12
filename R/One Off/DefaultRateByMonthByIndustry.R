library(MASS)
library(ROCR)
library(MKmisc)
source('LogisticLib.R')

IncorporateGoogleTrendV2 <- function(dataExpanded) {
  raw_data <- read.csv("../Data/Google Trend Monthly Diff Lag.csv", colClasses = c("character", rep("numeric", 20)), header = TRUE)
  year <- as.numeric(substr(raw_data$Date, 1, 4))
  month <- as.numeric(substr(raw_data$Date, 6, 7))
  month_num <- year * 12 + month
  
  row_idx <- match(dataExpanded$month, month_num)
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


# process raw data
load('AggData.RData')
data <- data[which(data$term > 0),]
data <- data[which(data$terminmonths > 0),]
data <- data[!is.na(data$stategroup),]
dataExpanded <- Expand(data)

numDefaultsGrouped <- aggregate(dataExpanded$default, 
                   by = list(dataExpanded$start_month_num + dataExpanded$monthindex, dataExpanded$industry), 
                   FUN = sum)
numLoansGrouped <- aggregate(dataExpanded$default, 
                   by = list(dataExpanded$start_month_num + dataExpanded$monthindex, dataExpanded$industry), 
                   FUN = length)
colnames(numDefaultsGrouped) <- c('month', 'industry', 'defaults')
numDefaultsGrouped$rate <- numDefaultsGrouped$defaults / numLoansGrouped$x
numDefaultsGrouped <- IncorporateGoogleTrendV2(numDefaultsGrouped)

plot(numDefaultsGrouped$google_trend, numDefaultsGrouped$rate, ylim=c(0,0.04))
