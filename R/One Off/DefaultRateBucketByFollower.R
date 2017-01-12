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

#buckets <- c(0, 10, 100, 1e8)
buckets <- c(0, 2, 14, 98, 1e8)
colors <- c('black', 'green', 'blue', 'red')
f3 <- rep(1/3, 3)
pdf('DefaultRateBucketByNumFollowers.pdf')
for(i in 1:(length(buckets)-1)) {
  UB <- buckets[i+1]
  LB <- buckets[i]
  bucket <- dataExpanded[(dataExpanded$adjustedfollowercount >= LB & dataExpanded$adjustedfollowercount < UB),]
  numDefaultsByMonth <- aggregate(bucket$default, by = list(bucket$start_month_num + bucket$monthindex), FUN = sum)
  numLoansByMonth <- aggregate(bucket$default, by = list(bucket$start_month_num + bucket$monthindex), FUN = length)
  year <- numDefaultsByMonth$Group.1/12
  defaultRate <- numDefaultsByMonth$x / numLoansByMonth$x
  plot(year, filter(defaultRate, f3, sides = 1), type = 'l', ylim = c(0, 0.012), col = colors[i],
       xlab = 'Year', ylab = 'Monthly Default Rate', main = 'Monthly Default Rates (3-month moving average)')  
  if(i < (length(buckets) - 1)) {
    par(new=TRUE)
  }
}
legend('topright', c('bottom quartile', '2nd quartile', '3rd quartile', 'top quartile'), 
       lty = c(1,1,1,1), lwd = c(2.5, 2.5, 2.5, 2.5), col = c('black', 'green', 'blue', 'red'))

dev.off()
