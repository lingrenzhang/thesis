load('SBAData.RData')
SBAData <- SBAData[SBAData$status!='CANCLD',]
pdf('Plots/NumLoansByLoanTerm.pdf')
hist(SBAData$terminmonths[SBAData$terminmonths <= 132], breaks = 132, main = '# Loans By Loan Term', xlab = '# Months in Loan Term', ylab = '# Loans', xaxt='n')
axis(side=1, at=seq(0,120,12), label = seq(0,120,12))
dev.off()

source('..//LogisticLib.R')
load('../AggData.RData')
data <- data[which(data$term > 0),]
data <- data[which(data$terminmonths > 0),]
data <- data[!is.na(data$stategroup),]
dataExpanded <- Expand(data)
numDefaultsByMonth <- aggregate(dataExpanded$default, by = list(dataExpanded$start_month_num + dataExpanded$monthindex), FUN = sum)
numLoansByMonth <- aggregate(dataExpanded$default, by = list(dataExpanded$start_month_num + dataExpanded$monthindex), FUN = length)
year <- numDefaultsByMonth$Group.1/12
defaultRate <- numDefaultsByMonth$x / numLoansByMonth$x
pdf('MonthlyDefaultRates.pdf')
f3 <- rep(1/3, 3)
plot(year, filter(defaultRate, f3, sides = 1), type = 'l', ylim = c(0, 0.012),
     xlab = 'Year', ylab = 'Monthly Default Rate', main = 'Monthly Default Rates (3-month moving average)')  
dev.off()