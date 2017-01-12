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
industry <- ""
bigdata <- FALSE
bigdataFileIndicator <- ifelse(bigdata, "big", "small")
outsample_start <- 2011 * 12 #Dec 2010
outsample_end <- 2015 * 12 #Dec 2014
forecast_horizon <- 24
update_interval <- 48
end_month <- 2015 * 12

# process raw data
if(bigdata) {
  load('AggData_zero_substitute.RData')
} else {
  load('AggData.RData')
}

data <- data[data$term > 0,]
data <- data[which(data$terminmonths > 0),]
data <- data[!is.na(data$stategroup),]
data <- data[!is.na(data$industry),]
data <- droplevels(data)
data$keyword <- NULL
data$fb_exists <- NULL
data$exist <- NULL
data <- data[which(end_month - data$start_month_num >= data$terminmonths),]

max_month = 60
term <- 1:max_month
dp <- c()
for(t in term) {
  filter_data <- data[data$terminmonths == t, ]
  dp <- c(dp, sum(filter_data$default)/nrow(filter_data))
}
table <- matrix(dp, ncol = max_month, byrow = TRUE)
colnames(table) <- term
pdf("term_is_full_year.pdf")
b <- barplot(table, col = rep('blue', max_month),
        xlab = "Term in Months", ylab = "Default Probability", main = "Empirical Default Probability by Term in Months", axes = FALSE, axisnames = FALSE)
axis(side = 2, pos = -0.2)
at_tick <- seq(12, max_month, 12)
axis(side = 1, at = c(b[1]*2 - b[2], b[at_tick]), xpd = TRUE, labels = seq(0,max_month,12))
dev.off()
