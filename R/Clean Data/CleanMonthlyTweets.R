colClasses <- c('character', rep("numeric", 12))
startYear <- 2006
endYear <- 2014

monthlyTweets <- read.table(sprintf("../Data/Monthly Tweet Count %04d.txt", startYear), colClasses = colClasses, header = FALSE, quote = "\"", sep = "\t", fill = TRUE);
colIdx <- 13
for(year in (startYear+1):endYear) {
  toAppend <- read.table(sprintf("../Data/Monthly Tweet Count %04d.txt", year), colClasses = colClasses, header = FALSE, quote = "\"", sep = "\t", fill = TRUE);
  for(month in 1:12) {
    colIdx <- colIdx + 1
    colName <- sprintf("V%d", colIdx)
    monthlyTweets[,colName] <- toAppend[,month+1]
  }
}

#start month is Jan 2006
save(monthlyTweets, file='MonthlyTweets.RData')

#generate and save the growth time series as well
n <- ncol(monthlyTweets)
for(colIdx in n:3) {
  monthlyTweets[,colIdx] <- (monthlyTweets[,colIdx] + 1) / (monthlyTweets[,colIdx-1] + 1) - 1
}
monthlyTweets[, 2] <- 0
save(monthlyTweets, file='MonthlyTweetsGrowth.RData')
