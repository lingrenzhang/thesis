outputFolder <- "../Data/PostProcess/";
numBatch <- 741;
batchSize <- 600;
numRow <- 444039;
numCol <- 9

TwitterData <- data.frame(index = numeric(numRow),
                          keyword = character(numRow),
                          screenname = character(numRow),
                          followercount = numeric(numRow),
                          friendcount = numeric(numRow),
                          statuscount = numeric(numRow),
                          uniqueamongraw = character(numRow),
                          uniqueamongfiltered = character(numRow),
                          matchingscore = numeric(numRow)
)

TwitterData$keyword <- as.character(TwitterData$keyword)
TwitterData$screenname <- as.character(TwitterData$screenname)
TwitterData$uniqueamongraw <- as.character(TwitterData$uniqueamongraw)
TwitterData$uniqueamongfiltered <- as.character(TwitterData$uniqueamongfiltered)
# some ugly ass code, but that's all I can think of!

colClasses = c("numeric", "character", "character", "numeric", "numeric", "numeric", "character", "character", "numeric" )
for(batchIndex in 0:(numBatch-1)) {
  if(batchIndex %% 10 == 0) print(batchIndex);
  BatchData <- read.table(sprintf("%spart-%04d.txt", outputFolder, batchIndex), colClasses = colClasses, header = FALSE, quote = "\"", sep = "\t", fill = TRUE);
  startRow <- batchIndex * batchSize + 1
  endRow <- startRow + nrow(BatchData) - 1
  TwitterData[startRow:endRow,] <- BatchData;
}
#colnames(TwitterData) <- c('index', 'keyword', 'screenname', 
#                           'followercount', 'friendcount', 'statuscount', 
#                           'uniqueamongraw', 'uniqueamongfiltered', 'matchingscore');
save(TwitterData, file='TwitterData.RData')
