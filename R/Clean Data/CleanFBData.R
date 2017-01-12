outputFolder <- "../Data/FBPostProcess/";
numBatch <- 741;
batchSize <- 600;
numRow <- 444039;

FBData <- data.frame(index      = numeric(numRow),
                     keyword    = character(numRow),
                     fb_page_id = character(numRow),
                     fb_likes   = numeric(numRow),
                     fb_exists  = character(numRow)
)

FBData$keyword <- as.character(FBData$keyword)
FBData$fb_page_id <- as.character(FBData$fb_page_id)
FBData$fb_exists <- as.character(FBData$fb_exists)
# some ugly ass code, but that's all I can think of!

colClasses = c("numeric", "character", "character", "numeric", "character")
for(batchIndex in 0:(numBatch-1)) {
  if(batchIndex %% 10 == 0) print(batchIndex);
  BatchData <- read.table(sprintf("%spart-%04d.txt", outputFolder, batchIndex), colClasses = colClasses, header = FALSE, quote = "\"", sep = "\t", fill = TRUE);
  startRow <- batchIndex * batchSize + 1
  endRow <- startRow + nrow(BatchData) - 1
  FBData[startRow:endRow,] <- BatchData;
}
save(FBData, file='FBData.RData')
