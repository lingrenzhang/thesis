raw     <- read.csv("../Data/SBA_7a.csv", header = TRUE)
raw2015 <- read.csv("../Data/SBA_7a_2015.csv", header = TRUE)
batchsize <- 100
for(batch in 1:4000) {
  start <- (batch-1) * batchsize
  end <- batch * batchsize
  raw_names <- as.character(raw$BorrName[start:end])
  raw2015_names <- as.character(raw2015$BorrName[start:end])
  if(raw_names != raw2015_names){
    browser()
  }
}