load('TwitterData.RData')
load('FBData.RData')
load('SBAData.RData')

n <- nrow(TwitterData)
FBData$index <- NULL
FBData$keyword <- NULL
FBData$fb_page_id <- NULL
FBData$name <- NULL
data <- cbind(SBAData[1:n,], TwitterData, FBData)
data$exist <- (data$uniqueamongraw == 'true')

#clean up in aggData
data$adjustedfollowercount <- ifelse(data$exist, data$followercount, 0)
data$adjustedfriendcount <- ifelse(data$exist, data$friendcount, 0)
data$adjustedstatuscount <- ifelse(data$exist, data$statuscount, 0)

# only look at companies with twitter accounts and fb pages
data <- data[which(data$exist),]
data <- data[which(data$fb_exists == 'true'),]
data <- data[which(data$status != "CANCLD"), ]

data$followercount <- NULL
data$friendcount <- NULL
data$statuscount <- NULL
data$uniqueamongraw <- NULL
data$uniqueamongfiltered <- NULL
data$matchingscore <- NULL

# clean up in main
data <- data[data$term > 0,]
data <- data[which(data$terminmonths > 0),]
data <- data[!is.na(data$stategroup),]
data <- data[!is.na(data$industry),]
data <- droplevels(data)

fileConn <- file('Clean Data\\TwitterScreenNames.txt')
lines <- data$screenname
writeLines(lines, fileConn)
close(fileConn)