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
#write(data$exist, file = "../Data/TwitterAccountExist.txt", ncolumns = 1)

medianFollower <- 0
medianFriend <- 0
medianStatus <- 0

#-----------test only----------------
#medianFollower <- median(data$followercount[data$exist])
#medianFriend <- median(data$friendcount[data$exist])
#medianStatus <- median(data$statuscount[data$exist])
#medianLikes <- median(data$fb_likes[data$exist])
#data$fb_likes[!data$exist] <- medianLikes
#-----------test only----------------

data$adjustedfollowercount <- ifelse(data$exist, data$followercount, medianFollower)
data$adjustedfriendcount <- ifelse(data$exist, data$friendcount, medianFriend)
data$adjustedstatuscount <- ifelse(data$exist, data$statuscount, medianStatus)

# only look at companies with twitter accounts and fb pages
data <- data[which(data$exist),]
data <- data[which(data$fb_exists == 'true'),]
data <- data[which(data$status != "CANCLD"), ]

#data$screenname <- NULL
data$followercount <- NULL
data$friendcount <- NULL
data$statuscount <- NULL
data$uniqueamongraw <- NULL
data$uniqueamongfiltered <- NULL
data$matchingscore <- NULL
data$fb_exists <- NULL
data$exist <- NULL

save(data, file = 'AggData.RData')
#save(data, file = 'AggData_zero_substitute.RData')

