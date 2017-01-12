library(twitteR)
library(plyr)

getUserTweets <- function(username, count, save = TRUE) {
  tweets <- userTimeline(username, count) 
  tweetsText <- sapply(tweets, function(x) x$text)
  fileConn <- file(paste('../opinionfinder/database/docs/sentiment/', username, '_user', sep = ''))
  writeLines(tweetsText, fileConn)
  close(fileConn)
  return(tweetsText)
}

getFollowersCount <- function(username) {
  user <- getUser(username)
  count <- user$followersCount
  return(count)
}

getTweetsWithHashTag <- function(username, hashtag, startDate, endDate, count, save = TRUE) {
  tweets <- searchTwitter(hashtag, since = startDate, until = endDate, n = count)
  tweetsText = sapply(tweets, function(x) x$getText())
  fileConn <- file(paste('../opinionfinder/database/docs/sentiment/', username, '_hashtag', sep = ''))
  writeLines(tweetsText, fileConn)
  close(fileConn)
  return(tweetsText)
}

api_key <- 'REUR3kn6BfEpsoxb8yVjS0EDH'
api_secret <- '3TSYaSZ3cjKDYPQKhc5JjSux0c7Ym908wphx3ebEOBbn7coCiU'
access_token <- "1656896509-ZB863KPYi4eMNJPmjCiD5NixLVxErrNIyBA7qu3"
access_token_secret <- "RiBLJ7R47eRutab5QlNdMmy42UdVjHtXrUiP3zjIUC0O2"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

username <- 'Betterment'
hashTag <- '#betterment'
userTweets <- getUserTweets(username, 1000)
tweetsHashTag <- getTweetsWithHashTag(username, hashTag, "2014-12-20", "2015-01-01", 1000)
followersCount <- getFollowersCount(username)
