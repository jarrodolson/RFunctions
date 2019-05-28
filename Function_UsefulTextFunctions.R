EnsurePackage <- function(x){
  x <- as.character(x)
  if(!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

PrepareTwitter <- function(){
  EnsurePackage("bitops")
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("tm")
  EnsurePackage("wordcloud")
  EnsurePackage("PBSmapping")
  EnsurePackage("maptools")
}

TweetFrame <- function(searchTerm,maxTweets){
  ##as.data.frame coerces each list element into a row
  ##lapply applies this to all elements in tweetList
  ##rbind takes all the rows and puts them together
  ##do.call gives rbind to all the rows as individual elements
  tweetList <- searchTwitter(searchTerm,n=maxTweets)
  tweetFrame <- do.call("rbind",lapply(tweetList,as.data.frame))
  return(tweetFrame[order(as.integer(tweetFrame$created)),])
}

ArrivalProbability <- function(times,increment,max){
  plist <- NULL
  timeLen <- length(times)
  if(increment>max){return(NULL)}
  for(i in seq(increment,max,by=increment)){
    ##print(i)
    ##print(sum(as.integer(diff(times))<i))
    val <- sum(as.integer(diff(times))<i)/timeLen
    plist <- c(plist,val)
    ##plist <- c(plist,(sum(as.integer(diff(times))<i))/timeLen)
  }
  return(plist)
}

DelayProbability <- function(delays,increment,max){
  plist <- NULL
  delayLen <- length(delays)
  if(increment>max){return(NULL)}
  for(i in seq(increment,max,by=increment)){
    plist <- c(plist,sum(delays<=i)/delayLen)
  }
  return(plist)
}

CleanText <- function(textVector){
  ##print(head(textVector))
  library(stringr)
  textVector <- str_replace_all(textVector,"  "," ")
  textVector <- str_replace_all(textVector,"http://t.co/[a-z,A-Z,0-9]* ","")
  textVector <- str_replace_all(textVector,"RT @[a-z,A-Z,0-9]* ","")
  textVector <- str_replace_all(textVector,"@[a-z,A-Z,0-9]* ","")
  textVector <- str_replace_all(textVector,"#","")
  return(textVector)
}

CreateWordCloud <- function(tweetVector,max=10){
  ##Takes vector of clean text... "tweetVector"
  ##Create corpus using library (tm)
  tweetCorpus <- Corpus((VectorSource(tweetVector)))
  tweetCorpus <- tm_map(tweetCorpus,tolower)
  tweetCorpus <- tm_map(tweetCorpus,removePunctuation)
  tweetCorpus <- tm_map(tweetCorpus,removeWords,stopwords('english'))
  ##Create term document matrix counting words
  tweetTDM <- TermDocumentMatrix(tweetCorpus)
  ##tweetTDM
  ##Sort matrix by count of words
  tdMatrix <- as.matrix(tweetTDM)
  sortedMatrix <- sort(rowSums(tdMatrix),decreasing=TRUE)
  cloudFrame <- data.frame(word=names(sortedMatrix),freq=sortedMatrix)
  cloudFrame <- cloudFrame[which(cloudFrame$freq>max),]
  wordcloud(cloudFrame$word,cloudFrame$freq)
}

MakeGeoURL <- function(address){
  root <- "http://maps.google.com/maps/api/geocode/"
  url <- paste(root,"json?address=",address,"&sensor=false",sep="")
  return(URLencode(url))
}

Addr2LatLong <- function(address){
  url <- MakeGeoURL(address)
  apiResult <- getURL(url)
  geoStruct <- fromJSON(apiResult)
  lat <- NA
  long <- NA
  try(lat <- geoStruct$results[[1]]$geometry$location$lat)
  try(long <- geoStruct$results[[1]]$geometry$location$lng)
  return(c(lat,long))
}