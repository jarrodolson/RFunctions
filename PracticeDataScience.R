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
  library(RCurl) 
  
  # Set SSL certs globally
  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
  #pin = 0813451
  reqURL <- "https://api.twitter.com/oauth/request_token"
  accessURL <- "https://api.twitter.com/oauth/access_token"
  authURL <- "https://api.twitter.com/oauth/authorize"
  consumerKey <- "5KurGrWitBe7cEDAe2lQ"
  consumerSecret <- "BNrkYk8iRHD0PjHH0lkyGRhVWWW4aP03sS739yKbA"
  
  twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=reqURL,accessURL=accessURL,authURL=authURL)
  
  twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  
  registerTwitterOAuth(twitCred)
  ##This doesn't work anymore, need to figure out how to pass search
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
  textVector <- str_replace_all(textVector,"  "," ")
  textVector <- str_replace_all(textVector,"http://t.co/[a-z,A-Z,0-9]* ","")
  textVector <- str_replace_all(textVector,"RT @[a-z,A-Z,0-9]* ","")
  textVector <- str_replace_all(textVector,"@[a-z,A-Z,0-9]* ","")
  textVector <- str_replace_all(textVector,"#","")
  return(textVector)
}

CreateWordCloud <- function(tweetVector){
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
  cloudFrame <- cloudFrame[which(cloudFrame$freq>10),]
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

PrepareTwitter()

strataTweetDF <- read.table("saveTweets.txt",sep="\t",header=TRUE,stringsAsFactors=FALSE,comment.char="",blank.lines.skip=FALSE,quote="")
colnames(strataTweetDF) <- c("text","favorited","favoriteCount","replytoSN","created","truncated","replyToUID","statusSource","screenName","retweetCount","isRetweet","retweeted","longitude","lattitude")
storage <- ""
trigger <- FALSE
save <- data.frame(matrix(nrow=0,ncol=ncol(strataTweetDF)))
colnames(save) <- colnames(strataTweetDF)
for(row in 1:nrow(strataTweetDF)){
  data <- strataTweetDF[row,]
  if(trigger==TRUE){
    data <- cbind(storage,strataTweetDF[row,2:ncol(strataTweetDF)])
    colnames(data)<-colnames(strataTweetDF)
    print(length(data))
    trigger <- FALSE
  }
  if(strataTweetDF[row,2]==''){
    storage <- strataTweetDF[row,1]
    trigger <- TRUE
  }
  if(trigger==FALSE){
    save <- rbind(save,data)
  }
}

save$datetime <- as.POSIXct(strftime(save$created,format="%Y-%m-%d %H:%M:%S"))

##strataTweetDF <- TweetFrame("#strataconf",1000)
attach(save)
hist(save$datetime,breaks=15,freq=TRUE)
detach(save)
tweetDF <- tweetDF[order(as.integer(tweetDF$created)),]
attach(tweetDF)
saved <- save
library(dplyr)
saved$datetime_int <- as.integer(saved$datetime)
saved2 <- saved %.%
  arrange(datetime_int) %.%
  filter(!is.na(datetime_int))
##diff gives us the difference between adjacent cells, working from the last value backward
hist(as.integer(diff(saved$datetime_int)))

maxDif <- max(as.integer(saved$datetime_int))-min(as.integer(saved$datetime_int))
maxDif
plist <- ArrivalProbability(saved$datetime,10,100)

##Look at most popular tweets
saved$favoriteCount <- as.integer(saved$favoriteCount)
temp <- saved %.%
  arrange(-favoriteCount)
View(temp)

##Compare sample probability distribution for delay
plot(DelayProbability(rpois(100,10),1,20),col=2)
points(DelayProbability(rpois(100,3),1,20),col=3)

##Compare probability distribution samples
plot(DelayProbability(rpois(100,10),1,20))
for(i in 1:15){points(DelayProbability(rpois(100,10),1,20))}

##Alternatively, get the probability of observing delay of 3 or less with poisson distro mean (lambda) of 10
ppois(3,lambda=10)

##Compare arrival rates
CompareTags <- function(searchTerm1,searchTerm2,max){
  tweetDF.1 <- TweetFrame(searchTerm1,max)
  eventDelays.1 <- as.integer(diff(tweetDF.1$created))
  tweetDF.2 <- TweetFrame(searchTerm2,max)
  eventDelays.2 <- as.integer(diff(tweetDF.2$created))
  ##Present results of significance test
  return(poisson.test(c(sum(eventDelays.video<=mean(eventDelays.video)),sum(eventDelays.reads<=mean(eventDelays.video))),c(500,500)))
}

##test function
CompareTags("#sequestration","#SadDrSeuss",500)

tweetDF.video <- TweetFrame("#musicvideo",500)
eventDelays.video <- as.integer(diff(tweetDF.video$created))
mean(eventDelays.video)
sum(eventDelays.video<=mean(eventDelays.video))
poisson.test(sum(eventDelays.video<=mean(eventDelays.video)),length(eventDelays.video))$conf.int
tweetDF.reads <- TweetFrame("#goodreads",500)
eventDelays.reads <- as.integer(diff(tweetDF.reads$created))
mean(eventDelays.reads)
sum(eventDelays.reads<=mean(eventDelays.reads))
poisson.test(sum(eventDelays.reads<=mean(eventDelays.video)),length(eventDelays.reads))$conf.int
poisson.test(c(sum(eventDelays.video<=mean(eventDelays.video)),sum(eventDelays.reads<=mean(eventDelays.video))),c(500,500))

##Twitter Text Analysis
## Runs vector of data through function
CreateWordCloud(tweetDF$text)

##Mapping
usShape <- importShapefile("gz_2010_us_040_00_500k",readDBF=TRUE)
summary(usShape)
plotPolys(usShape,xlim=c(-130,-60),ylim=c(20,50))
EID <- 1
X <- myAddy[2]
Y <- myAddy[1]
pointData <- data.frame(EID,X,Y)
eventData <- as.EventData(pointData,projection=NA)
addPoints(eventData,col="red",cex=.5)

myAddy <- Addr2LatLong("3007 NW Morning Glory Dr., Corvallis, OR")