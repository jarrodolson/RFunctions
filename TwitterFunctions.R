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
  EnsurePackage("twitteR")
  tweetList <- searchTwitter(searchTerm,n=maxTweets)
  tweetFrame <- do.call("rbind",lapply(tweetList,as.data.frame))
  return(tweetFrame[order(as.integer(tweetFrame$created)),])
}

ArrivalProbability <- function(times,increment,max){
  plist <- NULL
  timeLen <- length(times)
  if(increment>max){return(NULL)}
  for(i in seq(increment,max,by=increment)){
    val <- sum(as.integer(diff(times))<i)/timeLen
    plist <- c(plist,val)
  }
  return(plist)
}

DelayProbability <- function(delays,increment,max){
  plist <- NULL
  delays <- as.numeric(delays)
  delayLen <- length(delays)
  if(increment>max){return(NULL)}
  for(i in seq(increment,max,by=increment)){
    plist <- c(plist,sum(delays<=i)/delayLen)
  }
  return(plist)
}

CleanTwitterText <- function(textVector){
  ##print(head(textVector))
  EnsurePackage("stringr")
  textVector <- str_replace_all(textVector,"  "," ")
  textVector <- str_replace_all(textVector,"http://t.co/[a-z,A-Z,0-9]* ","")
  textVector <- str_replace_all(textVector,"RT @[a-z,A-Z,0-9]* ","")
  textVector <- str_replace_all(textVector,"@[a-z,A-Z,0-9]* ","")
  textVector <- str_replace_all(textVector,"#[a-z,A-Z,0-9]*","")
  textVector <- tolower(textVector)
  textVector <- str_replace_all(textVector,"[[:punct:]]","")
  textVector <- str_replace_all(textVector,"[[:cntrl:]]","")
  return(textVector)
}

SearchText <- function(textVector,term){
  returnForm <- NULL
  for(i in textVector){
    i <- tolower(i)
    term <- tolower(term)
    word.list <- str_split(i,"\\s+")
    words <- unlist(word.list)
    matches.total <- sum(!is.na(match(words,term)))
    match <- 0
    match[matches.total>0]<-1
    returnForm <- c(returnForm,match)
  }
  return(returnForm)
}

CreateWordCloud <- function(tweetVector){
  ##Takes vector of clean text... "tweetVector"
  ##Create corpus using library (tm)
  EnsurePackage("stringr")
  EnsurePackage("tm")
  EnsurePackage("wordcloud")
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
  cloudFrame <- cloudFrame[which(cloudFrame$freq>5),]
  wordcloud(cloudFrame$word,cloudFrame$freq)
}

MakeGeoURL <- function(address){
  EnsurePackage("RCurl")
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

JSONToDF <- function(json,totalPasses){
  EnsurePackage("stringr")
  if(length(json$results)>0){
    for(i in 1:length(json$results)){
      results <- json$results[[i]]
      created <- results$created_at
      created <- as.character(created)
      created <- as.POSIXct(created,tz="GMT",format="%a, %d %b %Y %H:%M:%S %z")
      from_user <- results$from_user
      from_user_id <- results$from_user_id
      from_user_id_str <- results$from_user_id_str
      from_user_name <- results$from_user_name
      lat <- results$geo$coordinates[1]
      long <- results$geo$coordinates[2]
      if(is.null(lat)|is.null(long)){
        lat<-NA
        long<-NA
      }
      id <- results$id
      id_str <- as.character(results$id_str)
      iso_language_code <- results$iso_language_code
      profile_image_url <- results$profile_image_url
      tweetSource <- results$source
      text <- str_replace_all(as.character(results$text),"\n","")
      if(totalPasses==1){tweetDF <- data.frame(created,text,from_user_id,lat,long,from_user_name,from_user,from_user_id_str,id,id_str,iso_language_code,profile_image_url,tweetSource)}
      if(totalPasses>1){tweetDF <- rbind(tweetDF,data.frame(created,text,from_user_id,lat,long,from_user_name,from_user,from_user_id_str,id,id_str,iso_language_code,profile_image_url,tweetSource))}
      totalPasses <- totalPasses+1
    }
  }
  if(length(json$results)==0){
    tweetDF <- as.data.frame(matrix(nrow=1,ncol=13))
    names(tweetDF)<-c("created","text","from_user_id","lat","long","from_user_name","from_user","from_user_id_str","id","id_str","iso_language_code","profile_image_url","tweetSource")
  }
  return(tweetDF)
}

GetNewTweets <- function(searchTerm,nResults,df,counter){
  ##Takes a search term and a maximum number of results and returns a dataframe
  ## Maximum requests are 150 per hour, so each page is a single request, that means
  ## you can request up to 15000 tweets per hour
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  df <- df[order(df$id),]
  df <- df[nrow(df),]
  lastTweetID <- df$id
  nPages <- ceiling(nResults/100)
  if(nPages>10){nPages=10}
  baseURL <- "http://search.twitter.com/search.json?"
  lastTweetIDQ <- paste("&since_id=",lastTweetID,sep="")
  totalPasses <- 1
  for(l in 1:nPages){
    searchURL <- URLencode(paste(baseURL,"q=",searchTerm,"&result_type=recent","&rpp=100","&page=",l,lastTweetIDQ,sep=""))
    data_in<-getURL(searchURL)
    fiName <- paste("ArchivedData\\",searchTerm,"_",l,"_",counter,"_",Sys.Date(),".txt",sep="")
    write(data_in,file=fiName)
    jsonData <- fromJSON(data_in)
    if(totalPasses==1){tweetDF<-JSONToDF(jsonData,1)}
    if(totalPasses>1){
      newTweetDF <- JSONToDF(jsonData,1)
      if(!is.na(newTweetDF$id)){
        tweetDF <- rbind(tweetDF,newTweetDF)
      }
      if(is.na(newTweetDF$id) | nrow(newTweetDF<100)){break}
    }
    totalPasses<- totalPasses+1
  }
  return(tweetDF)
}

GetTweets <- function(searchTerm,nResults){
  ##Takes a search term and a maximum number of results and returns a dataframe
  ## Maximum requests are 150 per hour, so each page is a single request, that means
  ## you can request up to 15000 tweets per hour
  nPages <- ceiling(nResults/100)
  EnsurePackage("RCurl")
  EnsurePackage("RJSONIO")
  baseURL <- "http://search.twitter.com/search.json?"
  totalPasses <- 1
  for(l in 1:nPages){
    searchURL <- URLencode(paste(baseURL,"q=",searchTerm,"&result_type=recent","&rpp=100","&page=",l,sep=""))
    data_in<-getURL(searchURL)
    fiName <- paste("ArchivedData\\",searchTerm,"_",l,"_",Sys.Date(),".txt",sep="")
    write(data_in,file=fiName)
    jsonData <- fromJSON(data_in)
    if(totalPasses==1){tweetDF<-JSONToDF(jsonData,1)}
    if(totalPasses>1){tweetDF <- rbind(tweetDF,JSONToDF(jsonData,1))}
    totalPasses<- totalPasses+1
  }
  return(tweetDF)
}

ReadInOldJSONFolder <- function(base){
  EnsurePackage("RJSONIO")
  EnsurePackage("RCurl")
  files <- list.files(base)
  countFi <- 1
  for(fi in files){
    fiName <- paste(base,"\\",fi,sep="")
    fiIn <- fromJSON(file=fiName)
    if(countFi==1){
      hold <- JSONToDF(fiIn,1)}
    if(countFi>1){hold <- rbind(hold,JSONToDF(fiIn,1))}
    countFi <- countFi+1
  } 
  return(hold)
}

SentimentAnalysis <- function(text){
  resultLi <- NULL
  pos.words <- readLines("positive_words.txt")
  neg.words <- readLines("negative_words.txt")
  cleanText <- CleanText(text)
  for(i in cleanText){
    i <- tolower(i)
    word.list <- str_split(i,"\\s+")
    words <- unlist(word.list)
    pos.matches.total <- sum(!is.na(match(words,pos.words)))
    neg.matches.total <- sum(!is.na(match(words,neg.words)))
    total <- pos.matches.total-neg.matches.total
    if(total>0){result=1}
    if(total==0){result=0}
    if(total<0){result=-1}
    resultLi <- c(resultLi,result)
  }
  return(resultLi)
}

PlotUS <- function(){
  EnsurePackage("maptools")
  EnsurePackage("PBSMapping")
  stateshape <- importShapefile("C:\\Users\\jarrodanderin\\Documents\\_RWork\\_Datasets\\tl_2010_us_state10\\tl_2010_us_state10",readDBF=TRUE)
  plotPolys(stateshape,xlim=c(-130,-60),ylim=c(20,50))
}

##searchTerm <- "#bachelor"
TrackTwitter <- function(searchTerm){
  EnsurePackage("RCurl")
  EnsurePackage("stringr")
  fileName <- paste(str_replace_all(searchTerm,"#",""),".txt",sep="")
  df <- GetTweets(searchTerm,1250)
  write.table(df,fileName,sep="\t")
  Sys.sleep(301)
  empty <- 1
  counter <- 2
  while(empty<2){
    df <- rbind(df,GetNewTweets(searchTerm,1250,df,counter))
    write.table(df,fileName,sep="\t")
    Sys.sleep(301)
    counter <- counter+1
  }
}

LoadTwitterDataSet <- function(fiName){
  df <- read.delim(fiName,stringsAsFactors=FALSE,row.names=NULL)
  df <- df[order(df$created),]
  df <- df[80:6611,]
  df$created <- as.POSIXct(df$created)
  df$lat <- as.numeric(df$lat)
  df$long <- as.numeric(df$long)
  df <- df[order(df$created),]
  df <- df[which(!is.na(df$created)),]
  return(df)
}

CleanTwitterDataSet <- function(fiName){
  EnsurePackage("stringr")
  temp <- readLines(fiName)
  tempHead <- temp[1]
  temp <- temp[2:length(temp)]
  writeLi <- tempHead
  rowCount <- 1
  flag <- FALSE
  skips <- 0
  for(line in temp){
    lineLi <- strsplit(line,"\t")
    ##Looks if the column length is too short
    if(length(lineLi[[1]])<14 & flag==FALSE){
      ## If not, slap together this line and next line
      lineTemp <- str_replace_all(line,'\n'," ")
      tempRowCount <- rowCount
      countSkips <- 1
      ##print(strsplit(temp[tempRowCount+1],"\t")[[1]])
      lengthNext <- length(strsplit(temp[tempRowCount+1],"\t")[[1]])
      ##print(lengthNext)
      while(lengthNext<14){
        lineTemp_2 <- str_replace_all(temp[tempRowCount+1],"\n"," ")
        lineNew <- paste(lineTemp,lineTemp_2)
        tempRowCount <- tempRowCount+1
        countSkips <- countSkips+1
        lengthNext <- length(strsplit(temp[tempRowCount+1],"\t")[[1]])
        ##print(lengthNext)
        skips <- 0
      }
      writeLi <- c(writeLi,lineNew)
      flag=TRUE
    }
    if(flag==TRUE){
      skips <- skips+1
    }
    if(flag==FALSE){
      writeLi <- c(writeLi,line)
    }
     if(skips>=countSkips){
       flag=FALSE
    }

    rowCount<-rowCount+1
  }
  outConn <- file(paste("CLEAN_",fiName,sep=""))
  writeLines(writeLi,con=outConn)
  close(outConn)
}

####These were poor functioning, but might have something useful
# LoadTwitterDataSet <- function(fiName){
#   df <- read.csv("~/_RWork/TwitterTest/Bachelor.txt",stringsAsFactors=FALSE,row.names=NULL)
#   temp <- readLines("~/_RWork/TwitterTest/Bachelor.txt")
#   tempHead <- temp[1]
#   temp <- temp[2:length(temp)]
#   rowCount <- 1
#   for(line in temp){
#     lineLi <- strsplit(line,"\t")
#     if(is.na(as.numeric(str_replace_all(lineLi[[1]][1],'"',"")))){
#       offendingBreak <- lineLi[[1]][1]
#       prevLi <- strsplit(temp[rowCount-1],"\t")
#       prevText <- prevLi[[1]][3]
#       if(!is.na(prevText)){
#         newReplace <- paste(prevText,offendingBreak)
#       }
#       if(is.na(prevText)){
#         newReplace <- offendingBreak
#       }
#       prevLi[[1]][3]<-newReplace
#       prevLi <- paste(prevLi,lineLi[[1]][2:length(lineLi[[1]])])
#       print(prevLi)
#       ##print(paste(lineLi,collapse="\t"))
#     }
#     rowCount <- rowCount+1
#   }
# }
# 
# LoadTwitterDataSet <- function(fiName){
#   df <- read.csv("~/_RWork/TwitterTest/Bachelor.txt",stringsAsFactors=FALSE,row.names=NULL)
#   temp <- readLines("~/_RWork/TwitterTest/Bachelor.txt")
#   tempHead <- temp[1]
#   temp <- temp[2:length(temp)]
#   rowCount <- 1
#   for(line in temp){
#     lineLi <- strsplit(line,"\t")
#     ##print(length(lineLi[[1]]))
#     if(length(lineLi[[1]])<4){
#       ##print(lineLi)
#       forwardLineLi <- strsplit(temp[rowCount+1],"\t")
#       forwardLineLiWords <- forwardLineLi[[1]][1]
#       forwardLineLiBody <- forwardLineLi[[1]][2:length(forwardLineLi[[1]])]
#       lineLi <- c(lineLi[[1]],forwardLineLiBody)
#       fixLine <- forwardLineLiWords
#       print(length(forwardLineLi[[1]]))
#       print(forwardLineLi)
#       rowCount <- rowCount+1
#     }
#     rowCount <- rowCount+1
#   }
#     if(is.na(as.numeric(str_replace_all(lineLi[[1]][1],'"',"")))){
#       offendingBreak <- lineLi[[1]][1]
#       prevLi <- strsplit(temp[rowCount-1],"\t")
#       prevText <- prevLi[[1]][3]
#       if(!is.na(prevText)){
#         newReplace <- paste(prevText,offendingBreak)
#       }
#       if(is.na(prevText)){
#         newReplace <- offendingBreak
#       }
#       prevLi[[1]][3]<-newReplace
#       print(prevLi)
#       ##print(paste(lineLi,collapse="\t"))
#     }
#     rowCount <- rowCount+1
#   }
# }