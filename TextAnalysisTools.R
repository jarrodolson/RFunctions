#############################################
## Text Analysis Tools
#############################################

EnsurePackage <- function(x){
  x <- as.character(x)
  if(!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

CleanText <- function(textVector){
  ##print(head(textVector))
  EnsurePackage("stringr")
  textVector <- str_replace_all(textVector,"  "," ")
  textVector <- tolower(textVector)
  textVector <- str_replace_all(textVector,"[[:punct:]]","")
  textVector <- str_replace_all(textVector,"[[:cntrl:]]"," ")
  return(textVector)
}

SearchText <- function(textVector,term){
  EnsurePackage("stringr")
  returnForm <- NULL
  for(i in textVector){
    i <- CleanText(i)
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
  ##cloudFrame <- cloudFrame[which(cloudFrame$freq>5),]
  wordcloud(cloudFrame$word,cloudFrame$freq)
  return(cloudFrame)
}

WarAnalysis <- function(text){
  EnsurePackage("stringr")
  resultLi <- NULL
  war.words <- readLines("war_words.txt")
  war.words <- unlist(war.words)
  cleanText <- CleanText(text)
  for(i in cleanText){
    i <- tolower(i)
    word.list <- str_split(i,"\\s+")
    words <- unlist(word.list)
    matches.total <- sum(!is.na(match(words,war.words)))
    if(matches.total==0){result=0}
    if(matches.total>0){result=1}
    resultLi <- c(resultLi,result)
  }
  return(resultLi)
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
