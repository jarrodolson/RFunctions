checkMissing <- function(variableLi,dfName,df){
  df$missing <- 0
  for(i in variableLi){
    var <- i
    ref <- paste(dfName,var,sep="$")
    df$missing[is.na(eval(parse(text=ref)))]<-1
  }
  print("Missing variables table")
  print(table(df$missing))
  return(df)
}

dropNonVariation <- function(dframe,idvar,dv){
  conflictC <- count(dat_3,c(idvar,dv))
  conflictC <- reshape(conflictC, idvar=idvar,timevar=dv,direction="wide")
  names(conflictC) <- c("ccode","intraCumIntFalse","intraCumIntTrue")
  temp <- merge(dframe,conflictC,by=idvar,all.x=TRUE,stringsAsFactors=FALSE)
  dat_big <- temp[which(is.na(temp$intraCumIntFalse)==FALSE & is.na(temp$intraCumIntTrue)==FALSE),]
  dat_big$ccode <- as.character(dat_big$ccode)
  return(dat_big)
}

lagIt <- function(name,n,panel,index,dataName,t.game){
  library(plm)
  test1 <- paste(dataName,"[order(",dataName,"$",panel,",",dataName,"$",index,"),]",sep="")
  temp <- eval(parse(text=test1))
  test <- paste("pdata.frame(temp,","index=c('",panel,"','",index,"'))",sep="")
  temp <- eval(parse(text=test))
  test2 <- paste("lag(temp","$",name,",k=",n,")",sep="")
  temp$temp <- eval(parse(text=test2))
  temp <- as.data.frame(temp)
  return(temp$temp)
}

countEm <- function(df){
  df$count <- 1:nrow(df)
  return(df)
}

getLogical <- function(str){
  return(grepl(str,d2012$description,ignore.case=TRUE))
}