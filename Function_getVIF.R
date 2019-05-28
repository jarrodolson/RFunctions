getVIF <- function(varLi,dataName){
  library(reshape)
  tempDF <- data.frame(matrix(NA,ncol=3,nrow=length(varLi)))
  tempDF <- rename(tempDF,c(X1="Variable",X2="R2",X3="VIF"))
  count<-1
  while(count<=length(varLi)){
    term <- varLi[count]
    alreadyEqualed <- FALSE
    for(i in 1:length(varLi)){
      if(alreadyEqualed==TRUE & i!=count){
        term<-paste(term,"+",varLi[i])
      }
      if(alreadyEqualed==FALSE & i!=count){
        term <- paste(term,"~",varLi[i])
        alreadyEqualed<-TRUE
      }
    }
    #print(paste("The variable being tested is ",varLi[count]))
    tempDF[count,1]<-varLi[count]
    temp <- lm(term,data=dataName)
    ##print(summary(temp))
    r2 <- summary(temp)$r.squared
    tempDF[count,2]<-r2
    tempDF[count,3]<-1/(1-r2)
    count <- count+1
  }
  return(tempDF)
}