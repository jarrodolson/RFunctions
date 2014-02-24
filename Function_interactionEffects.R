#PRGen Like Command
##Function accepts 6 values, fist is dataset name, second if the continuous variable
####to be simulated against, third is the dummy variable to be
####held constant, fourth is the value for the dummy variable being
####held constant, 5th is the minimum for the continuous variable
####6th is the maximum for the continuous variable; Returns the dataframe
prepPred <- function(data,holdValue,holdValue_2,value_2,min,max){
  temp2 <- data.frame(holdValue=(min:max))
  temp2[[holdValue]]=temp2$holdValue
  temp2[[holdValue_2]]<-value_2
  
  ##Creates new frame (temp2), with all the columnames filled in as 1
  for (x in colnames(data)){
    if (x!=holdValue & x!=holdValue_2){
    temp2[[x]]<-mean(data[[x]],na.rm=TRUE)}}
  ##colnames(temp2)
  print("Need to manually go in and fix dummy variables to desired level")
  return(temp2)
  }
#Graphs confidence intervals at 90% CI, requires prediction dataframe
##and min and max for lines
##Thank you to http://twri.tamu.edu/bret/BretWebSiteDocs/GLMCI.pdf
graph90CI <- function(preddat,min,max,color){
  upperlogit = preddat$fit+1.96*preddat$se.fit
  lowerlogit = preddat$fit-1.96*preddat$se.fit
  ucl = plogis(upperlogit)
  lcl = plogis(lowerlogit)
  lines(min:max,ucl,lty=2,col=color)##90% confidence
  lines(min:max,lcl,lty=2,col=color)##90% confidence
  }
#Graphs predicted fit line, takes data frame, min, max, and color
graphFit <- function(preddat,min,max,color){
  lines(min:max,exp(preddat$fit)/(1+exp(preddat$fit)),col=color)
  }