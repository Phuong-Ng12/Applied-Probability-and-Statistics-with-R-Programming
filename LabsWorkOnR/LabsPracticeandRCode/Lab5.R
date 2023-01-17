# roll m dice n times using sample()
RollSomeDice=function(n,m){
  numOf3s<-c(1:n)
  for (i in 1:n){
    numOf3s[i]<-sum(sample(c(1,2,3,4,5,6),m,repl=T)==3)
  }
  # table of relative frequency
  numOf3sTable<-table(numOf3s)/n
  titleNumOf3s = paste("Number of 3’s obtained in rolling",m, "dice")
  barplot(numOf3sTable, main=titleNumOf3s, xlab="Number of 3's", ylab="Relative frequency")
  return (numOf3sTable)
}

# roll m dice n times using rbinom()
RollSomeDiceRbinom=function(n,m){
  numOf3s<-rbinom(n,m,1/6)
  # table of relative frequency
  numOf3sTable<-table(numOf3s)/n
  titleNumOf3s = paste("Number of 3’s obtained in rolling",m, "dice with rbinom()")
  barplot(numOf3sTable, main=titleNumOf3s, xlab="Number of 3's", ylab="Relative frequency")
  return (numOf3sTable)
}

DiceMeans=function(n,m){
  meansList<-c(1:n)
  sdList<-c(1:n)
  for (i in 1:n){
    meansList[i]<-mean(sample(c(1,2,3,4,5,6),m,repl=T))
    sdList[i]<-sd(sample(c(1,2,3,4,5,6),m,repl=T))
  }
  meansTable<-table(meansList)
  titleMeans = paste("Means for",n,"rolls of",m, "dice")
  Means = mean(meansList)
  Sds = sd(meansList)
  RollPlot = barplot(meansTable, main=titleMeans, xlab="Means", ylab="Number of dice roll times")
  mylist = list("means" = Means, "sd" = Sds, RollPlot)
  return (mylist)
}

