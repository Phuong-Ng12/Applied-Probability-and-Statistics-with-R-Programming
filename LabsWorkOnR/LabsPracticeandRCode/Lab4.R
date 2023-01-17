FlipOnce = function() {
  HeadOrTail<-sample(c("Heads","Tails"),1)
  return(HeadOrTail)
}

CoinResults=function(n){
  coinList<-sample(c("Heads","Tails"),n,repl=T)
  return(coinList)
}

ProbHeads=function(n){
  coinList<-CoinResults(n)
  numHeads<-sum(coinList=="Heads")
  return(numHeads/n)
}

MaxAndMinHeads=function(m,n){
  probHeadsList<-c(1:n);
  for(i in 1:n){
    probHeadsList[i]=ProbHeads(m)
  }
  max = max(probHeadsList)
  min = min(probHeadsList)
  return (c(max,min))
}

RollDie=function(n){
  dieRollList<-sample(c(1,2,3,4,5,6),n,repl=T)
  dieRollTable<-table(dieRollList)
  titleBarPlot = paste("Distribution of outcomes of",n,"die rolls")
  return (barplot(dieRollTable, main=titleBarPlot))
}

RollSomeDice=function(n,m){
  numOf3s<-c(1:n)
  for (i in 1:n){
    numOf3s[i]<-sum(sample(c(1,2,3,4,5,6),m,repl=T)==3)
  }
  numOf3sTable<-table(numOf3s)
  titleNumOf3s = paste("Number of 3’s obtained in rolling",m, "dice")
  return (barplot(numOf3sTable, main=titleNumOf3s))
}

DrawCardsWithReplacement=function(n,m){
  numOfRed<-c(1:n)
  for (i in 1:n) {
    numOfRed[i]=sum(sample(c("Red","Black"),m,repl=T)=="Red")
  }
  numOfRedTable<-table(numOfRed)
  titleNumOfRed = paste("Number of red cards in",m,"draws with replacement")
  return (barplot(numOfRedTable, main=titleNumOfRed, xlab="Number of red cards", ylab="Frequency"))
}

DrawCardsWithoutReplacement=function(n,m){
  deckOfCards<-c(1:52)
  for (i in 1:26){deckOfCards[i]="Red"}
  for (i in 27:52){deckOfCards[i]="Black"}
  numOfRed<-c(1:n)
  for (i in 1:n) {
    numOfRed[i]=sum(sample(deckOfCards,m,repl=F)=="Red")
  }
  numOfRedTable<-table(numOfRed)
  titleNumOfRed = paste("Number of red cards in",m,"draws without replacement")
  return (barplot(numOfRedTable, main=titleNumOfRed, xlab="Number of red cards", ylab="Frequency"))
}