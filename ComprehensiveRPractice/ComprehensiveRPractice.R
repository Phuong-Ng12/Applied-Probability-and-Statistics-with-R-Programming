# question 4
# n : number of drawing cards
drawCards=function(n) {
  rankOfCards<-c("Ace", 2, 3, 4, 5, 6, 7, 8, 9, 10, "J", "Q", "K")
  resultOfDrawCards<-sample(rankOfCards, n, repl=T)
  tableResultOfDrawCards<-table(resultOfDrawCards)
  barplot(tableResultOfDrawCards, main ="Frequency of each rank of cards by drawing 10000 cards with replacement")
  return(tableResultOfDrawCards)
}

# question 4b
# n : number of drawing cards
# m : number of times drawing cards
drawCards4b=function(n, m) {
  deckOfCards<-c(1:52)
  for (i in 1:4){deckOfCards[i]="J"}
  for (i in 5:8){deckOfCards[i]="Q"}
  for (i in 9:12){deckOfCards[i]="K"}
  
  resultOfDrawCards<-(1:m)
  for (i in 1:m) {
    result<-sample(deckOfCards, n, repl=F)
    print(result)
    counter = 0
    for (j in 1:m) {
      if (result[j] == "J" || result[j] == "Q" || result[j] == "K") {
        counter = counter + 1
      }
    }
    print(counter)
    resultOfDrawCards[i]<-counter
  }
  tableFaceCards=table(resultOfDrawCards)
  barplot(tableFaceCards)
  return(tableFaceCards)
}

# n : number of batteries
# m : time
probBatteryLongerThan20=function(n,m){
  lifetime=runif(n, 0, m)
  counter = 0
  for(i in 1:n){
    if(lifetime[i] > 20) {
      counter = counter + 1
    }
  }
  probability=counter/n
  
  return(probability)
}

# n : number of batteries
# m : beta = mean
# o : number of devices
prob4BatteriesLongerThan20=function(n,m,o){
  result<-c(1:o)
  for(i in 1:o) {
    result[i]<-sum(rexp(n,1/m)>20)==4
  }
  total=sum(result)
  probability=total/o
  return(probability)
}