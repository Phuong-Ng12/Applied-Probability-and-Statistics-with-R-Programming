NumOfAcesProbs=function(n,m){
  numOfProbs<-c(0:n)
  for (i in 0:n){
    numOfProbs[i + 1]=dhyper(i,4,48,m)
  }
  numOfAces=c(0:n)
  tableAcesProbs=data.frame(numOfProbs,numOfAces)
  titleProbsOfAces = paste("Probability distribution for Number of Aces obtained 
                           when",n, "cards are drawn")
  barplot(numOfProbs,main=titleProbsOfAces,xlab="Number of Aces",names=numOfAces,ylab="Probability")
  return(tableAcesProbs)
}

# draw m cards n times using sample()
DrawAces=function(n,m){
  deckOfCards<-c(1:52)
  for (i in 1:4){deckOfCards[i]="Ace"}
  numOfAces<-c(1:n)
  for (i in 1:n){
    numOfAces[i]<-sum(sample(deckOfCards,m,repl=F)=="Ace")
  }
  # table of relative frequency
  numOfAcesTable<-table(numOfAces)/n
  titleNumOfAces = paste("Number of Aces obtained in drawing",m, "cards",n,"times")
  barplot(numOfAcesTable, main=titleNumOfAces, xlab="Number of Aces", ylab="Relative frequency")
  return (numOfAcesTable)
}

# draw m cards n times using rhyper()
DrawAcesWithRhyper=function(n,m){
  numOfAces<-c(1:n)
  for (i in 1:n){
    numOfAces[i]<-rhyper(n,4,48,m)
  }
  # table of relative frequency
  numOfAcesTable<-table(numOfAces)/n
  titleNumOfAces = paste("Number of Aces obtained in drawing",m, "cards",n,"times with rhyper()")
  barplot(numOfAcesTable, main=titleNumOfAces, xlab="Number of Aces", ylab="Relative frequency")
  return (numOfAcesTable)
}

# probability distribution of failure for the number of tickets 
# the student will buy before getting a winner
NumTicketsProbs=function(n,m){
  numOfAllTicketsProbs<-c(1:n)
  counter = 0;
  for (i in 1:n){
    numOfAllTicketsProbs[i]<-dgeom(i,m)
    if(numOfAllTicketsProbs[i]>0.0001){
      counter = counter + 1
    }
  }
  numOfTicketsProbs<-c(1:counter)
  j = 0
  for(i in 1:n){
    if(numOfAllTicketsProbs[i]>0.0001){
      j = j + 1
      numOfTicketsProbs[j]=numOfAllTicketsProbs[i]
    }
  }
  numOfTickets=c(1:counter)
  tableWinProbs=data.frame(numOfTicketsProbs,numOfTickets)
  titleProbsOfTickets = paste("Probability distribution of failure for number of tickets students 
                              will buy before getting a winner")
  barplot(numOfTicketsProbs,main=titleProbsOfTickets,xlab="Number of tickets",names=numOfTickets,ylab="Probability")
  return(tableWinProbs)
}

# With sample(): probability distribution for the number of lottery tickets the student 
# must buy before obtaining a winner 
# n : number of students
# m : number of times buying lottery tickets 
WinTicketProbWithSample=function(n,m){
  
  numOfTicketsToWin<-c(1:n)
  for (i in 1:n){
    buyTicketsRec<-sample(c("W","L"),m,repl=T,prob = c(0.2,0.8))
    counter = 0
    for (j in 1:m){
      if(buyTicketsRec[j]=="W"){
        counter = counter + 1
        break
      } else{
        counter = counter + 1
      }
    }
    numOfTicketsToWin[i]=counter
  }
  
  WinProbsTable=table(numOfTicketsToWin)/n
  
  titleProbsOfTickets = paste("Probability distribution of failure for number of tickets",n,"students 
                              will buy before getting a winner with sample()")
  barplot(WinProbsTable,main=titleProbsOfTickets,xlab="Number of tickets",ylab="Relative Frequency")
  return(WinProbsTable)
  
}

# With rgeom(): probability distribution for the number of lottery tickets the student 
# must buy before obtaining a winner
# n : number of people buy lottery tickets
# m : probability of winning
WinTicketProbWithRgeom=function(n,m){
  numOfTickets = rgeom(n,m)
  numOfTickets[numOfTickets==0]=NA
  winTable = table(numOfTickets)/n
  
  titleWinProbs = paste("Probability distribution of failure for number of tickets",n,"people 
                              will buy before getting a winner")
  barplot(winTable,main=titleWinProbs,xlab="Number of tickets",ylab="Relative frequency")
  return(winTable)
}

# probability distribution for the number of flaws in one metre of cable 
# n : maximum number of flaws
# m : lambda
NumOfFlaws=function(n,m){
  flawAllProbs<-c(0:n)
  counter = 0
  for (i in 0:n){
    flawAllProbs[i+1]<-dpois(i,m)
    if(flawAllProbs[i+1]>0.0001){
      counter = counter + 1
    }
  }
  counter = counter - 1
  flawProbs<-c(0:counter)
  j = 0
  for (i in 0:n){
    if(flawAllProbs[i+1]>0.0001){
      j = j + 1
      flawProbs[j] = flawAllProbs[i+1]
    }
  }
  numFlaws=c(0:counter)
  tableFlawProbs=data.frame(flawProbs,numFlaws)
  titleFlawProbs = paste("Probability distribution for the number of flaws in one meter of cable")
  barplot(flawProbs,main=titleFlawProbs,xlab="Number of flaws",names=numFlaws,ylab="Probability")
  return(tableFlawProbs)
}


# With rpois(): probability distribution for the number of flaws in one metre of cable 
# n : number of cables
# m : lambda
NumOfFlawsRpois=function(n,m){
  numFlaws<-table((rpois(n,m)))/n
  titleFlawProbs = paste("Probability distribution for the number of flaws 
                         in one meter of",n,"cable")
  barplot(numFlaws,main=titleFlawProbs,xlab="Number of flaws",ylab="Relative frequency")
  return(numFlaws)
}