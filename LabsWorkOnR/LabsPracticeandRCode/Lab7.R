# n : number of waiting people
# m : number of waiting minutes 
WaitTimesFreq=function(n,m){
  titleFreq=paste("Frequency of waiting times for",n,"people
       who are waiting for a bus coming every",m,"minutes")
  hist(runif(n,0,m),main=titleFreq,ylab="Number of people",xlab="Waiting time (minutes)")
}

# n : number of waiting people
# m : number of waiting minutes 
WaitTimesLessThan10MinsProbs=function(n,m){
  peopleWaitLessThan10mins = 0
  for(i in 1:n){
    WaitTime<-runif(n,0,m)
    if(WaitTime[i]<10){
      peopleWaitLessThan10mins <- peopleWaitLessThan10mins + 1
    }
  }
  probability<-peopleWaitLessThan10mins/n
  return(probability)
}

# n : number of waiting people
# m : number of waiting minutes 
WaitTimesExpo=function(n,m){
  titleFreq=paste("Frequency of exponentially-distributed waiting times for",n,"people
       with mean",m,"minutes")
  hist(rexp(n,1/m),main=titleFreq,ylab="Number of people",xlab="Waiting time (minutes)")
}

# n : number of waiting people
# m : number of waiting minutes 
WaitTimesLessThan10MinsExpoProbs=function(n,m){
  peopleWaitLessThan10mins = 0
  for(i in 1:n){
    WaitTime<-rexp(n,1/m)
    if(WaitTime[i]<10){
      peopleWaitLessThan10mins <- peopleWaitLessThan10mins + 1
    }
  }
  probability<-peopleWaitLessThan10mins/n
  return(probability)
}

# n : number of batteries
# m : mean
# sd : standard deviation
ShippedBatteryProbs=function(n,m,sd){
  allBattery<-rnorm(n,m,sd)
  numShippedBatteries = 0
  for (i in 1:n){
    if(allBattery[i]>=8.9&&allBattery[i]<=9.1){
      numShippedBatteries = numShippedBatteries + 1
    }
  }
  probability=numShippedBatteries/n
  return (probability)
}