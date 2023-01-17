confidence=function(data, conflevel, meanOfWhat, unit){
  if(missing(conflevel)){
    conflevel = 0.95
  }
  lowerbound = t.test(data, conf.level = conflevel)$conf.int[1]
  upperbound = t.test(data, conf.level = conflevel)$conf.int[2]
  CIsentence = cat("We are", conflevel*100, "percent sure that the mean", meanOfWhat, "is between", 
                   lowerbound, "and", upperbound, unit, "\n")
  return(CIsentence)
}
