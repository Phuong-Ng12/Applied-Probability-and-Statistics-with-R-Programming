hypothesis=function(data, level, meanInfo, units, meanValue, inequality){
  pvalue = t.test(data, mu = meanValue, alternative = inequality, conf.level = 1-level)$p.value
  if (pvalue < level) {
    isAccept = "do"
  } else {
    isAccept = "do not"
  }
  CIsentence = cat("At alpha=",level, "we", isAccept ,"have sufficient evidence that the mean", meanInfo, "is", inequality, "than", meanValue, units, "\n")
  return(CIsentence)
}