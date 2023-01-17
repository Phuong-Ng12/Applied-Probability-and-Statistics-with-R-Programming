correlationtest=function(x, y, sigLevel, a, b) {
  if (missing(sigLevel)) {
    sigLevel = 0.05
  }
  pvalue = cor.test(x, y, conf.level = 1-sigLevel)$p.value
  
  if (pvalue > sigLevel) {
    isEvidence = "have no"
  } else {
    isEvidence = "have"
  }
  CIsentence = cat("At alpha =", sigLevel, "we", isEvidence, "evidence of a linear correlation between", a, "and", b, ".\n")
  return (CIsentence)
}