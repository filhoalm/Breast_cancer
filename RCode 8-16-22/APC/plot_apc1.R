plot.apc1 <- function(M)
{
  # Plot first set of age-period-cohort estimable functions.
  
  par(mfrow = c(4,3))
  
  
  DATA <- cbind(matrix(M$LongAge[,1]), (M$LongAge[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  pcurve(DATA, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Longitudinal Age Curve", cex.main = 1)

  DATA <- cbind(matrix(M$CrossAge[,1]), (M$CrossAge[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  pcurve(DATA, lwd = 3, col = "darkred", colf = "pink", cex = 1.0, pch = 21)
  title(main = "Cross-Sectional Age Curve", cex.main = 1)
  
  pcurve(M$Long2CrossRR, lwd = 3, col = "darkred", colf = "pink", cex = 1.0, pch = 21)
  abline(1,0, lty = 3)
  title(main = "Long vs. Cross RR", cex.main = 1)
  
  pcurve(M$FittedTemporalTrends, col = "steelblue4", colf = "slategray1", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Fitted Temporal Trends", cex.main = 1)
  
  pcurve(M$PeriodRR, col = "steelblue4", colf = "slategray1", lwd = 3, cex = 1.0, pch = 21)
  abline(1, 0, lty = 3)
  title(main = "Period RR", cex.main = 1)
  
  pcurve(M$CohortRR, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(1, 0, lty = 3)
  title(main = "Cohort RR", cex.main = 1)
  
  pcurve(M$LocalDrifts, col = "black", colf = "grey88", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Local Drifts", cex.main = 1)
  
  pcurve(M$AgeDeviations, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Age Deviations", cex.main = 1)
  
  pcurve(M$PerDeviations, col = "steelblue4", colf = "slategray1", lwd = 3 , cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Period Deviations", cex.main = 1)
  
  pcurve(M$CohDeviations, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Cohort Deviations", cex.main = 1)
  
  pcurve(M$FittedCohortPattern, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Fitted Cohort Pattern", cex.main = 1)
  
}