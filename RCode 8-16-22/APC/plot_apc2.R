plot.apc2 <- function(M)
{
  # Plot second set of age-period-cohort estimable functions.
  
  par(mfrow = c(4,3))
  
  
  DATA <- cbind(matrix(M$QuadLongAge[,1]), (M$QuadLongAge[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  pcurve(DATA, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Longitudinal Age Curve", cex.main = 1)

  DATA <- cbind(matrix(M$QuadCrossAge[,1]), (M$QuadCrossAge[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Age", "Rate", "CILo", "CIHi"))
  pcurve(DATA, lwd = 3, col = "darkred", colf = "pink", cex = 1.0, pch = 21)
  title(main = "Cross-Sectional Age Curve", cex.main = 1)
  
  pcurve(M$Long2CrossRR, lwd = 3, col = "darkred", colf = "pink", cex = 1.0, pch = 21)
  abline(1,0, lty = 3)
  title(main = "Long vs. Cross RR", cex.main = 1)
  
  pcurve(M$QuadFittedTemporalTrends, col = "steelblue4", colf = "slategray1", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Fitted Temporal Trends", cex.main = 1)
  
  pcurve(M$QuadPeriodRR, col = "steelblue4", colf = "slategray1", lwd = 3, cex = 1.0, pch = 21)
  abline(1, 0, lty = 3)
  title(main = "Period RR", cex.main = 1)
  
  pcurve(M$QuadCohortRR, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(1, 0, lty = 3)
  title(main = "Cohort RR", cex.main = 1)
  
  pcurve(M$QuadLocalDrifts, col = "black", colf = "grey88", lwd = 3, cex = 1.0, pch = 21)
  abline(M$NetDrift[1,1], 0, lty = 3)
  title(main = "Local Drifts", cex.main = 1)
  
  pcurve(M$QuadAgeDeviations, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Age Deviations", cex.main = 1)
  
  pcurve(M$QuadPerDeviations, col = "steelblue4", colf = "slategray1", lwd = 3 , cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Period Deviations", cex.main = 1)
  
  pcurve(M$QuadCohDeviations, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Cohort Deviations", cex.main = 1)
  
  pcurve(M$QuadFittedCohortPattern, col = "seagreen4", colf = "darkseagreen1", lwd = 3, cex = 1.0, pch = 21)
  abline(0, 0, lty = 3)
  title(main = "Fitted Cohort Pattern", cex.main = 1)
  
  DATA <- cbind(matrix(M$QuadGradientShifts[,1]), (M$QuadGradientShifts[,c(2,3,4)]))
  dimnames(DATA) <- list(c(), c("Period", "Mean % Change Per Year of Age", "CILo", "CIHi"))
  pcurve(DATA, col = "darkred", colf = "pink", lwd = 3, cex = 1.0, pch = 21)
  title(main = "Gradient Shifts", cex.main = 1)
  abline(100*(exp(M$Coefficients[4,1]) - 1), 0, lty = 3)
  
 
}