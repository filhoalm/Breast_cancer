pcurve_log <- function(DATA, col = 'steelblue4', colf = 'slategray1', bg = 'grey99', 
                   pch = 1, type = 'b', lty = 1, lwd = 2, 
                   XLim = NA, YLim = NA, cex = 1.5)
{
  # Plot a selected output from an age-period-cohort model. 
  
  x <- DATA[,1]
  rangex <- range(x)[2]-range(x)[1]
  if (is.na(XLim[1])) {XLim <- c(min(x)-0.05*rangex, max(x)+0.05*rangex)}
  xl <- dimnames(DATA)[[2]][1] 
  
  y <- log(DATA[,2])
  rangey <- range(y)[2]-range(y)[1]
  if (is.na(YLim[1])) {YLim <- c(min(y)-0.05*rangey, max(y)+0.05*rangey)}
  yl <- dimnames(DATA)[[2]][2] 
  
  
  if (ncol(DATA)==4){
    xci <- c(x, rev(x))
    yci <- c(log(DATA[, 3]), rev(log(DATA[, 4])))
    rangey <- range(yci)[2]-range(yci)[1]
    YLim <- c(min(yci) - 0.05*rangey, max(yci) + 0.05*rangey)}
  else {yci <- NULL}
  
  plot(x, y, col = col, pch = pch, type = type, lty = lty, lwd = lwd, cex = cex, 
       xlab = xl, ylab = yl, xlim = XLim, ylim = YLim, las = 1, bg = bg)  
  
  if (!is.null(yci[1])){
    polygon(xci, yci, col = colf, border = colf)
  }
  
  abline(h = 1, lty = 2, lwd = 1)
  abline(v = 50, lty = 2, lwd = 1)
  
  points(x, y, col = col, pch = pch, type = type, lty = lty, lwd = lwd, cex = cex, 
         xlab = xl, ylab = yl, xlim = XLim, ylim = YLim, las = 1, bg = bg)
  
}
