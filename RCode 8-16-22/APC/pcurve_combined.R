pcurve_combined <- function(DATA1, DATA2, col = c("#e41a1c", '#003F87'), colf = 'slategray1', bg = 'grey99', 
                   pch = 1, type = 'b', lty = 1, lwd = 2, 
                   XLim1 = NA, YLim1 = NA, XLim2 = NA, YLim2 = NA, cex = 1.5)
  #"#fdbf31", "#000811" yeallow balck
{
  # Plot a selected output from an age-period-cohort model. 
  
  x1 <- DATA1[,1]
  rangex1 <- range(x1)[2]-range(x1)[1]
  if (is.na(XLim1[1])) {XLim1 <- c(min(x1)-0.05*rangex1, max(x1)+0.05*rangex1)}
  xl1 <- dimnames(DATA1)[[2]][1]
  
  x2 <- DATA2[,1]
  rangex2 <- range(x2)[2]-range(x2)[1]
  if (is.na(XLim2[1])) {XLim2 <- c(min(x2)-0.05*rangex2, max(x2)+0.05*rangex2)}
  xl2 <- dimnames(DATA2)[[2]][1]
  
  XLim <- c(min(XLim1[1], XLim2[1]), max(XLim1[2], XLim2[2]))
  
  
  y1 <- 100*(exp(DATA1[,2])-1)
  rangey1 <- range(y1)[2]-range(y1)[1]
  if (is.na(YLim1[1])) {YLim1 <- c(min(y1)-0.05*rangey1, max(y1)+0.05*rangey1)}
  yl1 <- dimnames(DATA1)[[2]][2]
  
  y2 <- 100*(exp(DATA2[,2])-1)
  rangey2 <- range(y2)[2]-range(y2)[1]
  if (is.na(YLim2[1])) {YLim2 <- c(min(y2)-0.05*rangey2, max(y2)+0.05*rangey2)}
  yl2 <- dimnames(DATA2)[[2]][2] 
  
  
  if (ncol(DATA1)==4){
    xci1 <- c(x1, rev(x1))
    yci1 <- c(100*(exp(DATA1[, 3])-1), rev(100*(exp(DATA1[, 4])-1)))
    rangey1 <- range(yci1)[2]-range(yci1)[1]
    YLim1 <- c(min(yci1) - 0.05*rangey1, max(yci1) + 0.05*rangey1)}
  else {yci1 <- NULL}
  
  if (ncol(DATA2)==4){
    xci2 <- c(x2, rev(x2))
    yci2 <- c(100*(exp(DATA2[, 3])-1), rev(100*(exp(DATA2[, 4])-1)))
    rangey2 <- range(yci2)[2]-range(yci2)[1]
    YLim2 <- c(min(yci2) - 0.05*rangey2, max(yci2) + 0.05*rangey2)}
  else {yci2 <- NULL}
  
  YLim <- c(min(YLim1[1], YLim2[1]), max(YLim1[2], YLim2[2]))
  
  
  plot(x1, y1, col = col[1], pch = pch, type = type, lty = lty, lwd = lwd, cex = cex, 
       xlab = xl1, ylab = yl1, xlim = XLim, ylim = YLim, las = 1, bg = bg)
  par(new=TRUE)
  plot(x2, y2, col = col[2], pch = pch, type = type, lty = lty, lwd = lwd, cex = cex,
       xlab = "", ylab = "", xlim = XLim, ylim = YLim, bg = bg, axes = FALSE)
  
  if (!is.null(yci1[1])){
    if (col[1] == "black") {
      polygon(xci1, yci1, col = rgb(red = 0, green = 0, blue = 0, alpha = 0.25), border = colf)
    } else {
      polygon(xci1, yci1, col = rgb(red = 0.89, green = 0.1, blue = 0.11, alpha = 0.25), border = colf)
    }
  }
  if (!is.null(yci2[1])){
    if (col[2] == "gold3") {
      polygon(xci2, yci2, col = rgb(red = 0.8, green = 0.68, blue = 0, alpha = 0.5), border = colf)
    } else {
      polygon(xci2, yci2, col = rgb(red = 0.21, green = 0.39, blue = 0.55, alpha = 0.25), border = colf)
    }
  }

  abline(h = 0, lty = 2, lwd = 1)
  abline(v = 50, lty = 2, lwd = 1)

  points(x1, y1, col = col[1], pch = pch, type = type, lty = lty, lwd = lwd, cex = cex,
         xlab = xl1, ylab = yl1, xlim = XLim, ylim = YLim, las = 1, bg = bg)
  points(x2, y2, col = col[2], pch = pch, type = type, lty = lty, lwd = lwd, cex = cex,
         xlab = "", ylab = "", xlim = XLim, ylim = YLim, las = 1, bg = bg)
  
  # legend('topright', legend=c("Male", "Female"), col=c('steelblue4', "#e41a1c"), lty=c(1, 1), cex=0.8)
  
}
