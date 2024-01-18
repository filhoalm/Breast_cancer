type <- function(R, comp = 'r')
{
  # Extract features of a rates object.
  A <- nrow(R$events)
  P <- ncol(R$events)
  
  if (comp == "r") {
    Tout <- R$offset_tick*R$events/R$offset
    dn <- paste('Rates - ', R$fullname)
    
  } else if (comp == "e") {
    Tout <- R$events
    dn <- paste("Events -", R$fullname)
    
  } else if (comp == "o") {
    Tout <- R$offset
    dn <- paste('Offset - ', R$fullname)
    
  } else if (comp == "eo") {
    
    DATA <- matrix(NaN, nrow = A, ncol = 2*P)
    DATA[,seq.int(1, 2*P, 2)]<-R$events
    DATA[,seq.int(2, 2*P, 2)]<-R$offset
    Tout <- DATA
    dn <- paste("Events & Offset -", R$fullname)
    
  } else if (comp == "er") {
    r <- R$offset_tick*R$events/R$offset
    DATA <- matrix(NaN, nrow = A, ncol = 2*P)
    DATA[,seq.int(1, 2*P, 2)]<-R$events
    DATA[,seq.int(2, 2*P, 2)]<-r
    Tout <- DATA
    dn <- paste("Events and Rates - ", R$fullname)
    
  } else if (comp == "eor") {
    
    r <- R$offset_tick*R$events/R$offset
    DATA <- matrix(NaN, nrow = A, ncol = 3*P)
    DATA[,seq.int(1, 3*P, 3)]<-R$events
    DATA[,seq.int(2, 3*P, 3)]<-R$offset
    DATA[,seq.int(3, 3*P, 3)]<-r
    Tout <- DATA
    dn <- paste("Events, offset, and Rates - ", R$fullname)
    
  } else if (comp == "rci") {
    
    r <- R$offset_tick*R$events/R$offset
    v <- (R$offset_tick^2)*R$events/R$offset^2
    cilo <- r - 1.96*sqrt(v)
    cilo[cilo<0] <- 0
    cihi <- r + 1.96*sqrt(v)
    DATA <- matrix(NaN, nrow = A, ncol = 3*P)
    DATA[,seq.int(1, 3*P, 3)]<-r
    DATA[,seq.int(2, 3*P, 3)]<-cilo
    DATA[,seq.int(3, 3*P, 3)]<-cihi
    Tout <- DATA
    dn <- paste("Rates and 95% CI - ", R$fullname)
    
  } else {
    
  }
  
  Tout <- list(name = dn, DATA = Tout, ages = R$ages, periods = R$periods) 
  Tout
  
}