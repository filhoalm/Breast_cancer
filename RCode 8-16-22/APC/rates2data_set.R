rates2data_set <- function(R) {
  # Convert rates object to matrix.
  
  A <- nrow(R$events)
  P <- ncol(R$events)
  
  da <- R$ages[2:(A+1)] - R$ages[1:A]
  D.a <- R$ages[1:A] + 0.5*da
  
  dp <- R$periods[2:(P+1)] - R$periods[1:P]
  D.p <- R$periods[1:P] + 0.5*dp
  
  
  
  ADATA <- kronecker(matrix(1, nrow=P),  matrix(D.a, nrow=A))
  PDATA <- kronecker(matrix(D.p, nrow=P), matrix(1, nrow=A))
  CDATA <- PDATA - ADATA
  D.c <- sort(c(unique(CDATA)))
  E <- c(R$events)
  O <- c(R$offset)
  D.DATA = cbind(ADATA, PDATA, CDATA, E, O)
  colnames(D.DATA)<-c("Age", "Period", "Cohort", "Events", "Offset")
  
  
  D <- list(name = R$name,
            description = R$description,
            DATA = D.DATA, 
            a = D.a,
            p = D.p,
            c = D.c)
  D
  
}

