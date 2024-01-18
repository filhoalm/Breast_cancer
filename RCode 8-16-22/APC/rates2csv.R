rates2csv <- function(R, FILE = "rates.csv")
  
{
  A <- nrow(R$events)
  P <- ncol(R$events)
  comma = ','
  commas <- paste(replicate(P-1, ','), collapse="")
  
  # fprintf(fid, ['Title: ' R.fullname commas '\n']);
  
  s1 <- paste(c('Title: ', R$fullname, commas), collapse = "")
  s2 <- paste(c('Description: ', R$description, commas), collapse = "")
  s3 <- paste(c('Start Year: ', toString(R$p[1]), commas), collapse = "")
  s4 <- paste(c('Start Age: ', toString(R$a[1]), commas), collapse = "")
  s5 <- paste(c('Interval (Years): ', toString(R$a[2]-R$a[1]), commas), collapse = "")
 
  DATA <- matrix(NaN, A, 2*P)
  DATA[,seq(1, 2*P-1, by = 2)] <- R$events
  DATA[,seq(2, 2*P, by = 2)] <- R$offset
  
  cat(s1, s2, s3, s4, s5, file = FILE, sep = "\n", append = FALSE)
  write(t(DATA), file = FILE, append = TRUE, sep = ',', ncolumns = 2*P)
  
  Fout <- list(s1 = s1, s2 = s2, s3 = s3, s3 = s4, s5 = s5, DATA = DATA)
  Fout
  
}