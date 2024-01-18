csv2rates <- function(FILE) {
  # Converts properly-formatted CSV file into Rates object
  #
  # rate = csv2rates("rates.csv")
  #
  # CSV files should be formatted with the following header
  # 
  # Title: title of the file
  # Description: when file was created
  # Start Age: youngest age considered
  # Start Year: earliest year considered
  # Age Interval (Years): interval between ages
  # Period Interval (Years): interval between years in period
  #
  # The function also accepts Interval (Years) in lieu of Age Interval (Years)
  # and Period Interval (Years). In this case, the ages and periods in the
  # corresponding Rates object will be spaced the same interval apart.
  
  StartYear <- 0
  StartAge <- 0
  Interval <- 1
  
  header <- scan(FILE, nlines = 7, what = character(0), sep = '/', quiet = 1)
  # Strip any excess delimeters
  header <- gsub(",", "", header)
  # Strip leading and trailing white space
  header <- gsub("^\\s+|\\s+$", "", header)
  H = length(header)
  
  Interval <- 0
  IntervalAge <- 0
  IntervalPeriod <- 0
  
  k <- 0
  for (h in 1:H) {
    headerh = header[h]
    nc = nchar(headerh)
    f <- regexpr("Title:", headerh, ignore.case = TRUE)
    d <- regexpr("Description:", headerh, ignore.case = TRUE)
    p <- regexpr("Start Year:", headerh, ignore.case = TRUE)
    a <- regexpr("Start Age:", headerh, ignore.case = TRUE)
    i <- regexpr("Interval \\(Years\\):", headerh, ignore.case = TRUE)
    ia <- regexpr("Age Interval \\(Years\\):", headerh, ignore.case = TRUE)
    ip <- regexpr("Period Interval \\(Years\\):", headerh, ignore.case = TRUE)
    if (f==1)
    {
      fullname <- gsub("^\\s+|\\s+$", "", substr(headerh, attr(f, "match.length")+1, nc))
      k <- k + 1
    }
    if (d==1)
    {
      description <- gsub("^\\s+|\\s+$", "", substr(headerh, attr(d, "match.length")+1,nc))
      k <- k + 1
    }
    if (p==1)
    {
      StartYear <- as.numeric(substr(headerh, attr(p, "match.length")+1,nc))
      k <- k + 1
    }
    if (a==1)
    {
      StartAge <- as.numeric(substr(headerh, attr(a, "match.length")+1,nc))
      k <- k + 1
    }
    
    if (i==1)
    {
      Interval <- as.numeric(substr(headerh, attr(i, "match.length")+1,nc))
      k <- k + 1
    }
    
    if (ia==1)
    {
      IntervalAge <- as.numeric(substr(headerh, attr(ia, "match.length")+1,nc))
      k <- k + 1
    }
    
    if (ip==1)
    {
      IntervalPeriod <- as.numeric(substr(headerh, attr(ip, "match.length")+1,nc))
      k <- k + 1
    }
  }
  # DATA is a data.frame
  DATA <- read.table(FILE, skip = k, header = FALSE, sep = ',')
  PP <- ncol(DATA)
  A <- nrow(DATA)
  E = as.matrix(DATA[, seq(1, PP-1, by = 2)])
  dimnames(E) <- NULL
  O = as.matrix(DATA[, seq(2, PP, by = 2)])
  dimnames(O) <- NULL
  
  if (Interval >= 1) {
    a <- seq(from = StartAge, by = Interval, to = StartAge + Interval*A)
    p <- seq(from = StartYear, by = Interval, to = StartYear + Interval*PP/2)
  }
  
  if (IntervalAge >= 1) {
    a <- seq(from = StartAge, by = IntervalAge, to = StartAge + IntervalAge*A)
  }
  
  if (IntervalPeriod >= 1) {
    p <- seq(from = StartYear, by = IntervalPeriod, to = StartYear + IntervalPeriod*PP/2)
  }
  
  R <- rates(E, O,
             fullname = fullname,
             description = description,
             ages = a,
             periods = p)
  R
}