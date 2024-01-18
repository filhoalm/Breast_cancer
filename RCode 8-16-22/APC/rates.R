rates <- function(EVENTS, OFFSET, 
                  fullname = character(0), 
                  label = character(0), 
                  description = character(0), 
                  event_label = "events", 
                  offset_units = "offset units", 
                  offset_tick = 100000, 
                  api = c(NaN, NaN, NaN),
                  ages = NaN,
                  periods = NaN)
  
{
  # rates object constructor.
  
  
  ###
  # Validate EVENTS and OFFSET
  ###
  if (is.matrix(EVENTS) && all(EVENTS >= 0))
  {
    A <- nrow(EVENTS)
    P <- ncol(EVENTS)
  }
  else
  {
    stop("events must be a matrix of non-negative values.")
  }
  
  if (!(is.matrix(OFFSET) && all(OFFSET >= 0) && nrow(OFFSET)==A && ncol(OFFSET)==P))
  {
    stop("offset must be a matrix of non-negative values the same size as events.")
  }
  
  ###
  # Validate ages and periods specified by api or ages/periods combination.
  ###
  if (!all(is.nan(api)))
  {
    if ( !( length(api==3) && is.numeric(api) ) )
    {stop("api must be a 3 element vector.")}
    if (!all(api==round(api)))
    {stop("api values must be single-years.")}
    
    a0 <- api[1]
    p0 <- api[2]
    INTERVAL <- api[3]
    ages <- a0 + INTERVAL*(0:A)
    periods <- p0 + INTERVAL*(0:P)
    a <- ages[1:A-1] + 0.5*INTERVAL
    p <- periods[1:P-1] + 0.5*INTERVAL
  }
  else
  {
  if (! ( !any(is.nan(ages)) && !any(is.nan(periods)) && 
          all(ages==round(ages)) && all(periods==round(periods)) && 
          (length(ages)==A+1) && (length(periods)==P+1) ) )  
    {stop("Invalid cutpoints for ages or periods.")}  
  }
  

  ###
  # Validate text inputs.
  ###
  
  if (!is.character(fullname))
  {stop('fullname must be a character string.')}
  else
  {fullname <- gsub("^\\s+|\\s+$", '', fullname)}
  
  if (length(fullname)==0)
  {fullname <- paste(c(toString(A), '-by-', toString(P), ' rates object'), collapse = "")}
  
  if (length(description)==0)
  {description <- paste(c('Created ', date()), collapse = "")}
 
  if (!is.character(event_label))
  {stop('event_label must be a character string.')}
  
  if (!is.character(offset_units))
  {stop('offset_units must be a character string.')}
  
  if (length(label)==0)
 {label <- fullname}
  da <- ages[2:(A+1)] - ages[1:A]
  DELTAa <- da[1]
  
  if (!all(da==1)){
    aL <- as.character(seq(from = ages[1], to = ages[A], by = DELTAa))
    TMP <- seq(from = ages[2], to = ages[A+1], by = DELTAa) - 1
    aH <- as.character(TMP) 
    age_labels <- paste(cbind(aL), ' - ', cbind(aH))}
  else{
    age_labels <- as.character(seq(from = ages[1], to = ages[A], by = 1))}
  
  dp <- periods[2:(P+1)] - periods[1:P]
  DELTAp <- dp[1]
  if (!all(dp==1)){
  pL <- as.character(seq(from = periods[1], to = periods[P], by = DELTAp))
  TMP <- seq(from = periods[2], to = periods[P+1], by = DELTAp) - 1
  pH <- as.character(TMP)
  per_labels <- paste(cbind(pL), ' - ', cbind(pH))}
  else{
  per_labels <- as.character(seq(from = periods[1], to = periods[P], by = 1))}

  a <- ages[1:A] + da/2
  p <- periods[1:P] + dp/2
  
  R <- list(fullname = fullname,
            label = label,
            description = description,
            events = EVENTS,
            event_label = event_label,
            offset = OFFSET,
            offset_units = offset_units,
            offset_tick = offset_tick,
            ages = ages,
            age_labels = age_labels,
            a = a,
            periods = periods,
            per_labels = per_labels,
            p = p)
  
  R
}

