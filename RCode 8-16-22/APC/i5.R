require(pracma)

i5 = function(R, ...){
  # RATES/I5 Interpolate rates for 5-year age groups down to 1-year age groups.
  #
  # R1 = I5(R5)
  #
  # Property-value pairs:
    #
  # 'Method' - 'linear' (default) or 'cubic'.
  #
  # Interpolation is carried out separately for cumulative person-years and
  # cumulative numbers of events. Using 'linear' interpolation will match
  # total within the 5 year age groups, but won't be as smooth.
  
  PVP = checkPVPAIRS(...)
  
  # Get rid of NAs:
  #R$events[which(is.na(R$events))] = 0
  #R$offset[which(is.na(R$offset))] = mean(R$offset[!is.na(R$offset)])
  
  if(PVP$method == 'cubic'){
    P = ncol(R$events)
    a = matrix(R$ages)
    sya = a[1]:a[length(a)]
    A5 = length(sya)-1
    ipy = matrix(NaN, A5, P)
    iev = matrix(NaN, A5, P)
    
    
    for(p in 1:P){
      py = matrix(R$offset[,p])
      ev = matrix(R$events[,p])
      
      ipypc = interp1(x = c(a), 
                      y = c(0, cumsum(py)), 
                      xi = sya,
                      method = 'cubic')
      ipyp0 = pmax(0, diff(ipypc))
      ipy[,p] = sum(py)*(ipyp0/sum(ipyp0))
     
      ievpc = interp1(x = c(a),
                      y = c(0, cumsum(ev)),
                      xi = sya,
                      method = 'cubic')
      ievp0 = pmax(0, diff(ievpc))
      iev[,p] = sum(ev)*(ievp0/sum(ievp0))
    }
    
    Ri5 = rates(iev, ipy,
                fullname = paste(R$fullname, ' (5-to-1 cubic)'),
                label = paste(R$label, ' (cubic)'),
                ages = sya,
                periods = R$periods,
                offset_tick = R$offset_tick,
                offset_units = R$offset_units)
    
  } else if(PVP$method == 'linear'){
    P = ncol(R$events)
    a = matrix(R$ages)
    sya = a[1]:a[length(a)]
    A5 = length(sya)-1
    ipy = matrix(NaN, A5, P)
    iev = matrix(NaN, A5, P)
    
    for(p in 1:P){
      py = matrix(R$offset[,p])
      ev = matrix(R$events[,p])
      ipypc = interp1(x = c(a), 
                     y = c(0, cumsum(py)), 
                     xi = sya,
                     method = 'linear')
      ipyp0 = pmax(0, diff(ipypc))
      ipy[,p] = sum(py)*(ipyp0/sum(ipyp0))
      ievpc = interp1(x = c(a), 
                      y = c(0, cumsum(ev)), 
                      xi = sya,
                      method = 'linear')
      ievp0 = pmax(0, diff(ievpc))
      iev[,p] = sum(ev)*(ievp0/sum(ievp0))
    }

    Ri5 = rates(iev, ipy,
                fullname = paste(R$fullname, ' (5-to-1 linear)'),
                label = paste(R$label, ' (linear)'),
                ages = sya,
                periods = R$periods,
                offset_tick = R$offset_tick,
                offset_units = R$offset_units)
    
  }
}

checkPVPAIRS = function(...){
  # Helper function. Validate property-value pairs.
  
  PVP = list(method = 'linear')
  
  lArgin = list(...)
  
  while(length(lArgin)>0){
    propi = names(lArgin)[[1]]
    vali = lArgin[[1]]
    lArgin = lArgin[-1] # Removes first argument
    
    if(tolower(propi) == 'method'){
      if(!is.character(vali)){
        stop('The "method" property value must be a string.')
      }
      
      vali = tolower(vali[1])
      
      if(tolower(vali) == 'l' | tolower(vali) == 'linear'){
        PVP$method = 'linear'
      } else if(tolower(vali) == 'c' | tolower(vali) == 'cubic'){
        PVP$method = 'cubic'
      } else{
        stop('The values of the "method" property must be "linear" or "cubic".')
      }
    } else{
      stop('Unrecognized property.')
    }
  }
  
  return(PVP)
}