mh_distance <- function(x, y) {
  
  #Create vectors of x and y
  x_char <- unlist(strsplit(as.character(x), NULL))
  y_char <- unlist(strsplit(as.character(y), NULL))
  
  #If numeric then take the absolute value
  if (is.numeric(x) & is.numeric(y)) {
    x_char <- unlist(strsplit(as.character(abs(x)), NULL))
    y_char <- unlist(strsplit(as.character(abs(y)), NULL))
  }
  
  # Check Inputs
  #Both same types
  if (mode(x) != mode(y)) {
    warning("x and y are not both logical, numeric, or characters")
    return(-1)
  }
  
  # Same Length
  if (typeof(x) != "logical" & typeof(y) != "logical") {
    if (length(x_char) != length(y_char)) {
      warning("x and y do not have the same number of digits or letters")
      return(-1)
    }
  }
  
  #NA, NaN, Inf or -Inf
  if (is.infinite(x) | is.nan(x) | is.na(x) || 
      is.infinite(y) | is.nan(y) | is.na(y)) {
    warning("x is NA, NaN, Inf or -Inf")
    return(-1)
  }
  
  #Decimals (fraction)
  if ( is.numeric(x) & is.numeric(y)) {
    if (x %% 1 != 0 || y %% 1 != 0) {
      warning("x or y contain decimal values")
      return (-1)
    }
  }
  
  
  #Run Calculations
  #Character or Numeric
  if ((is.character(x) & is.character(y)) || (is.numeric(x) & is.numeric(y))) {
    count <- 0
    i <- 1
    while(i <= length(x_char)) {
      if (x_char[i] != y_char[i]) {
        count = count + 1
      }
      i <- i + 1
    }
    return (count)
  }
  
  #Logical
  else if (is.logical(x) & is.logical(y)) {
    value <- abs(x - y)
    return (value)
  }
  
  else{
    warning("x and y are not both logical, numeric, or characters")
  }
  
}