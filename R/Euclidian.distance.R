Euclidian.distance <- function(X,Y){
  E=0
  if(length(X)!=length(Y)){
    cat("X and Y have different length in Euclidian.distance\n")
    return(NaN)
  }
  for(i in 1:(length(X))){
    E=(E+((X[i]-Y[i])^2))
  }
  E=sqrt(E)
  return(E)
}
