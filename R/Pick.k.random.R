Pick.k.random = function(k,tab2d){

  d = dim(tab2d)[1]
  pickk = runif(k,1,d)
  pk = round(pickk)

  return(pk)

}
