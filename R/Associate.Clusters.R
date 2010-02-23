Associate.Clusters = function(cl, DataA.tbc, DataA.tbp){

  Nc = max(cl)
  Nday.c = dim(DataA.tbc)[1]
  Nday.p = dim(DataA.tbp)[1]
  J = dim(DataA.tbc)[2]

  if(J != (dim(DataA.tbp)[2])){
    stop("dim(DataA.tbc)[2] != dim(DataA.tbp)[2] in Associate.Clusters\n")
  }
  if(Nday.c != (length(cl))){
    stop("Nday.c != length(cl) in Associate.Clusters\n")
  }

  ###
  cat("Computing mean cluster patterns\n")

  MeanCL = array(0, dim=c(Nc, J))
  totCL = array(0,dim=Nc)

  for(i in 1:Nday.c){
    MeanCL[cl[i],] = MeanCL[cl[i],] + DataA.tbc[i,]
    totCL[cl[i]] = totCL[cl[i]] + 1
  }
  cat("totCL=",totCL,"\n")
  for(c in 1:Nc){
    MeanCL[c,] = MeanCL[c,]/totCL[c]
  }
  ###

  
  cat("Computing Euclidian distances and allocating days to patterns\n")

  new.cl = array(NaN,dim=Nday.p)
  for(i in 1:Nday.p){
    Ed = array(NaN,dim=Nc)
    for(c in 1:Nc){
      Ed[c] = Euclidian.distance(DataA.tbp[i],MeanCL[c])
    }
    new.cl[i] = order(Ed)[1]
  }


  totnewCL = array(0,dim=Nc)
  for(i in 1:(length(new.cl))){
    totnewCL[new.cl[i]] = totnewCL[new.cl[i]]+1
  }
  cat("totnewCL=",totnewCL,"\n")


  return(new.cl)

}

