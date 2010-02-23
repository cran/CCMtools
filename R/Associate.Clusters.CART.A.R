########################################################################

Associate.Clusters.CART.A = function(cl, DataA.tbc, DataA.tbp){

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
  cat("COMPUTING CART TREE\n")

  trA = try(tree(as.factor(cl)~.,as.data.frame(DataA.tbc),mindev=0.001))
  if(length(trA)==1){
    trA = try(tree(as.factor(cl)~.,as.data.frame(DataA.tbc),mindev=0.003))
  }

  ###
  cat("ALLOCATING NEW DAYS TO CLUSTERS THROUGH CART TREE A\n")

  clpA = predict(trA,as.data.frame(DataA.tbp))

  new.cl = array(NaN,dim=Nday.p)
  for(i in 1:Nday.p){
    new.cl[i] = order(clpA[i,])[Nc]
  }


  totnewCL = array(0,dim=Nc)
  for(i in 1:(length(new.cl))){
    totnewCL[new.cl[i]] = totnewCL[new.cl[i]]+1
  }
  cat("totnewCL=",totnewCL,"\n")


  return(new.cl)

}
