Associate.Clusters.CART.A.and.w = function(cl, DataA.tbc, DataA.tbp, DataS.tbc){

  Nc = max(cl)
  Nday.c = dim(DataA.tbc)[1]
  Nday.p = dim(DataA.tbp)[1]
  J = dim(DataA.tbc)[2]
  I = dim(DataS.tbc)[2]

  M = min(I,J)

  if(J != (dim(DataA.tbp)[2])){
    stop("dim(DataA.tbc)[2] != dim(DataA.tbp)[2] in Associate.Clusters\n")
  }
  if(Nday.c != (length(cl))){
    stop("Nday.c != length(cl) in Associate.Clusters\n")
  }

  ###
  cat("COMPUTING CART TREE A IN ASS.CL.CART.A.and.w\n")

  trA = try(tree(as.factor(cl)~.,as.data.frame(DataA.tbc),mindev=0.001))
  if(length(trA)==1){
    trA = try(tree(as.factor(cl)~.,as.data.frame(DataA.tbc),mindev=0.003))
  }


  ###
  #cat("COMPUTE cancor.Vrac(DataS.tbc,DataA.tbp) TO GET W VAR. IN ALLOC.CL.CART.A.and.W\n")
  CC2 = cancor.Vrac(DataS.tbc,DataA.tbc)
  w2 = (CC2$w)[,1:M]


  ###
  cat("PROJECT NEW DATAA.TBP ONTO THE ALL-DATA W-SPACE IN ALLOC.CL.CART.A.and.W\n")

  w22 = project.on.w.CCA.2(DataA.tbp, M, J, CC2$B,CC2$mA)


  ###
  cat("COMPUTING CART TREE IN ALLOC.CL.CART.A.and.W\n")

  #trw = tree(as.factor(cl)~.,as.data.frame(w2),mindev=0.001)
  trw = try(tree(as.factor(cl)~.,as.data.frame(w2),mindev=0.001))
  if(length(trw)==1){
    trw = try(tree(as.factor(cl)~.,as.data.frame(w2),mindev=0.003))
  }



  ###
  cat("ALLOCATING NEW DAYS TO CLUSTERS THROUGH CART TREE A IN ALLOC.CL.CART.A.and.W\n")

  clpA = predict(trA,as.data.frame(DataA.tbp))

  ###
  cat("ALLOCATING NEW DAYS TO CLUSTERS THROUGH CART TREE w IN ALLOC.CL.CART.A.and.W\n")

  clpw = predict(trw,as.data.frame(w22))


  ###
  cat("ALLOCATING NEW DAYS TO CLUSTERS THROUGH CART TREE A AND w\n")

  clp = clpA+clpw

  new.cl = array(NaN,dim=Nday.p)
  for(i in 1:Nday.p){
    new.cl[i] = order(clp[i,])[Nc]
  }


  totnewCL = array(0,dim=Nc)
  for(i in 1:(length(new.cl))){
    totnewCL[new.cl[i]] = totnewCL[new.cl[i]]+1
  }
  cat("totnewCL=",totnewCL,"\n")


  return(new.cl)

}

