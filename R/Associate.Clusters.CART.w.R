Associate.Clusters.CART.w = function(cl, DataA.tbc, DataA.tbp, DataS.tbc){

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
  #cat("COMPUTE CCA.Vrac.2(DataS.tbc,I,DataA.tbp) TO GET W VARIABLES IN ALLOC.CL.CART.W\n")
  #CC2 = CCA.Vrac.2(DataS.tbc,I,DataA.tbc)
  cat("COMPUTE cancor.Vrac(DataS.tbc,DataA.tbp) TO GET W VARIABLES IN ALLOC.CL.CART.W\n")
  CC2 = cancor.Vrac(DataS.tbc,DataA.tbc)
  #w2 = CC2$w
  w2 = (CC2$w)[,1:M]


  ###
  cat("PROJECT NEW DATAA.TBP ONTO THE ALL-DATA W-SPACE IN ALLOC.CL.CART.W\n")

  w22 = project.on.w.CCA.2(DataA.tbp, M, J, CC2$B,CC2$mA)

  #cat("dim(w2)=",dim(w2),"\n")
  #cat("dim(w22)=",dim(w22),"\n")

  ###
  cat("COMPUTING CART TREE\n")

  trw = try(tree(as.factor(cl)~.,as.data.frame(w2),mindev=0.001))
  if(length(trw)==1){
    trw = try(tree(as.factor(cl)~.,as.data.frame(w2),mindev=0.003))
  }

  ###
  cat("ALLOCATING NEW DAYS TO CLUSTERS THROUGH CART TREE A\n")

  clpw = predict.tree(trw,as.data.frame(w22))

  new.cl = array(NaN,dim=Nday.p)
  for(i in 1:Nday.p){
    new.cl[i] = order(clpw[i,])[Nc]
  }

  totnewCL = array(0,dim=Nc)
  for(i in 1:(length(new.cl))){
    totnewCL[new.cl[i]] = totnewCL[new.cl[i]]+1
  }
  cat("totnewCL=",totnewCL,"\n")

  return(new.cl)

}

