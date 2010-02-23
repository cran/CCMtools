Associate.Clusters.wcv.2 = function(cl, DataA.tbc, DataA.tbp, DataS.tbc){

  Nc = max(cl)
  Nday.c = dim(DataA.tbc)[1]
  Nday.p = dim(DataA.tbp)[1]
  J = dim(DataA.tbc)[2]
  I = dim(DataS.tbc)[2]

  M = min(I,J) # = dim(Bc)[2]

  if(J != (dim(DataA.tbp)[2])){
    stop("dim(DataA.tbc)[2] != dim(DataA.tbp)[2] in Associate.Clusters\n")
  }
  if(Nday.c != (length(cl))){
    stop("Nday.c != length(cl) in Associate.Clusters\n")
  }


  totCL = array(0,dim=Nc)
  for(i in 1:Nday.c){
    totCL[cl[i]] = totCL[cl[i]] + 1
  }
  #cat("totCL=",totCL,"\n")


  ###
  cat("COMPUTE cancor.Vrac(DataS.tbc,DataA.tbp) TO GET W VARIABLES IN ALLOC.CL.W2\n")
  CC2 = cancor.Vrac(DataS.tbc,DataA.tbc)
  w2 = (CC2$w)[,1:M]


  ###
  cat("COMPUTING MEANS OF THE W-PATTERNS IN ALLOC.CL.W2\n")

  MeanCLw = array(0, dim=c(Nc,M))
  for(i in 1:Nday.c){
    MeanCLw[cl[i],] = MeanCLw[cl[i],] + w2[i,]
  }
  for(c in 1:Nc){
    MeanCLw[c,] = MeanCLw[c,]/totCL[c]
  }



  ###
  cat("PROJECT NEW DATAA.TBP ONTO THE ALL-DATA W-SPACE IN ALLOC.CL.W2\n")

  w22 = project.on.w.CCA.2(DataA.tbp, M, J, CC2$B,CC2$mA)


  ###
  cat("COMPUTING EUCLIDIAN DIST. ON W-SPACE AND ALLOCATING DAYS TO PATTERNS IN ALLOC.CL.W2\n")

  new.cl = array(NaN,dim=Nday.p)

  for(i in 1:Nday.p){
    Ed = array(NaN,dim=Nc)
    for(c in 1:Nc){
      Ed[c] = Euclidian.distance(w22[i,],MeanCLw[c,])
    }
    new.cl[i] = order(Ed)[1]
  }


  return(new.cl)

}

