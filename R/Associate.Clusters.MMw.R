Associate.Clusters.MMw = function(cl, DataA.tbc, DataA.tbp, DataS.tbc){

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
  cat("PROJECT NEW DATAA.TBP ONTO THE ALL-DATA W-SPACE IN ALLOC.CL.W2\n")

  w22 = project.on.w.CCA.2(DataA.tbp, M, J, CC2$B,CC2$mA)


  #################################

  cat("TRAIN MMw\n")
  Tr = mclustDAtrain(w2,cl, G=1:5)

  cat("PROJECT MMw\n")
  Te = mclustDAtest(w22,Tr)

  new.cl = as.numeric(summary(Te)$classification)


  return(new.cl)

}
