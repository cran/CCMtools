Associate.Clusters.wcv = function(cl, DataA.tbc, DataA.tbp, DataS.tbc){

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



#######################

  xp = NaN
  MeanCLw = array(0, dim=c(Nc, M))


  Bc = array( NaN, dim = c(Nc, J, J) )
  mAc = array(NaN,dim=c(Nc,J))

  cat("Define CCA model for each cluster:")

  for(c in 1:Nc){

    cat(c,"")
    DataS2 = kept.data2D.from.cluster(cl,c,Nc,DataS.tbc)
    DataA2 = kept.data2D.from.cluster(cl,c,Nc,DataA.tbc)
  
    CC2 = cancor.Vrac(DataS2,DataA2)
    mAc[c,] = CC2$mA
    Bc[c,,] = CC2$B


    ## computation of the mean w patterns
    w2 = (CC2$w)[,1:M]
    for(i in 1:(totCL[c])){
      MeanCLw[c,] = MeanCLw[c,] + w2[i,]
    }
    MeanCLw[c,] = MeanCLw[c,]/totCL[c]
  }
  cat("\n")


#######################



  cat("PROJECT DataA.tbp ONTO THE W-SPACES\n")
  w = array(NaN,dim=c(Nc,Nday.p,M))
  for(c in 1:Nc){
    cat("Projections onto the cluster",c,"w-space\n")
    w[c,,] = project.on.w.CCA.2(DataA.tbp, M, J, Bc[c,,],mAc[c,])
  }



  ###

  cat("COMPUTING EUCLIDIAN DIST. ON W-SPACE AND ALLOCATING DAYS TO PATTERNS\n")

  new.cl = array(NaN,dim=Nday.p)

  for(i in 1:Nday.p){
    Ed = array(NaN,dim=Nc)
    for(c in 1:Nc){
      Ed[c] = Euclidian.distance(w[c,i,],MeanCLw[c,])
    }
    new.cl[i] = order(Ed)[1]
  }


  return(new.cl)

}

