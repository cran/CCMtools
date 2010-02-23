Associate.Clusters.Gaussian.w = function(cl, DataA.tbc, DataA.tbp, DataS.tbc){

  if( (dim(DataA.tbc)[2])!=(dim(DataA.tbp)[2]) ){
    stop("dim(DataA.tbc)[2] != dim(DataA.tbp)[2] in Associate.Clusters (Gaussian.A)\n")
  }

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



  ###
  cat("COMPUTE cancor.Vrac(DataS.tbc,DataA.tbp) TO GET W VARIABLES\n")
  CC2 = cancor.Vrac(DataS.tbc,DataA.tbc)
  w2 = (CC2$w)[,1:M]



  ### Means and cov-matrices for each cluster

  cat("Compute means and covariance matrix for each cluster\n")

  Vec.mean = array(NaN,dim=c(Nc,M))
  Cov = array(NaN,dim=c(Nc,M,M))
  pi = array(Nc)

  for(c in 1:Nc){

    DataA2 = kept.data2D.from.cluster(cl,c,Nc,w2)

    pi[c] = (dim(DataA2)[1]) / (dim(w2)[1])
    Vec.mean[c,] = apply(DataA2,2,mean)
    Cov[c,,] = cov(DataA2)
  
  } 



  ### projection of the new data onto the canonical space
  cat("PROJECT NEW DATAA.TBP ONTO THE ALL-DATA W-SPACE\n")

  w22 = project.on.w.CCA.2(DataA.tbp, M, J, CC2$B,CC2$mA)



  ### Assign each day to the clusters by pi*fi > pj*fj

  new.cl = array(NaN,dim=dim(w22)[1])

  for(i in 1:dim(new.cl)){

    data = w22[i,]

    c = 1
    bestValue = pi[c] * dmvnorm(data ,Vec.mean[c,],Cov[c,,])

    for(j in 2:Nc){
      val = pi[j] * dmvnorm(data,Vec.mean[j,],Cov[j,,])
      if(val>bestValue){
        bestValue = val
        c = j
      }
    }
    new.cl[i] = c
    #cat(c," ")
  }

  return(new.cl)

}
  

