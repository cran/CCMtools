Associate.Clusters.Gaussian.A = function(cl, DataA.tbc, DataA.tbp){

  if( (dim(DataA.tbc)[2])!=(dim(DataA.tbp)[2]) ){
    stop("dim(DataA.tbc)[2] != dim(DataA.tbp)[2] in Associate.Clusters (Gaussian.A)\n")
  }

  Nc = max(cl)


  cat("compute PCA\n")
  PCA.A = prcomp(DataA.tbc, center=TRUE, scale=TRUE, retx=TRUE, tol=NULL)
  #print(summary(PCA.A))
  p = search.PCAlevel(PCA.A,95)
  X.A = PCA.A$x[,1:p]
  Rot.A = PCA.A$rotation
  SDEV = sqrt(diag(cov(DataA.tbc)))
  ##sdev = PCA.A$sdev
  center = PCA.A$center
  cat("dim(X.A)=",dim(X.A),"\n")


  ### Means and cov-matrices for each cluster

  Vec.mean = array(NaN,dim=c(Nc,p))
  Cov = array(NaN,dim=c(Nc,p,p))
  pi = array(Nc)

  for(c in 1:Nc){

    DataA2 = kept.data2D.from.cluster(cl,c,Nc,X.A)

    pi[c] = (dim(DataA2)[1]) / (dim(X.A)[1])
    Vec.mean[c,] = apply(DataA2,2,mean)
    Cov[c,,] = cov(DataA2)
  
  } 



  ### projection of the new data onto the factorial space

  # centering and reduction of the data

  X.Centered = array(NaN,dim=dim(DataA.tbp))
  for(j in 1:(dim(DataA.tbp)[2])){
    X.Centered[,j] = (DataA.tbp[,j] - center[j]) / SDEV[j]
  }


  # projection on the rotation matrix
  
  X.B <- (X.Centered %*% Rot.A)[,1:p]

  #dim(X.A)
  #dim(X.B)
  #range(X.A)
  #range(X.B)



  ### Assign each day to the clusters by pi*fi > pj*fj

  new.cl = array(NaN,dim=dim(X.B)[1])

  for(i in 1:dim(new.cl)){

    data = X.B[i,]

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
