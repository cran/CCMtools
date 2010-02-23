Associate.Clusters.MM = function(cl, DataA.tbc, DataA.tbp, DataS.tbc){


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


  ### projection of the new data onto the factorial space

  # centering and reduction of the data

  X.Centered = array(NaN,dim=dim(DataA.tbp))
  for(j in 1:(dim(DataA.tbp)[2])){
    X.Centered[,j] = (DataA.tbp[,j] - center[j]) / SDEV[j]
  }


  # projection on the rotation matrix
  
  X.B <- (X.Centered %*% Rot.A)[,1:p]


  ##### MM

  cat("TRAIN MM\n")
  Tr = mclustDAtrain(X.A,cl, G=1:5)

  cat("PROJECT MM\n")
  Te = mclustDAtest(X.B,Tr)

  new.cl = as.numeric(summary(Te)$classification)


  return(new.cl)

}
