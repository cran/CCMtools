project.on.w.CCA.2 = function(DataA.tbp, I, J, B, mA=NaN){

  #J = dim(DataA.tbp)
  D2p = array(NaN,dim=dim(DataA.tbp))
  Nday = dim(D2p)[1]

  if(is.na(mA)){
    cat(" mA = NaN\n")
    mA = array(NaN,dim=J)
    for(j in 1:J){
      mA[j] = mean(DataA.tbp[,j]) # => centered by grid-point
      ###m2 = mean(DataA.tbp) # => centered for all grid-points together ?????
      D2p[,j] = DataA.tbp[,j] - mA[j]
    }
  }
  else{
    cat(" mA != NaN\n")
    for(j in 1:J){
      D2p[,j] = DataA.tbp[,j] - mA[j]
    }
  }
  


  w = array(NaN,dim=c(Nday,min(I,J)))

  for(i in 1:Nday){
    for(m in 1:(min(I,J))){
      w[i,m] = t(B[m,]) %*% D2p[i,]
    }
  }

  return(w)

}
