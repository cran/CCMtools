######################################################################

cancor.Vrac = function(DataS,DataA){

  CCA = try(cancor(DataS,DataA,xcenter=TRUE, ycenter=TRUE))

  if(length(CCA)==1){
    cat("!!!!!! length(CCA)==1 !!!!!!! IN cancor.Vrac !!!!!!!!!!!!!!\n")
    DataS2 = DataS
    for(w in 1:dim(DataS)[2]){
      for(i in 1:dim(DataS)[1]){
        DataS2[i,w] = DataS[i,w] + runif(1,-0.01,0.01)
      }
    }
    CCA = cancor(DataS,DataA,xcenter=TRUE, ycenter=TRUE)
  }

  r = CCA$cor
  xcoef = CCA$xcoef
  ycoef = CCA$ycoef
  xm = CCA$xcenter
  ym = CCA$ycenter

  Nday = dim(DataS)[1]

  ###
  # Matrices of x and y mean to be removed to data to multiply to lin. coef. and get v and w
  Mxmean = array(NaN,dim=c(Nday,length(xm)))
  for(i in 1:(length(xm))){
    Mxmean[,i]=xm[i]
  }

  Mymean = array(NaN,dim=c(Nday,length(ym)))
  for(i in 1:(length(ym))){
    Mymean[,i]=ym[i]
  }
  ###

  ###
  # Increase sizes of the coef matrices if needed
  if(dim(xcoef)[2] < dim(DataS)[2]){
    cat("extend matrix xcoef\n")
    xcoef.2 = array(0,dim=c(dim(DataS)[2],dim(DataS)[2]))
    xcoef.2[1:dim(xcoef)[2],1:dim(xcoef)[2]] = xcoef
    xcoef = xcoef.2
  }
  if(dim(ycoef)[2] < dim(DataA)[2]){
    cat("extend matrix ycoef\n")
    ycoef.2 = array(0,dim=c(dim(DataA)[2],dim(DataA)[2]))
    ycoef.2[1:dim(ycoef)[2],1:dim(ycoef)[2]] = ycoef
    ycoef = ycoef.2
  }
  ###

  v = (DataS - Mxmean) %*% xcoef
  w = (DataA - Mymean) %*% ycoef

  CV = min(length(xm),length(ym))

  return(list(v=v, w=w, CV=CV, r=r, mS=xm, mA=ym, A=xcoef, B=t(ycoef)))

}

######################################################################
