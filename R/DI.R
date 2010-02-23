DI = function(cl.calibration, cl.check, cl.proj, DataS.Calibration, DataS.check){


  if((length(cl.check))!=(length(cl.proj))){
    stop("length(cl.check)!=length(cl.proj) in Global.proba.cost\n")
  }

  if((length(cl.check))!= (dim(DataS.check)[1])){
    stop("length(cl.check)!=dim(DataS.check)[1] in Global.proba.cost\n")
  }

  if(max(cl.check) != max(cl.proj)){
    cat("max(cl.check)=",max(cl.check)," != max(cl.proj)=",max(cl.proj)," in Ponctual.attribution.cost\n")
  }

  d = dim(DataS.check)
  nday = d[1]
  ns = d[2]
  nc = max(cl.check)




  std.calibration = array(NaN,dim=ns)
  mean.calibration = array(NaN,dim=ns)
  for(s in 1:ns){
    std.calibration[s] = sqrt(var(DataS.Calibration[,s]))
    mean.calibration[s] = mean(DataS.Calibration[,s])
  }

  DataS.Calibration.Norm = array(NaN,dim=dim(DataS.Calibration)) # c(nday, ns)

  for(s in 1:ns){
    DataS.Calibration.Norm[,s] = DataS.Calibration[,s] / std.calibration[s]

  }

  ##############


  Mean.C.real = array(NaN,dim=c(nc,ns))

  for(c in 1:nc){
    DataS2.O = kept.data2D.from.cluster(cl.calibration,c,nc,DataS.Calibration.Norm)

    for(s in 1:ns){
      Mean.C.real[c,s] = mean(DataS2.O[,s])
    }
  }



  PIsd.Calibration = array(NaN,dim=c(ns,nday))

  for(s in 1:ns){
    for(i in 1:nday){
      PIsd.Calibration[s,i] = abs(Mean.C.real[cl.check[i],s] - DataS.check.Norm[i,s]) - abs(Mean.C.real[cl.proj[i],s] - DataS.check.Norm[i])
    }
  }


  PIs.Calibration = array(NaN,dim=ns)
  for(s in 1:ns){
    PIs.Calibration[s] = mean(PIsd.Calibration[s,])
  }

  PId.Calibration = array(NaN,dim=nday)
  for(i in 1:nday){
    PId.Calibration[i] = mean(PIsd.Calibration[,i])
  }

  PI.Calibration = mean(PIs.Calibration)
  
  

  return(list( DI = PIsd.Calibration,
               MDI = PIs.Calibration,
               RMDI = PI.Calibration))


}
