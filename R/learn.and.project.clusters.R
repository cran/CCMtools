###################################################################################

learn.and.project.clusters = function(DataCalibration, DataToBeProjected, cl.calibration, allocmet, DataS.Calibration=NaN){

  if(allocmet == "CART.A.and.w" || allocmet == "CART.w" || allocmet == "Euclid.dist.w1" || allocmet == "Euclid.dist.w2" || allocmet == "Gaussian.w" || allocmet == "MMw"){
    if(is.na(DataS.Calibration)){
      stop("DataS.Calibration must be different from NaN in learn.and.project function\n")
    }
  }
  else{
    if(is.na(DataS.Calibration)!=TRUE){
      cat("DataS.Calibration is not NaN in learn.and.project function but the allocation method (",allocmet,") does not need it: DataS.calibration will not be used.\n")
    }
  }

  Nc = max(cl.calibration)

############

  if(allocmet=="CART.A.and.w"){
    library(tree)
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters.CART.A.and.w(cl.calibration, DataCalibration, DataToBeProjected, DataS.Calibration)
  }
  if(allocmet=="CART.A"){
    library(tree)
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters.CART.A(cl.calibration, DataCalibration, DataToBeProjected)
  }
  if(allocmet=="CART.w"){
    library(tree)
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters.CART.w(cl.calibration, DataCalibration, DataToBeProjected, DataS.Calibration)
  }
  if(allocmet=="Euclid.dist.A"){
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters(cl.calibration, DataCalibration, DataToBeProjected)
  }
  if(allocmet=="Euclid.dist.w1"){
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters.wcv(cl.calibration, DataCalibration, DataToBeProjected, DataS.Calibration)
  }
  if(allocmet=="Euclid.dist.w2"){
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters.wcv.2(cl.calibration, DataCalibration, DataToBeProjected, DataS.Calibration)
  }
  if(allocmet=="knnA"){
    library(class)
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = as.numeric(knn(DataCalibration,DataToBeProjected,as.factor(cl.calibration),k=1,prob=TRUE))
  }
  if(allocmet=="Gaussian.A"){
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters.Gaussian.A(cl.calibration, DataCalibration, DataToBeProjected)
  }
  if(allocmet=="Gaussian.w"){
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters.Gaussian.w(cl.calibration, DataCalibration, DataToBeProjected,DataS.Calibration)
  }


  if(allocmet=="knnA10"){
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = as.numeric(knn(DataCalibration,DataToBeProjected,as.factor(cl.calibration),k=10,prob=TRUE))
  }

  if(allocmet=="MM"){
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters.MM(cl.calibration, DataCalibration, DataToBeProjected)
  }

  if(allocmet=="MMw"){
    library(mclust)
    cat("ALLOCATION DAYS TO PATTERNS THROUGH",allocmet,"\n")
    new.cl = Associate.Clusters.MMw(cl.calibration, DataCalibration, DataToBeProjected, DataS.Calibration)
  }



  totNewCL = array(0,dim=Nc)
  for(i in 1:(length(new.cl))){
    totNewCL[new.cl[i]] = totNewCL[new.cl[i]]+1
  }

  cat("For",allocmet," attribution method:\n")
  cat("totNewCL =",totNewCL,"\n")
  cat("% totNewCL =",round(totNewCL*100/sum(totNewCL),1),"\n")


  return(list(cl=new.cl, tot=totNewCL))

}

########################################################################
