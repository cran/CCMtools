CCM = function(
         Nc, NS, DataA.tbc,DataS.tbc,NN, DataStation, init="block",
         ITmax=15, rq=0
      ){

  ICq.best = array(NaN,dim=NS)
  ICq.bestIC.it = array(-1000,dim=NS)

  for(it in 1:ITmax){

    cat("Nc =",Nc,",  it =",it,"\n")
  
    ################################ Fern
    FC = CCM_1_internal(Nc, DataA.tbc, DataS.tbc, NN, DataStation, init)
    cl = FC$cl
    clIC = FC$clIC
    ################################
  
    totCLIC = array(0,dim=Nc)
    for(i in 1:(length(clIC))){
      totCLIC[clIC[i]] = totCLIC[clIC[i]]+1
    }
    ##cat("totCLIC=",totCLIC,"\n")



    ICq.best = Info.Criterion(NS, DataStation, rq, totCLIC, Nc, clIC)


    ICqbestIC.m = median(ICq.best)

    if(median(ICqbestIC.best.it) < ICqbestIC.m){
      ICqbestIC.best.it = ICq.best
      clIC.best.it.ICq = clIC
    }


  } # end for it

  return(list(cl = clIC.best.it.ICq, IC = ICqbestIC.best.it))

}