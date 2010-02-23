CWGLI = function(cl.check, cl.proj, DataS.check){


  if((length(cl.check))!=(length(cl.proj))){
    stop("length(cl.check)!=length(cl.proj) in Global.proba.cost\n")
  }

  if((length(cl.check))!= (dim(DataS.check)[1])){
    stop("length(cl.check)!=dim(DataS.check)[1] in Global.proba.cost\n")
  }

  if(max(cl.check) != max(cl.proj)){
    cat("max(cl.check)=",max(cl.check)," != max(cl.proj)=",max(cl.proj)," in Global.proba.cost\n")
  }

  d = dim(DataS.check)
  nday = d[1]
  ns = d[2]
  nc = max(cl.check)



  CWGLIs = array(0,dim=ns)

  for(c in 1:nc){

    for(s in 1:s){

      Dc = DataS.check[(which(cl.check==c)),s]
      Dp = DataS.check[(which(cl.proj==c)),s]
      Co = length(which(Dc>0))
      Cp = length(which(Dp>0))

      # From NON-normalized data
      CWGLIs[s] = CWGLIs[s] + ( (Co*(log(sum(Dc))-log(Co))) - (Cp*(log(sum(Dp))-log(Cp))) )

    } # end for s    

  }



  Z = array(0,dim=ns)
  for(s in 1:ns){
    Z[s] = length(which(DataS.check[,s]>0))
  }

  CWGLIs = CWGLIs/Z

  retunr(CWGLIs)

}

