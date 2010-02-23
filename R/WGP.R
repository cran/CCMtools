WGP = function(cl.check, cl.proj, DataS.check){

# Corresponds to WGP(s)

  if((length(cl.check))!=(length(cl.proj))){
    stop("length(cl.check)!=length(cl.proj) in Global.proba.cost\n")
  }

  if((length(cl.check))!= (dim(DataS.check)[1])){
    stop("length(cl.check)!=dim(DataS.check)[1] in Global.proba.cost\n")
  }

  if(max(cl.check) != max(cl.proj)){
    cat("max(cl.check)=",max(cl.check)," != max(cl.proj)=",max(cl.proj)," in Weighted.Global.Probability.Cost\n")
  }

  d = dim(DataS.check)
  nday = d[1]
  ns = d[2]
  nc = max(cl.check)


  WGPs = array(0,dim=ns)
  for(c in 1:nc){
    for(s in 1:ns){
      LCo = length( which( (DataS.check[(which(cl.check==c)),s])>0 ) )
      LCp = length( which( (DataS.check[(which(cl.proj==c)),s])>0 ) )
      WGPs[s] = WGPs[s] + abs(LCo - LCp)
    }
  }
  WGPs = WGPs/nday

  return(WGPs)

}
