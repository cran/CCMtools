###############################################################################
# Compute the Information Criterion (IC) given in Moron et al. (2008) and Santos et al. (2005)

Info.Criterion = function(NS, DataS, r, totCL, Nc, cl){

### IC = SOM_i=1^K |n_i,r - (p_r * n_i)|

### n_i,r = # of days in cluster i that receive a rainfall amount > r
### p_r = proba of such rainy days in the whole population
### n_i = # of days in cluster i


ni = totCL

pr = array(NaN,dim=NS)
for(s in 1:NS){
  Data.s = kept.data1D.greater.than.r(DataS[,s],r)
  pr[s] = (length(Data.s))/(dim(DataS)[1])
  #cat("pr[",s,"]=",(pr[s]),"\n")
}


nir = array(NaN,dim=c(Nc,NS))
IC = array(0,dim=NS)

for(i in 1:Nc){

  DataS2 = kept.data2D.from.cluster(cl,i,Nc,DataS)

  for(s in 1:NS){

    Data.rs = kept.data1D.greater.than.r(DataS2[,s],r)
    nir[i,s] = length(Data.rs)

    IC[s] = IC[s] + abs( (nir[i,s] - (pr[s] * ni[i])) )

    #cat(abs( (nir[i,s] - (pr[s] * ni[i])) ),"\n")

  }

}

IC

return(IC)

}

###############################################################################
