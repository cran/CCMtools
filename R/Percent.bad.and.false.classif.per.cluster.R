Percent.bad.and.false.classif.per.cluster = function(cl, new.cl){

  nc = max(cl)
  bad.cl = rep(0,nc)
  bad.tot = 0
  false.cl = rep(0,nc)

  for(i in 1:(length(cl))){
    if(new.cl[i] != cl[i]){
      false.cl[new.cl[i]] = false.cl[new.cl[i]] + 1
      bad.cl[cl[i]] = bad.cl[cl[i]] + 1
      bad.tot = bad.tot+1
    }
  }

  for(c in 1:nc){
    false.cl[c] = false.cl[c]*100/(length(which(new.cl==c)))
    bad.cl[c] = bad.cl[c]*100/(length(which(cl==c)))
  }
  bad.tot = bad.tot*100/length(cl)

  mat.att = array(0, dim=c(nc,nc))
  for(i in 1:(length(cl))){
    mat.att[cl[i],new.cl[i]] = mat.att[cl[i],new.cl[i]] + 1
  }

  return(list(tot=bad.tot, BadPerCluster=bad.cl, FalsePerCluster=false.cl, mat.att=mat.att))

}
