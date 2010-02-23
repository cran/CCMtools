######################################################################

kept.data2D.from.cluster = function(cl,c,Nc,Data){
  # dim(Data)[1] = individuals
  # dim(Data)[2] = variables

 
  totCL = array(0,dim=Nc)
  for(i in 1:(length(cl))){
    totCL[cl[i]] = totCL[cl[i]]+1
  }
  totCL

  Data2 = array(NaN,dim=c((totCL[c]),(dim(Data)[2])))

  j=0
  for(i in 1:(dim(Data)[1])){
    if(cl[i] == c){
      j=j+1
      Data2[j,] = Data[i,]
    }
  }

  if(j!=(totCL[c])){
    cat(" !!!!!!!!!!!!!! j=",j,"  totCL[c]=",(totCL[c]),"!!!!!!!!!!!!!!\n")
  }

  return(Data2)

}

