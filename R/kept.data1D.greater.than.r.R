######################################################################

kept.data1D.greater.than.r = function(Data,r){

  L = length(Data)
  nn = 0
  for(i in 1:L){
    if(Data[i]>r){
      nn = nn + 1
    }
  }

  Data.r = array(NaN,dim=nn)
  nnn = 0
  for(i in 1:L){
    if(Data[i]>r){
      nnn = nnn + 1
      Data.r[nnn] = Data[i]
    }
  }

  return(Data.r)

}
