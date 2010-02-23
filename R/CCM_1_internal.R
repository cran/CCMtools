####################################################################

CCM_1_internal = function(Nc, NS, DataA.tbc,DataS.tbc,NN, DataStation, init="block"){

  if(init=="block"){
    ######
    cat("block init\n")
    N = round(NN/Nc)
    cl = array(NaN,dim=NN)
    #cat(dim(cl),"\n")
    for(c in 1:(Nc-1)){
      cl[(((c-1)*N)+1):(c*N)] = c
    }
    c = Nc
    cl[(((c-1)*N)+1):NN] = c
  }
  else{
    if(init=="12345"){
      ######
      cat("12345 init\n")
      cl = array(NaN,dim=NN)
      j=1
      for(i in 1:(dim(DataA.tbc)[1])){
        cl[i] = j
        if(j==Nc){
          j=1
        }
        else{
          j=j+1
        }
      }
    } # end if(init)
    else{
      if(init=="Kmeans"){
        cat("Kmeans init\n")
        CCi = cancor.Vrac(DataS.tbc,DataA.tbc)
        vi = CCi$v
        wi = CCi$w
        clk = kmeans(wi, Nc, nstart = 25)
        #clk = kmeans(DataS.tbc, Nc, nstart = 25)
        cl = clk$cluster
      }
      else{
        if(init=="Mixtn"){
          cat("Mixtn init\n")
          cl = array(NaN,dim=NN)
          j=1
          bl = 12 # block length
          Nb = round( (dim(DataA.tbc)[1]) / bl )
          for(i in 1:Nb){
            if((i*bl)<=NN){
              cl[((i-1)*bl+1):(i*bl)] = j
              if(j==Nc){
                j=1
              }
              else{
                j=j+1
              }
            }
          }
          if((i*bl)>NN){
            cl[((i-1)*bl+1):NN] = j
          }
          ind = length(cl)
          while(ind>0 && is.na(cl[ind])){
            cl[ind] = j
            ind = ind-1
          }
        }
        else{
          if(init=="EMw"){
            cat("EMw init\n")
            CCi = cancor.Vrac(DataS.tbc,DataA.tbc)
            M = min(dim(DataS.tbc)[2],dim(DataA.tbc)[2])
            wi = (CCi$w)[,1:M]
            #library(mclust)
            MC = Mclust(wi,Nc:Nc)
            cl = MC$classification
          }

          else{
            if(init=="EMvw"){
              cat("EMvw init\n")
              CCi = cancor.Vrac(DataS.tbc,DataA.tbc)
              M = min(dim(DataS.tbc)[2],dim(DataA.tbc)[2])
              wi = (CCi$w)[,1:M]
              vi = (CCi$v)[,1:M]
              dd = dim(wi)[2]
              vwi = array(NaN,dim=c(NN,(2*dd)))
              vwi[,1:dd] = vi[,1:dd]
              vwi[,(dd+1):(2*dd)] = wi[,1:dd]
              MC = Mclust(vwi,Nc:Nc)
              cl = MC$classification
            }
          } 
        }
      }
    }

  } # end else{}

  totCL = array(0,dim=Nc)
  for(i in 1:(length(cl))){
    totCL[cl[i]] = totCL[cl[i]]+1
  }
  cat(totCL,"\n")

  cat("IC init =",(median(Info.Criterion(NS, DataStation, 0, totCL, Nc, cl))),"\n")


  ### IN ONE LOOP.

  ######### begin loop


  I = dim(DataS.tbc)[2]
  J = dim(DataA.tbc)[2]
  CV = min(I,J)

  IC = 0
  clIC = NaN ; ICclIC = NaN ; aIC = NaN ; bIC = NaN ; mSIC = NaN ; mAIC = NaN
  A.IC = NaN ; B.IC = NaN ; rIC = NaN
  vc.IC = NaN ; wc.IC = NaN

  it = 0
  id = FALSE # id == TRUE means that the previous clusters were identical
 
  while(id == FALSE && it<100){
  
    it = it + 1
    cat("\niteration",it,"\n")
 
    CV2 = array(NaN,dim=Nc)

    mS2 = array(NaN,dim=c(Nc,I))
    mA2 = array(NaN,dim=c(Nc,J))

    vr = array(NaN,dim=c( (dim(DataA.tbc)[1]), Nc, CV))
    wr = array(NaN,dim=c( (dim(DataA.tbc)[1]), Nc, CV))
    hatv = array(NaN,dim=c( (dim(DataA.tbc)[1]), Nc, CV))
    #r2 = array(NaN,dim=c(Nc,CV))
    r2 = array(0,dim=c(Nc,CV))
  
    #a=array(NaN,dim=c(Nc,CV))
    #b=array(NaN,dim=c(Nc,CV))
    a=array(0,dim=c(Nc,CV))
    b=array(0,dim=c(Nc,CV))

    A =   array( NaN, dim = c(Nc, (min(I,J)), I) )
    #B =   array( NaN, dim = c(Nc, (min(I,J)), J) )
    B =   array( NaN, dim = c(Nc, J, J) )

    vc = array(NaN,dim=c( Nc, (dim(DataA.tbc)[1]), CV))
    wc = array(NaN,dim=c( Nc, (dim(DataA.tbc)[1]), CV))



    for(c in 1:Nc){
  
      DataS2 = kept.data2D.from.cluster(cl,c,Nc,DataS.tbc)
      DataA2 = kept.data2D.from.cluster(cl,c,Nc,DataA.tbc)
      CC2 = try(cancor.Vrac(DataS2,DataA2))

   ###### NEW when CC2 has bugged
      if(length(CC2)==1){ # 
        cat("!!!!!!!!!!!!!!! length(CC2) = 1 !!!!!!!!!!!!!!!\n")
        bb = 60 #min(60,min(totCL))
        DataS3 = array(NaN,dim=c(Nc*bb,dim(DataS2)[2]))
        DataA3 = array(NaN,dim=c(Nc*bb,dim(DataA2)[2]))
        cat("bb =",bb,"\n")

        ### random new data
        for(ii in 1:Nc){
          DataStmp = kept.data2D.from.cluster(cl,ii,Nc,DataS.tbc)
          pk = Pick.k.random(bb,DataStmp)
          cat("pk",ii," =",pk,"\n")
          DataS3[((ii-1)*bb+1):(ii*bb),] = (kept.data2D.from.cluster(cl,ii,Nc,DataS.tbc))[pk,]
          DataA3[((ii-1)*bb+1):(ii*bb),] = (kept.data2D.from.cluster(cl,ii,Nc,DataA.tbc))[pk,]
        }

        DataS2 = DataS3
        DataA2 = DataA3

        CC2 = cancor.Vrac(DataS2,DataA2)
      }
    ###### 

      vc[c,1:(dim(CC2$v)[1]),1:CV] = (CC2$v)[,1:CV]
      wc[c,1:(dim(CC2$w)[1]),1:CV] = (CC2$w)[,1:CV]
      v2 = CC2$v
      w2 = CC2$w
      mS2[c,] = CC2$mS
      mA2[c,] = CC2$mA
      CV2[c] = CC2$CV
      #r2[c,] = CC2$r
      r2[c,1:(length(CC2$r))] = CC2$r
      cat("r2[",c,",]=",(r2[c,]),"\n")

      A[c,,] = CC2$A
      B[c,,] = CC2$B

      for(cv in 1:CV){
        lmf = lm(v2[,cv] ~ w2[,cv])   #lmf = lm(Y ~ X) fits Y=aX+b
        a[c,cv]=lmf$coef[2]
        b[c,cv]=lmf$coef[1]
        #####
        if(is.na(a[c,cv])){
          a[c,cv]=0
          b[c,cv]=0
        }
        #####
      }

      D1p = array(NaN,dim=dim(DataS.tbc))
      for(i in 1:I){
        D1p[,i] = DataS.tbc[,i] - mS2[c,i]
      }
      D2p = array(NaN,dim=dim(DataA.tbc))
      for(j in 1:J){
        D2p[,j] = DataA.tbc[,j] - mA2[c,j]
      }

####### NEW
      A2 = t(A[c,,])    # because A=xcoef (NOT t(xcoef)) in cancor.Vrac
      for(i in 1:(dim(DataA.tbc)[1])){
        for(cv in 1:CV){
          vr[i,c,cv] = t(A2[cv,]) %*% D1p[i,]    # 15/12/2008

          wr[i,c,cv] = t(B[c,cv,]) %*% D2p[i,] #OK from B=t(ycoef) in cancor.Vrac

          hatv[i,c,cv] =  a[c,cv] * wr[i,c,cv] + b[c,cv]
        }
      }
#########



    } #end for(c in 1:Nc){}


   COEF = array(NaN,dim=c(Nc,CV))
   COEF2 = array(NaN,dim=c(Nc,CV))
   DIFF = array(NaN,dim=c((dim(DataA.tbc)[1]),Nc,CV))

    err = array(0,dim=c( (dim(DataA.tbc)[1]), Nc))
    for(i in 1:(dim(DataA.tbc)[1])){
      for(c in 1:Nc){

        for(cv in 1:(CV2[c])){

          COEF[c,cv] = (r2[c,cv]/r2[c,1])
          COEF2[c,cv] = (r2[c,cv]/sum(r2[c,]))
          DIFF[i,c,cv] = (vr[i,c,cv] - hatv[i,c,cv])

          err[i,c] = err[i,c] + ( (r2[c,cv]/r2[c,1]) * (vr[i,c,cv] - hatv[i,c,cv])^2 )

        }
      }
    }
  

    cl2=c()
    
    for(i in 1:(dim(DataA.tbc)[1])){
      O = order(err[i,])
      cl2 = c(cl2,O[1])   
    }


    totCL = array(0,dim=Nc)
    for(i in 1:(length(cl2))){
      totCL[cl2[i]] = totCL[cl2[i]]+1
    }
    cat(totCL,"\n")

################ 17/102008

    if(min(totCL)<=1){

      cat("\n !!!!!!!!!! min(totCL)<=1 => stop CCM_1_base.R !!!!!!!!!!!!\n")

      if(it==1){

        cat("it==1 => Stock prev values\n")

        clprev=cl; ICclprev=median(Info.Criterion(NS, DataStation, 0, totCL, Nc, cl))
        errprev=err; aprev=a; bprev=b; Aprev=A; Bprev=B; rprev=r2; mSprev=mS2; mAprev=mA2
        vcprev=vc; wcprev=wc

        clIC=cl; ICclIC=ICclprev; aIC=a; bIC=b; A.IC=A; B.IC=B; rIC=r2; mSIC=mS2; mAIC=mA2
        vc.IC=vc; wc.IC=wc
      }

      cl=clprev; ICcl=ICclprev; err=errprev; a=aprev; b=bprev; A=Aprev; B=Bprev; r=rprev; mS=mSprev; mA=mAprev; vc=vcprev; wc=wcprev

      id = TRUE # => stop

    }
    else{

################


    ICcl = median(Info.Criterion(NS, DataStation, 0, totCL, Nc, cl2))
    cat("IC =",ICcl,"\n")
    if(ICcl>IC){
      clIC = cl2 # clusters corresponding to the best IC
      IC = ICcl  # best IC value 
      aIC = a    # a and b are the regression coef between v and w (b=0)
      bIC = b
      mSIC=mS2   # mean of the station data per cluster for best IC
      mAIC=mA2   # mean of the atmospheric data per cluster for best IC
      A.IC = A   # 3d matrices containing the matrices A and B per cluster
      B.IC = B
      rIC = r2
      vc.IC = vc
      wc.IC = wc

      cat("New best IC\n")
    }


    id = compare2clusterings(cl,cl2)
    # TRUE  => cl == cl2
    # FLASE => cl != cl2

  
################ 17/10/2008

    clprev=cl; ICclprev=ICcl; errprev=err; aprev=a; bprev=b; Aprev=A; Bprev=B; rprev=r2; mSprev=mS2; mAprev=mA2; vcprev=vc; wcprev=wc

################


    cl = cl2

    cat("CV2 =",CV2,"\n")


################ 17/102008

  } # end else{}

################

  } # end while(id==FALSE){}

  return(list(cl=cl, ICcl=ICcl, err=err,
              a=a, b=b, Ac=A, Bc=B, r=r2, mS=mS2, mA=mA2, vc=vc, wc=wc,
              clIC=clIC, ICclIC=IC, rIC=rIC, vc.IC=vc.IC, wc.IC=wc.IC,
              aIC=aIC, bIC=bIC, A.IC=A.IC, B.IC=B.IC, mSIC=mSIC, mAIC=mAIC
          ) )



} # end function(){}



###############################################################################
