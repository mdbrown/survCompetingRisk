               
EstROC.all.cr.type1.FUN <- function(data) {

   Z=data[,2]; z=sort(unique(Z)); nzz=length(z)
   AUC.z = NULL; TPR.cz=FPR.cz=PPV.cz=NPV.cz=cutoff=NULL; nz=NULL; nz.unique = NULL
                  
   for (i in 1:nzz) {
                          
      data.z = data[data[,2]==z[i],]; nrow = dim(data.z)[1] # take only subset of z
      cutoff.z = data.z[,1]
      Thec = sort(unique(cutoff.z))  
      ncz = length(Thec)
      TPR.cz.temp = FPR.cz.temp=PPV.cz.temp=NPV.cz.temp =Ft0.Syc.z = St0.Syc.z=rep(0,ncz)
                          
      for (i in c(1:ncz)) {
         ge.c = cutoff.z>=Thec[i]
         Ft0.Syc.z[i] = sum(data.z[ge.c,3])  # P(T<t0,Y>=c, Z=z) at each values of Y and fixed z
         St0.Syc.z[i] = sum(1-data.z[ge.c,3]) # P(T>t0,Y>=c,Z=z)
                                  
         ngec = sum(ge.c)
         PPV.cz.temp[i] = Ft0.Syc.z[i]/ngec
         St0.Fyc.z = sum(1-data.z[!ge.c,3])
         NPV.cz.temp[i] = St0.Fyc.z/(sum(!ge.c))                            
      }
                    
      nz = c(nz, nrow)
      Ft0.z = max(Ft0.Syc.z)     # P(T<t0)
      St0.z = max(St0.Syc.z)     # P(T>t0)
      TPR.cz.temp = Ft0.Syc.z/Ft0.z        
      FPR.cz.temp = St0.Syc.z/St0.z
      AUC.z = c(AUC.z,sum(TPR.cz.temp*(FPR.cz.temp-c(FPR.cz.temp[-1],0))))
      TPR.cz = c(TPR.cz,TPR.cz.temp)
      FPR.cz = c(FPR.cz,FPR.cz.temp)
      PPV.cz = c(PPV.cz,PPV.cz.temp)
      NPV.cz = c(NPV.cz,NPV.cz.temp)
      cutoff = c(cutoff,Thec)
      nz.unique = c(nz.unique, length(Thec))
    
   }

   NPV.cz[is.na(NPV.cz)] <- 0
   list("AUC"=AUC.z, 
        'ALL'=data.frame("cutoff"=cutoff,"FPR"=FPR.cz,"TPR"=TPR.cz,"NPV"=NPV.cz,"PPV"=PPV.cz),
        'nz'=nz, 
        'nz.unique' = nz.unique )
}
