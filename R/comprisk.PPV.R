comprisk.PPV <-
function( times, status1, status2, x, Z=NULL, predict.time, type, smooth = FALSE, sigma=FALSE){
   
   if(length(Z)==0){ Z = rep(0, length(times))}
   if(smooth & length(Z) !=0){
      warning( "Defining Z while using smoothing is not functional at this time, ignoring Z...")
      Z = rep(0, length(times))
      }   
   x0    <- cbind(x,Z)
   theX0 <- as.matrix(x0)
   n = length(times)
   if(length(sigma)==0) sigma  <- sd(x)/((n)^(1/3))
    
   #cif.i1 = Est.CIF(times, status1, status2, x0, theX0, predict.time)
   
   if(smooth){
     
      ooo <- order(-times)
      times   <- times[ooo]
      status1 <- status1[ooo]
      status2 <- status2[ooo]
      x       <- x[ooo]
      x0      <- x0[ooo,]
      theX0   <- sort(unique(x))
      
      cif.i1  <- Est.CIF.Smooth.discrete.noz(times, status1, status2, x, theX0, predict.time, sigma) 
      cif.i2  <- Est.CIF.Smooth.discrete.noz(times, status2, status1, x, theX0 ,predict.time, sigma) 
      rocdata <- cbind(theX0, rep(1, length(theX0)), cif.i1, cif.i2)
      rocdata <- rocdata[match(x, theX0),]      
   }else{
      cif.i1 <- Est.CIF(times, status1, status2, x0, theX0, predict.time)
      cif.i2 = Est.CIF(times, status2, status1, x0, theX0, predict.time)   
      rocdata <- cbind(x, Z,cif.i1, cif.i2)        
   }

   if(type ==1){

      temp <- EstROC.all.cr.type1.FUN(rocdata)
      nz <- temp$nz
      nz <- c(0,cumsum(nz))
    
      #calculate v
      v = NULL
      for (i in 1:(length(nz)-1)) {

      junk   <- temp$ALL[(nz[i]+1):(nz[i+1]),]
      njunk  <- dim(junk)[1]
      v <- c(v, rank(junk$cutoff)/njunk)

      }

      PPV <- data.frame("v" = v, "NPV" = temp$ALL$NPV,"PPV" = temp$ALL$PPV)

      if(length(temp$nz)>1){ PPV$Z = sort(Z)}
      

   } else if(type==2){

      temp <- EstROC.all.cr.type2.FUN(rocdata)
      nz <- temp$nz
      nz <- c(0,cumsum(nz))
    
      #calculate v
      v = NULL

      for (i in 1:(length(nz)-1)) {

      junk   <- temp$ALL1[(nz[i]+1):(nz[i+1]),]
      njunk  <- dim(junk)[1]
      v <- c(v, rank(junk$cutoff)/njunk)

      }

      PPV <- data.frame("v" = v, "NPV.event1" = temp$ALL1$NPV,"PPV.event1" = temp$ALL1$PPV, 
                                 "NPV.event2" = temp$ALL2$NPV,"PPV.event2" = temp$ALL2$PPV )

      if(length(temp$nz)>1){ PPV$Z = sort(Z)}
      
      
      }
   
  PPV
}
