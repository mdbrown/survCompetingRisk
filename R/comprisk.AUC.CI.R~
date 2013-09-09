comprisk.AUC.CI <-
function(times, status1, status2, x, Z=NULL, predict.time, type, smooth = FALSE, alpha = .05, bootstraps = 500, sigma=NULL){

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
   z.unique <- sort(unique(Z))
   
      if(smooth){
      
         ooo <- order(-times)
         times   <- times[ooo]
         status1 <- status1[ooo]
         status2 <- status2[ooo]
         x       <- x[ooo]
         x0      <- x0[ooo,]
         theX0   <- sort(unique(x))
      
         cif.i1 <- Est.CIF.Smooth.discrete.noz(times, status1, status2, x, theX0, predict.time, sigma) 
         cif.i2 <- Est.CIF.Smooth.discrete.noz(times, status2, status1, x, theX0 ,predict.time, sigma) 
         rocdata <- cbind(theX0, rep(1, length(theX0)), cif.i1, cif.i2)
         rocdata <- rocdata[match(x, theX0),]
      }else{
         cif.i1 <- Est.CIF(times, status1, status2, x0, theX0, predict.time)
         cif.i2 <- Est.CIF(times, status2, status1, x0, theX0, predict.time)   
         rocdata <- cbind(x, Z,cif.i1, cif.i2)
         }
      

     if(type ==1){
     
      out.boot <- matrix(0, nrow = length(z.unique), ncol=bootstraps)

      for( tmpboot in 1:bootstraps){

        boot.index <- sample(1:nrow(rocdata), replace = TRUE)
        temp <- EstROC.all.cr.type1.FUN(rocdata[boot.index,])

	      out.boot[,tmpboot] <- temp$AUC
      }

      out.CI <- apply(out.boot, 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm=TRUE)

      out <- data.frame(z.unique, t(out.CI))
      names(out) <- c("Z",  paste("AUC", c(alpha/2, 1-alpha/2), sep = "_"))

   } else if(type==2){   

      rocdata <- cbind(x, Z,cif.i1, cif.i2)
      temp <- EstROC.all.cr.type2.FUN(rocdata)

      Z.temp   <- rep(z.unique)
      
      out.boot.1 <- matrix(0, nrow = length(z.unique), ncol=bootstraps)
      out.boot.2 <- matrix(0, nrow = length(z.unique), ncol=bootstraps)
      for( tmpboot in 1:bootstraps){

         boot.index <- sample(1:nrow(rocdata), replace = TRUE)
        # browser()
         temp <- EstROC.all.cr.type2.FUN(rocdata[boot.index,])

         out.boot.1[,tmpboot] <- temp$AUC.1
         out.boot.2[,tmpboot] <- temp$AUC.2

      }

      out.CI.1 <- apply(out.boot.1, 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm=TRUE)
      out.CI.2 <- apply(out.boot.2, 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm=TRUE)

      out <- data.frame(Z.temp, t(out.CI.1), t(out.CI.2))
      names(out) <- c("Z", paste("event1_AUC", c(alpha/2, 1-alpha/2), sep="_"), paste("event2_AUC", c(alpha/2, 1-alpha/2), sep="_"))

      }
      out
}
