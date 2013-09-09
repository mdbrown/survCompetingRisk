comprisk.PPV.CI <-
function( times, status1, status2, x, Z=NULL, predict.time, type, smooth = FALSE, fixed.values = 1:20/20, fixed = "v",  alpha=.05, bootstraps=500, sigma=NULL){


   if(length(Z)==0){ Z = rep(0, length(times))}
   if(smooth & length(Z) !=0){
      warning( "Defining Z while using smoothing is not functional at this time, ignoring Z...")
      Z = rep(0, length(times))
      }
   x0    <- cbind(x,Z)
   theX0 <- as.matrix(x0)
   n = length(times)
  # cif.i1 = Est.CIF(times, status1, status2, x0, theX0, predict.time)
   z.unique <- sort(unique(Z))
   if(length(sigma)==0) sigma  <- sd(x)/((n)^(1/3))
    
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
      cif.i2 = Est.CIF(times, status2, status1, x0, theX0, predict.time)   
      rocdata <- cbind(x, Z,cif.i1, cif.i2)
   }

   if(fixed =="v"){

   out.name1 <- "NPV"
   out.name2 <- "PPV"

  } else if(fixed == "PPV") {
   out.name1 <- "NPV"
   out.name2 <- "v"


  } else {
   out.name1 <- "PPV"
   out.name2 <- "v"

  }
    

 if(type ==1){

      Z.temp   <- rep(z.unique, length(fixed.values))
      out1.boot <- matrix(0, nrow = length(z.unique)*length(fixed.values), ncol=bootstraps)
      out2.boot <- matrix(0, nrow = length(z.unique)*length(fixed.values), ncol=bootstraps)

      for( tmpboot in 1:bootstraps){
            
        boot.index <- sample(1:nrow(rocdata), replace = TRUE)
        temp <- EstROC.all.cr.type1.FUN(rocdata[boot.index,]) 

        out1.temp <- matrix(0, nrow = length(z.unique), ncol = length(fixed.values))      
        out2.temp <- matrix(0, nrow = length(z.unique), ncol = length(fixed.values))      
       
        for (k in 1:length(fixed.values)) {

           temp.vals <- EstPPV.v.cr.FUN(temp$ALL,temp$nz.unique, fixed.values[k], fixed)[,4:5] 

           out1.temp[,k] <- temp.vals[,1]
           out2.temp[,k] <- temp.vals[,2]       
         }
         
         out1.boot[,tmpboot] <- as.vector(out1.temp)
         out2.boot[,tmpboot] <- as.vector(out2.temp)
      } 
           
      out1.CI <- apply(out1.boot, 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm=TRUE)
      out2.CI <- apply(out2.boot, 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm=TRUE)
            
      out <- data.frame(Z.temp, rep(fixed.values, rep(length(z.unique), length(fixed.values))), t(out1.CI), t(out2.CI)) 
      names(out) <- c("Z", paste("fixed_", fixed, sep=""), paste(out.name1, c(alpha/2, 1-alpha/2), sep="_"), paste(out.name2, c(alpha/2, 1-alpha/2), sep="_"))
      
   } else if(type==2){

      Z.temp   <- rep(z.unique, length(fixed.values))

      out1.boot.status1 <- matrix(0, nrow = length(z.unique)*length(fixed.values), ncol=bootstraps)
      out2.boot.status1 <- matrix(0, nrow = length(z.unique)*length(fixed.values), ncol=bootstraps)

      out1.boot.status2 <- matrix(0, nrow = length(z.unique)*length(fixed.values), ncol=bootstraps)
      out2.boot.status2 <- matrix(0, nrow = length(z.unique)*length(fixed.values), ncol=bootstraps)

      for( tmpboot in 1:bootstraps){
            
         boot.index <- sample(1:nrow(rocdata), replace = TRUE)
         temp <- EstROC.all.cr.type2.FUN(rocdata[boot.index,]) 

         out1.temp.status1 <- matrix(0, nrow = length(z.unique), ncol = length(fixed.values))  
         out2.temp.status1 <- matrix(0, nrow = length(z.unique), ncol = length(fixed.values))  

         out1.temp.status2 <- matrix(0, nrow = length(z.unique), ncol = length(fixed.values))      
         out2.temp.status2 <- matrix(0, nrow = length(z.unique), ncol = length(fixed.values))      
       
         for (k in 1:length(fixed.values)) {
         
           temp.vals.status1 <- EstPPV.v.cr.FUN(temp$ALL1, temp$nz.unique, fixed.values[k], fixed)[,4:5]
           
	         out1.temp.status1[,k] <- temp.vals.status1[,1]      
           out2.temp.status1[,k] <- temp.vals.status1[,2] 

           temp.vals.status2 <- EstPPV.v.cr.FUN(temp$ALL2, temp$nz.unique, fixed.values[k], fixed)[,4:5]
           out1.temp.status2[,k] <- temp.vals.status2[,1]      
           out2.temp.status2[,k] <- temp.vals.status2[,2] 

         }
         
         out1.boot.status1[,tmpboot] <- as.vector(out1.temp.status1)
         out2.boot.status1[,tmpboot] <- as.vector(out2.temp.status1)

         out1.boot.status2[,tmpboot] <- as.vector(out1.temp.status2)
         out2.boot.status2[,tmpboot] <- as.vector(out2.temp.status2)    
      } 
      browser()     
      out1.CI.status1 <- apply(out1.boot.status1, 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm=TRUE)
      out2.CI.status1 <- apply(out2.boot.status1, 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm=TRUE)

      out1.CI.status2 <- apply(out1.boot.status2, 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm=TRUE)
      out2.CI.status2 <- apply(out2.boot.status2, 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm=TRUE)
      
      out <- data.frame(Z.temp, rep(fixed.values, rep(length(z.unique), length(fixed.values))), t(out1.CI.status1), t(out1.CI.status2),
                                                                                                t(out2.CI.status1), t(out2.CI.status2)) 
      names(out) <- c("Z", paste("fixed", fixed, sep="_"), paste("event1", out.name1, c(alpha/2, 1-alpha/2), sep="_"),
                                                           paste("event2", out.name1, c(alpha/2, 1-alpha/2), sep="_"), 
                                                           paste("event1", out.name2, c(alpha/2, 1-alpha/2), sep="_"),
                                                           paste("event2", out.name2, c(alpha/2, 1-alpha/2), sep="_"))

      

      }

   out

}
