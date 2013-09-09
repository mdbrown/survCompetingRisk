EstROC.v.cr.FUN <-
function(accuracy.out, nz, uu0, type){ 

   nz <- c(0,cumsum(nz))
    
   summary <- NULL
   #browser()
   for (i in 1:(length(nz)-1)) {
      #browser()
      junk <- accuracy.out[(nz[i]+1):(nz[i+1]),-1]
      ind0 <- match(type, c("FPR","TPR","NPV","PPV")) 
      uuk  <- junk[,ind0]
      junk <- junk[order(uuk),-ind0]
      uuk  <- sort(uuk)

      if(ind0 == 1){ tmpind <- sum.I(uu0, ">=", uuk) } else { tmpind <- sum.I(uu0, ">", uuk) }
      
      summary <- rbind(summary, junk[tmpind,])
   }

   summary
}
