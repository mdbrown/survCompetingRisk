comprisk.ROC <-
function( times, status1, status2, x, Z=NULL, predict.time, type, smooth = FALSE, sigma=NULL){

   if(length(Z)==0){ Z = rep(0, length(times))}
   if(smooth & length(Z) !=0){
      warning( "Defining Z while using smoothing is not functional at this time, ignoring Z...")
      Z = rep(0, length(times))
      }
   
   n = length(times)
   x0    <- cbind(x,Z)
   
   if(length(sigma)==0) sigma  <- sd(x)/((n)^(1/3))
       
   theX0 <- as.matrix(x0)
   #browser()
   if(smooth){
      
      ooo <- order(-times)
      times   <- times[ooo]
      status1 <- status1[ooo]
      status2 <- status2[ooo]
      x       <- x[ooo]
      x0      <- x0[ooo,]
      theX0   <- sort(unique(x))
      
      cif.i1 <- Est.CIF.Smooth.discrete.noz(times, status1, status2, x, theX0, predict.time, sigma) 
      cif.i2 = Est.CIF.Smooth.discrete.noz(times, status2, status1, x, theX0 ,predict.time, sigma) 
      rocdata <- cbind(theX0, rep(1, length(theX0)), cif.i1, cif.i2)
      rocdata <- rocdata[match(x, theX0),]       
   }else{
      cif.i1 <- Est.CIF(times, status1, status2, x0, theX0, predict.time)
      cif.i2 = Est.CIF(times, status2, status1, x0, theX0, predict.time)   
      rocdata <- cbind(x, Z,cif.i1, cif.i2)      
   }
        
   if(type ==1){

      temp <- EstROC.all.cr.type1.FUN(rocdata)
      
      ROC <- data.frame("TPR" = temp$ALL$TPR,"FPR" = temp$ALL$FPR)

      if(length(temp$nz)>1){ ROC$Z = sort(Z)}
      
      AUC <- data.frame(t(temp$AUC))
      names(AUC) = paste("Z_", unique(sort(Z)), sep="")
      
   } else if(type==2){
      
      temp <- EstROC.all.cr.type2.FUN(rocdata)
      
      ROC <- data.frame("TPR.event1" = temp$ALL1$TPR,"FPR.event1" = temp$ALL1$FPR, 
                        "TPR.event2" = temp$ALL2$TPR,"FPR.event2" = temp$ALL2$FPR )

      if(length(temp$nz)>1){ ROC$Z = sort(Z)}
      
      AUC <- data.frame(t(rbind(temp$AUC.1, temp$AUC.2)))
      row.names(AUC) = paste("Z_", unique(sort(Z)), sep="")
      names(AUC) = c("event1", "event2")

      }



  result = list("AUC" = AUC, "ROC" = ROC)
  class(result) = "comprisk.ROC"
  result

}
