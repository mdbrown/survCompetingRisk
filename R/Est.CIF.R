Est.CIF <-
function(times, status1, status2, x0, theX0 = NA, predict.time) {

   ## status1 indicating the event of interest
   ## x0: is the covariate matrix
   ## theX0 is a nrow by np matrix of covariate values that the CIF is evaluated at; it can be fixed covariates at multiple values or at a single value  
   ## sort time from big to small
   #browser()
   n    <- length(times)

   if(length(theX0)==1) theX0 <- as.matrix(x0)

   nrow <- dim(theX0)[1]
   
   #order the data in terms of times	
   ooo <- order(-times)

   times   <- times[ooo]
   status1 <- status1[ooo]
   status2 <- status2[ooo]
   x0      <- x0[ooo,] ####this assumes x0 has more than one column!
   if(length(unique(x0[,2]))==1){ 
   x0 <- x0[,1]
   theX0 <- as.matrix(theX0[,1])
   }
   #fit the cox reg models 
   beta1.hat <- coxph(Surv(times,status1)~x0)$coef
   lambda01u <- 1/cumsum(exp(beta1.hat%*%t(x0)))*(status1==1) ## lambda01(u) at all failure times 

   beta2.hat <- coxph(Surv(times,status2)~x0)$coef
   lambda02u <- 1/cumsum(exp(beta2.hat%*%t(x0)))*(status2==1)  ## lambda02(u) at all failure times
 
   if (nrow>1) {
   
      St1 <- exp(-( VTM(cumsum(lambda01u[n:1])[n:1], nrow)*c(exp(theX0%*%beta1.hat)) +
                    VTM(cumsum(lambda02u[n:1])[n:1], nrow)*c(exp(theX0%*%beta2.hat)) ))  ## n by n matrix 

      fu <- St1*VTM(lambda01u*(times <= predict.time), nrow)*c(exp(theX0%*%beta1.hat))

      apply(fu,1,sum)
        
   } else {
       
      St1<-exp(-( cumsum(lambda01u[n:1])[n:1]*exp(theX0%*%beta1.hat)+
                  cumsum(lambda02u[n:1])[n:1]*exp(theX0%*%beta2.hat) ))  ## n by n 

      fu<-St1*lambda01u*(times<=predict.time)*exp(theX0%*%beta1.hat)
      sum(fu)

   }
}
