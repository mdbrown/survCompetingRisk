SIM.crdata.FUN <-
function(n, beta1 = c(log(5), log(1.5)), beta2 = c(0, 0)  ) {

   ## generate 2 types of events

   x0 <- matrix(0, n, 2)

   x0[,2] <- rbinom(n, 1, 0.5)

   alpha0 <- 0.5
   alpha1 <- 0
   
   x0[,1] <- rnorm(n,alpha0*x0[,2], exp(alpha1*x0[,2]))
   x0[,1] <- ifelse(x0[,1]>3,    3, x0[,1])
   x0[,1] <- ifelse(x0[,1]< -3, -3, x0[,1])

      
   ## generate failure time assuming Cox model
   mu.i <- x0%*%beta1

   stime01 <- -mu.i+log(-log(runif(n)))
   stime01 <- 10*exp(stime01)

   mu.i    <- x0%*%beta2
   stime02 <- -mu.i+log(-log(runif(n)))
   stime02 <- 10*exp(stime02)

   ## add censoring
   cuttime <- 25
   ctime0  <- runif(n, 0, cuttime)

   junk <- cbind(stime01, stime02, ctime0)

   times <- apply(junk, 1, min)  ## observed time

   status  <- matrix(apply(junk, 1, rank), n, 3, byrow=T)
   status1 <- ifelse(status[,1]==1, 1, 0)  ## 1 = observed event is type 1; otherwise =0
   status2 <- ifelse(status[,2]==1, 1, 0)  ## 1 = observed event is type 2; otherwise =0
   status3 <- ifelse(status[,3]==1, 1, 0)  ## censoring indicator
      
   data <- cbind(times, status1, status2, status3, x0) 
   as.data.frame(data)
}
