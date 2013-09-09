
Est.CIF.Smooth.discrete.noz<-function(times,status1,status2,x0,theX0,predict.time, sigma) { 

## status1 indicating the event of interest ## x0: is the covariate matrix, include marker y as the first column and other covariate Z ## theX0 is a nrow by np matrix of covariate values that the CIF is evaluated at; it can be fixed covariates at multiple values or at a single value ## sort time from big to small

### make sure my smoothcoxph does not assume data are sorted
   n = length(times)
   nx.unique = length(theX0)
   
   junkS0yu.1 = CalculateDS0YU.discrete.noz(times,x0,theX0,sigma)
   lambda01u = junkS0yu.1$dSy.u*VTM(status1,nx.unique)    ## lambda01(u) at all failure times

   lambda02u = junkS0yu.1$dSy.u*VTM(status2,nx.unique)  ## lambda02(u) at all failure times
        
   St = t(apply(lambda01u[,n:1],1,cumsum))[,n:1]+t(apply(lambda02u[,n:1],1,cumsum))[,n:1]
   St  = exp(-St)                      ## sum of all possible times; n by n

   fu<-St*lambda01u*VTM(times<=predict.time,nx.unique)
   apply(fu,1,sum)

}

