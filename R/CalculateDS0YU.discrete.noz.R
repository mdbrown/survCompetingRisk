                                                                                                 
CalculateDS0YU.discrete.noz<-function(times,xVector,xEvaluate,sigma){    

## return n by n matrix , each row is cumulative dni/pi given ith x at each u (over column)
## assume data are sorted by order(-times)

   weight.x = CalWtMatrix.discrete(xVector,xEvaluate,sigma)
   pi.x = apply(weight.x,1,cumsum)

   dSy.u= weight.x/t(pi.x)
   dSy.u = ifelse(is.na(dSy.u),0,dSy.u)
   list(dSy.u=dSy.u)
}
