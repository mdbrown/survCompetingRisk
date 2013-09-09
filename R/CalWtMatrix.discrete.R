                                                                                                 
CalWtMatrix.discrete = function(xVector,xEvaluate,sigma){  

# return a n *n matix, the ith row is the weight evaluated at the xEvaluate[i] 

   n = length(xVector)
   nxevaluate = length(xEvaluate)
   tempX = (VTM(xVector,nxevaluate)-t(VTM(xEvaluate,n)))/sigma;
   
   1/sqrt(2*3.1415926) * exp(-tempX * tempX/2.0)/sigma; 
}
