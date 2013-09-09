\name{comprisk.ROC.CI}
\alias{comprisk.ROC.CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
calculate bootstrap CI's for ROC curves under competing risk models
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~ 
This function calculates bootstrap confidence intervals for either the true positive rate (TPR) or the false positive rate (FPR) for time to event data when there are potentially two causes of failure (competing risks). The user is able to fix either the TPR or the FPR at specific values. Confidence bands for the non-fixed variable are then derived from the empirical bootstrap distribution at the values given by "fixed.values". 

Two different ways of defining cases for these data can be accounted for in the analysis (see 'type' variable below).   
}
\usage{
comprisk.ROC.CI(times, status1, status2, x, Z = NULL, predict.time, type, smooth=FALSE, fixed.values = 1:20/20, fixed = "FPR", alpha = 0.05, bootstraps = 500, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{times}{
%%     ~~Describe \code{times} here~~
  vector of min(time to event, censoring time) for each individual.
}
  \item{status1}{
%%     ~~Describe \code{status1} here~~
  vector of indicators specifying that event 1 was the cause of failure (should equal 1 for individuals where event 1 occured)
}
  \item{status2}{
%%     ~~Describe \code{status2} here~~
  vector of indicators specifying that event 2 was the cause of failure (should equal 1 for individuals where event 2 occured)
}
  \item{x}{
%%     ~~Describe \code{x} here~~
  vector of marker values
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
  vector of (discrete) covariate values
}
  \item{predict.time}{
%%     ~~Describe \code{predict.time} here~~
  The prediction time to use to calculate the TPR and FPR. Should be a single numeric value. 
}
   \item{type}{
%%     ~~Describe \code{type} here~~
  Can take on two values. 
  
  Set type = 1 if case is defined by the event of interest, and controls are all the rest. 
  Set type = 2 if case is defined by stratifying on event type, and controls are those who have not experienced any events.  
    
  See 'references' for more information on type. 
 
}
  \item{smooth}{  TRUE/FALSE (default). When set to TRUE non-parametric smoothing is used instead of Cox proportional hazards model. Note, when smooth = TRUE, Z is ignored. 
  
}
  \item{fixed.values}{
%%     ~~Describe \code{fixed.values} here~~
  vector of values to obtain confidence bounds. Default is fixed.values = 1:20/20. 
  
}
  \item{fixed}{
%%     ~~Describe \code{fixed} here~~
  Which quantity to fix. Must be one of "FPR" or "TPR". Default is fixed = "FPR", so that confidence bands for TPR, at fixed values of FPR, are output.   
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
  (1-alpha)\% CI's are returned. Default is alpha = .05 (95\% CI's given). 
  
}
  \item{bootstraps}{
  number of bootstraps to use to obtain CI's. Default is set to 500.  
}

\item{sigma}{ Tuning parameter to be set when smooth = TRUE. The default value of sigma = sd(x)/length(x)^1/3}
}

\value{
 A dataframe with the following values: Z, fixed values, lower bound CI, upper bound CI. 
 If type = 1, only one set of confidence intervals are given.
 If type = 2, we stratify on event type, and so confidence intervals are given for both event 1 and for event 2.   

}
\references{

Zheng Y, Cai T, Jin Y, Feng Z. Evaluating prognostic accuracy of biomarkers under
competing risk. Biometrics. 2012 Jun;68(2):388-96.


Zheng Y, Cai T, Feng Z, and Stanford J. Semiparametric Models of Time-dependent
Predictive Values of Prognostic Biomarkers. Biometrics. 2010, 66: 50-60.


}


\seealso{
\code{\link{comprisk.ROC}}
}
\examples{

data(crdata)

## comprisk.ROC.CI
#type I
myROC.type1.CI <- comprisk.ROC.CI( times = crdata$times, 
                                status1 = crdata$status1, 
                                status2 = crdata$status2, 
                                x = crdata$x, 
                                Z = crdata$Z, 
                                predict.time = 10, 
                                type = 1, 
                                fixed.values = c(.05, .45, .6), 
                                fixed = "FPR", ## one could also set this as TPR
                                alpha = .05, 
                                bootstraps = 100) 

myROC.type1.CI


## Now use comprisk.ROC to obtain the ROC curve, so we can plot the confidence bands
myROC.type1 <- comprisk.ROC( times = crdata$times,
                             status1 = crdata$status1, 
                             status2 = crdata$status2, 
                             x = crdata$x, 
                             Z = crdata$Z, 
                             predict.time = 10, 
                             type = 1)
                                                   
tmp <- myROC.type1$ROC

#plot the ROC curve
plot(tmp$FPR[tmp$Z==0], tmp$TPR[tmp$Z==0], type="l",lwd=2, main="Type 1", xlab="FPR", ylab="TPR")
lines(tmp$FPR[tmp$Z==1], tmp$TPR[tmp$Z==1], lty=2, lwd=2)
legend(x="bottomright", c("Z = 0", "Z = 1"), lty=c(1,2))
lines(c(0, 1), c(0,1), col="lightgrey")

#add CI's to plot
tmp.CI <- myROC.type1.CI

for( i in 1:nrow(myROC.type1.CI)){
#offset the confidence intervals by .005 for Z=0 and Z=1, so we can tell the CI's apart on the plot
  lines(x=rep(tmp.CI$fixed_FPR[i],2) + ifelse(tmp.CI$Z[i]==0, -0.005, .005), y=c(tmp.CI$TPR_0.025[i], tmp.CI$TPR_0.975[i]), lty=ifelse(tmp.CI$Z[i]==0, 1, 2), lwd=1.5)
  
  points(x=rep(tmp.CI$fixed_FPR[i],2)+ifelse(tmp.CI$Z[i]==0, -.005, .005), y=c(tmp.CI$TPR_0.025[i], tmp.CI$TPR_0.975[i]), pch="-", cex=1.5)
}




#type 2
myROC.type2.CI <- comprisk.ROC.CI( times = crdata$times, 
                                status1 = crdata$status1, 
                                status2 = crdata$status2, 
                                x = crdata$x, 
                                Z = crdata$Z, 
                                predict.time = 10, 
                                type = 2, 
                                fixed.values = c(.05, .45, .6), 
                                fixed = "FPR", ## one could also set this as TPR
                                alpha = .05, 
                                bootstraps = 100) 

myROC.type2.CI


}

