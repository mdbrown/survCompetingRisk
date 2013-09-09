\name{comprisk.PPV.CI}
\alias{comprisk.PPV.CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
calculate bootstrap CI's for PPV curves under competing risk models
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates bootstrap confidence intervals for either the positive prediction value (PPV) or the negative prediction value (NPV) for time to event data when there are potentially two causes of failure (competing risks). The user is able to fix one of PPV, NPV or v = marker quantile. Confidence bands for the non-fixed variables are then derived from the empirical bootstrap distribution at the values given by "fixed.values". 

Two different ways of defining cases for these data can be accounted for in the analysis  (see 'type' variable below).   

}
\usage{
comprisk.PPV.CI(times, status1, status2, x, Z = NULL, predict.time, type, smooth = FALSE, fixed.values = 1:20/20, fixed = "v", alpha = 0.05, bootstraps = 500, sigma)
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
  The prediction time to use to calculate the PPV and NPV. Should be a single numeric value. 
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
  Which quantity to fix. Must be one of "v", "PPV", or "NPV". Default is fixed = "v", so that confidence bands for PPV and NPV, at fixed values of FPR, are output.   
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

 A dataframe with the following values: Z, fixed values, lower bound CI 1, upper bound CI 1, lower bound CI 2, upper bound CI 2. 
 The user is able to fix one of PPV, NPV, or v (= marker quantile), so confidence bounds are given for the other (non-fixed) variables. For example, if v is fixed, confidence bounds for NPV and PPV will be provided. The columns are automatically named to aviod confusion. 
 
 If type = 1, only one set of confidence intervals are given for each varying measure. 
 If type = 2, we stratify on event type, and so confidence intervals are given for both event 1 and for event 2 and for each varying measure.   

}
\references{

Zheng Y, Cai T, Jin Y, Feng Z. Evaluating prognostic accuracy of biomarkers under
competing risk. Biometrics. 2012 Jun;68(2):388-96.


Zheng Y, Cai T, Feng Z, and Stanford J. Semiparametric Models of Time-dependent
Predictive Values of Prognostic Biomarkers. Biometrics. 2010, 66: 50-60.


}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{comprisk.PPV}}
}
\examples{

data(crdata)

## comprisk.PPV.CI
#type 1

myPPV.type1.CI <- comprisk.PPV.CI( times = crdata$times, 
                                status1 = crdata$status1, 
                                status2 = crdata$status2, 
                                x = crdata$x, 
                                Z = crdata$Z, 
                                predict.time = 10, 
                                type = 1, 
                                fixed.values = c(.05, .45, .6), 
                                fixed = "v", ## one could also set this as "PPV" or "NPV"
                                alpha = .05, 
                                bootstraps = 100) 

myPPV.type1.CI


myPPV.type1 <- comprisk.PPV( times = crdata$times,
                             status1 = crdata$status1, 
                             status2 = crdata$status2, 
                             x = crdata$x, 
                             Z = crdata$Z, 
                             predict.time = 10, 
                             type = 1)
                                                   
head(myPPV.type1)


#plot

tmp <- myPPV.type1

par(mfrow=c(1,1))
plot(tmp$v[tmp$Z==0], tmp$PPV[tmp$Z==0], type="l",lwd=2, main="Type 1", xlab="v", ylab="PPV", ylim=c(0,1))
lines(tmp$v[tmp$Z==1], tmp$PPV[tmp$Z==1], lty=2, lwd=2)
legend(x="bottomright", c("Z = 0", "Z = 1"), lty=c(1,2))

#add horizontal lines at P( T < predict.time, experienced event of interest | Z)
abline( h = min(tmp$PPV[tmp$Z==1]), lty = 2, lwd = 2)
abline( h = min(tmp$PPV[tmp$Z==0]), lty = 1, lwd = 2)



#add CI's to plot
tmp.CI <- myPPV.type1.CI

for( i in 1:nrow(myPPV.type1.CI)){
#offset the confidence intervals by .005 for Z=0 and Z=1, so we can tell the CI's apart on the plot
  lines(x=rep(tmp.CI$fixed_v[i],2) + ifelse(tmp.CI$Z[i]==0, -0.005, .005), y=c(tmp.CI$PPV_0.025[i], tmp.CI$PPV_0.975[i]), lty=ifelse(tmp.CI$Z[i]==0, 1, 2), lwd=1.5)
  
  points(x=rep(tmp.CI$fixed_v[i],2)+ifelse(tmp.CI$Z[i]==0, -.005, .005), y=c(tmp.CI$PPV_0.025[i], tmp.CI$PPV_0.975[i]), pch="-", cex=1.5)
}


 myPPV.type2.CI <- comprisk.PPV.CI( times = crdata$times, 
                                status1 = crdata$status1, 
                                status2 = crdata$status2, 
                                x = crdata$x, 
                                Z = crdata$Z, 
                                predict.time = 10, 
                                type = 2, 
                                fixed.values = c(.05, .45, .6), 
                                fixed = "v", ## one could also set this as PPV or NPV
                                alpha = .05, 
                                bootstraps = 100) 

myPPV.type2.CI



}

