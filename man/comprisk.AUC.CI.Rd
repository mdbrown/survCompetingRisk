\name{comprisk.AUC.CI}
\alias{comprisk.AUC.CI}

\title{
bootstrap confidence interval for AUC under competing risk models%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates bootstrap confidence intervals for AUC for time to event data when there are two causes of failure (competing risks). Two different ways of defining cases for these data can be accounted for in the analysis (see 'type' variable below).  Confidence bands for the AUC is derived from the empirical bootstrap distribution.
}
\usage{
comprisk.AUC.CI(times, status1, status2, x, Z = NULL, predict.time, type, smooth=FALSE, alpha = 0.05, bootstraps = 500, sigma)
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
  The prediction time to use to calculate the AUC. Should be a single numeric value. 
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

  A dataframe with the following values: Z, lower bound CI for AUC, upper bound CI for AUC. Each row is a confidence interval for the specific Z strata. If no Z is specified, Z is defined as equalling 1 for all individuals, and can be ignored here. 
  
 If type = 1, only one confidence interval for AUC is given.
 If type = 2, we stratify on event type, and so confidence intervals are given for both event 1 and for event 2.  

}
\references{

Zheng Y, Cai T, Jin Y, Feng Z. Evaluating prognostic accuracy of biomarkers under
competing risk. Biometrics. 2012 Jun;68(2):388-96.


Zheng Y, Cai T, Feng Z, and Stanford J. Semiparametric Models of Time-dependent
Predictive Values of Prognostic Biomarkers. Biometrics. 2010, 66: 50-60.


}


\examples{

data(crdata)

#type 1 
myAUC.type1.CI <- comprisk.AUC.CI( times = crdata$times, 
                                status1 = crdata$status1, 
                                status2 = crdata$status2, 
                                x = crdata$x, 
                                Z = crdata$Z, 
                                predict.time = 10, 
                                type = 1, 
                                alpha = .05, 
                                bootstraps = 100) 
#confidence iterval                                
myAUC.type1.CI

myROC.type1 <- comprisk.ROC( times = crdata$times,
                             status1 = crdata$status1, 
                             status2 = crdata$status2, 
                             x = crdata$x, 
                             Z = crdata$Z, 
                             predict.time = 10, 
                             type = 1)
#estimate                                                    
myROC.type1$AUC

#type 2
myAUC.type2.CI <- comprisk.AUC.CI( times = crdata$times, 
                                status1 = crdata$status1, 
                                status2 = crdata$status2, 
                                x = crdata$x, 
                                Z = crdata$Z, 
                                predict.time = 10, 
                                type = 2, 
                                alpha = .05, 
                                bootstraps = 100) 
                                
#confidence iterval                                
myAUC.type2.CI

myROC.type2 <- comprisk.ROC( times = crdata$times,
                             status1 = crdata$status1, 
                             status2 = crdata$status2, 
                             x = crdata$x, 
                             Z = crdata$Z, 
                             predict.time = 10, 
                             type = 2)
#estimate                                                    
myROC.type2$AUC
}

