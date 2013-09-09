\name{comprisk.ROC}
\alias{comprisk.ROC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
calculate ROC curves for competing risk models
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates true positive rate (TPR) and false positive rate (FPR) for time to event data when there are potentially two causes of failure (competing risks). These values can be used to plot ROC curves. AUC is also calculated. 

Two different ways of defining cases for these data can be accounted for in the analysis (see 'type' variable below). 
}
\usage{
comprisk.ROC(times, status1, status2, x, Z = NULL, predict.time, type, smooth=FALSE, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{times}{vector of min(time to event, censoring time) for each individual. 
}
  \item{status1}{vector of indicators specifying that event 1 was the cause of failure (should equal 1 for individuals where event 1 occured)
}
  \item{status2}{vector of indicators specifying that event 2 was the cause of failure (should equal 1 for individuals where event 2 occured)
}
  \item{x}{ vector of marker values
}
  \item{Z}{ vector of (discrete) covariate values
}
  \item{predict.time}{The prediction time to use to calculate the TPR and FPR. Should be a single numeric value. 
}
  \item{type}{Can take on two values. 
  
  Set type = 1 if case is defined by the event of interest, and controls are all the rest. 
  Set type = 2 if case is defined by stratifying on event type, and controls are those who have not experienced any events.  
 
}
  \item{smooth}{  TRUE/FALSE (default). When set to TRUE non-parametric smoothing is used instead of Cox proportional hazards model. Note, when smooth = TRUE, Z is ignored. Also, optional tuning parameter sigma may be set when using smooth =TRUE. 
  
}

  \item{sigma}{ Tuning parameter to be set when smooth = TRUE. The default value of sigma = sd(x)/length(x)^1/3}
}

\value{Returns a list of values
\item{AUC }{AUC values under each Z strata. If type = 2, AUC is also given for each event type }
\item{ROC }{A dataframe consisting of TPR, FPR, and Z values. 

If type = 1, the names of the data frame are TPR, FPR and Z.  


If type = 2, we stratify on event type, so we have seperate TPR and FPR values for different events. 
Thus, the names of the data frame are TPR.event1, FPR.event1, TPR.event2, FPR.event2, Z. 
  
  See 'references' for more information on type. 
}
%% ...
}
\references{
 
Zheng Y, Cai T, Jin Y, Feng Z. Evaluating prognostic accuracy of biomarkers under competing risk. In press, Biometrics for further description of methods. 
}
\author{
Marshall Brown and Yingye Zheng
}


\seealso{
\code{\link{comprisk.ROC.CI}}

}
\examples{

data(crdata)

## comprisk.ROC
#type 1
myROC.type1 <- comprisk.ROC( times = crdata$times,
                             status1 = crdata$status1, 
                             status2 = crdata$status2, 
                             x = crdata$x, 
                             Z = crdata$Z, 
                             predict.time = 10, 
                             type = 1)
                                                   
myROC.type1

#plot
tmp <- myROC.type1$ROC

plot(tmp$FPR[tmp$Z==0], tmp$TPR[tmp$Z==0], type="l",lwd=2, main="Type 1", xlab="FPR", ylab="TPR")
lines(tmp$FPR[tmp$Z==1], tmp$TPR[tmp$Z==1], lty=2, lwd=2)
legend(x="bottomright", c("Z = 0", "Z = 1"), lty=c(1,2))
lines(c(0, 1), c(0,1), col="lightgrey")


myROC.type2 <- comprisk.ROC( times = crdata$times,
                             status1 = crdata$status1, 
                             status2 = crdata$status2, 
                             x = crdata$x, 
                             Z = crdata$Z, 
                             predict.time = 10, 
                             type = 2)
                                                   
myROC.type2



}
\references{

Zheng Y, Cai T, Jin Y, Feng Z. Evaluating prognostic accuracy of biomarkers under
competing risk. Biometrics. 2012 Jun;68(2):388-96.


Zheng Y, Cai T, Feng Z, and Stanford J. Semiparametric Models of Time-dependent
Predictive Values of Prognostic Biomarkers. Biometrics. 2010, 66: 50-60.


}
