\name{comprisk.PPV}
\alias{comprisk.PPV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
calculate PPV curves for competing risk models
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function calculates positive prediction value (PPV) and negative predictive value (NPV) for time to event data when there are potentially two causes of failure (competing risks). These values can be used to plot the respective curves. 

Two different ways of defining cases for these data can be accounted for in the analysis (see 'type' variable below). 
}
\usage{
comprisk.PPV(times, status1, status2, x, Z=NULL, predict.time, type, smooth = FALSE, sigma)
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
\item{sigma}{ Tuning parameter to be set when smooth = TRUE. The default value of sigma = sd(x)/length(x)^1/3}
}

\value{
A dataframe consisting of v = marker quantiles, PPV, NPV,   and Z values. 

If type = 1, the names of the data frame are v,  PPV, NPV and Z. 
 
If type = 2, we stratify on event type, so we have seperate PPV and NPV values for different events. 
Thus,  the names of the data frame are v, PPV.event1, NPV.event1, PPV.event2, NPV.event2, and Z.
}
\references{
%% ~put references to the literature/web site here ~
Zheng Y, Cai T, Jin Y, Feng Z. Evaluating prognostic accuracy of biomarkers under competing risk. In press, Biometrics for further description of methods.
}
\author{
%%  ~~who you are~~
Marshall Brown and Yingye Zheng
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{comprisk.PPV.CI}}
}
\examples{
#type 1
data(crdata)

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

#type 2
myPPV.type2 <- comprisk.PPV( times = crdata$times,
                             status1 = crdata$status1, 
                             status2 = crdata$status2, 
                             x = crdata$x, 
                             Z = crdata$Z, 
                             predict.time = 10, 
                             type = 2)

head(myPPV.type2)


}
\references{

Zheng Y, Cai T, Jin Y, Feng Z. Evaluating prognostic accuracy of biomarkers under
competing risk. Biometrics. 2012 Jun;68(2):388-96.


Zheng Y, Cai T, Feng Z, and Stanford J. Semiparametric Models of Time-dependent
Predictive Values of Prognostic Biomarkers. Biometrics. 2010, 66: 50-60.


}
