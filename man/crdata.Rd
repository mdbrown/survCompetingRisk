\name{crdata}
\alias{crdata}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Data for Competing Risk Package
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Simulated time to event data, where there are two competing causes of failure. These data were generated to be used as an example data set along with the R package 'CompetingRisk'. 
}
\usage{data(crdata)}
\format{
  A data frame with 250 observations on the following 5 variables.
  \describe{
    \item{\code{times}}{a numeric vector of min(time to event, censoring time) for each individual. }
    \item{\code{status1}}{  vector of indicators specifying that event 1 was the cause of failure (equals 1 for individuals where event 1 occured)}
    \item{\code{status2}}{vector of indicators specifying that event 1 was the cause of failure (equals 1 for individuals where event 2 occured)}
    \item{\code{x}}{numeric vector of biomarker of interest}
    \item{\code{Z}}{vector of discrete covariate values. Here Z takes on values 0 or 1.}
  }
}

