survCompetingRisk
========================================================

This package aims to help to evaluate the prognostic accuracy of a marker with multiple competing risk events. Functions to calculate the AUC, ROC, PPV, and NPV are provided. A discrete covariate Z, if available, can be included.

There are five main functions in this package:

1. `comprisk.ROC`: Calculate the values needed to plot the ROC curve, along with the AUC.

2. `comprisk.ROC.CI`: Calculate bootstrap confidence intervals for a given set of TPR or FPR values.

3. `comprisk.AUC.CI`: Calculate bootstrap confidence intervals for the AUC.

4. `comprisk.PPV`: Calculate PPV and NPV values.

5. `comprisk.PPV.CI`: Calculate bootstrap confidence intervals for a given set of PPV, NPV, or v (marker quantile).



### Tutorial


```r

library(survCompetingRisk)
```

```
## Loading required package: survival Loading required package: splines
```

```r
# simulated data for illustration
data(crdata)
```


You can also embed plots, for example:


```r
plot(cars)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


### References
For more information regarding estimation procedures see the following references:

Zheng Y, Cai T, Jin Y, Feng Z. Evaluating prognostic accuracy of biomarkers under competing risk. *Biometrics.* 2012 Jun;68(2):388-96.

Zheng Y, Cai T, Feng Z, and Stanford J. Semiparametric Models of Time-dependent Predictive Values of Prognostic Biomarkers. *Biometrics.* 2010, 66: 50-60.

