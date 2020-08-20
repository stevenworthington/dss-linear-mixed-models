##
## This script demos confidence intervals on the HS&B dataset
##
## 

# various libraries
library( arm )
library( foreign )
library( tidyverse )
library( lme4 )


# read student data
dat = read.spss( "hsb1.sav", to.data.frame=TRUE )
head( dat )

# read school data
sdat = read.spss( "hsb2.sav", to.data.frame=TRUE )
head( sdat )


dat = merge( dat, sdat, by="id", all.x=TRUE )


# Fitting a baseline model with ses 
M1 = lmer( mathach ~ 1 + ses + meanses + (1 + ses|id), data=dat )
display( M1 )



#  ** STOP HERE FOR LECTURE ** 



##
## Looking at SEs for the "fixed effects"
##

help( se.fixef )  # from arm package


# fixed effects
b.hat = fixef( M1 )
b.hat

# standard errors for fixed effects
se.hat = se.fixef( M1 )
se.hat


# lower and upper bounds of a 95% confidence interval (Normal Approximation)
b.hat - 2 * se.hat
b.hat + 2 * se.hat

results = data.frame( b.hat = b.hat, 
                      se.hat = se.hat,
                      CI.low = b.hat - 2 * se.hat,
                      CI.high = b.hat + 2 * se.hat )

results


# Doing it with confint method
CIs = confint( M1, method="Wald")
CIs

CIs = confint( M1, method="Wald", level = 0.9 )
CIs



##
## Profile confidence intervals
## (The default of the confint() method.)
## 

confint( M1 )









