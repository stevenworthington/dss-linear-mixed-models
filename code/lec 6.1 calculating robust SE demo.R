
##
## Illustrating sandwich and cluster robust standard error calculations
##
## NOTE: Sadly, this data set doesn't really show how SEs increase when we take
## these issues into account. This provides an important reminder that these
## methods allow for clustering, but if the clustering is weak, then our results
## may be different than what we expect. That being said, using robust SEs is
## definitely a good safety precaution.
## 


library( tidyverse )
library( foreign )
library( arm )
library( lme4 )

options( digits=3 )

#### Load the Data  ####

## Load the National Youth Survey data from the Longitudinal unit (see
## Raudenbush and Brky, complex error structure lectures)

nyswide = read.csv("nyswide.csv")
head(nyswide)
nys1.na = reshape(nyswide, direction="long", #we want it in long format
              varying=list(ATTIT=paste("ATTIT",11:15,sep="."), 
                          EXPO=paste("EXPO",11:15,sep=".") ),
              v.names=c("ATTIT","EXPO"), idvar="ID", timevar="AGE", times=11:15)

# Drop missing ATTIT values
nys1 = filter( nys1.na, !is.na(nys1.na$ATTIT) )

head( nys1 )


# Tally number of observations within group (youth)
table( table( nys1$ID ) )

# Number of observations for each age
table( nys1$AGE )


# Look at variance for each age
# (It goes up by age.)
nys1 %>% group_by( AGE ) %>%
  summarise( mean.ATTIT = mean( ATTIT ),
             sd.ATTIT = sd( ATTIT ) )





#### Make fake cross sectional data with only one observation per ID ####

## We do this to illustrate heteroskediastic robust standard errors


# order rows randomly
nys1 = nys1[ sample(1:nrow(nys1) ), ]

# select first observation for each ID
nys1.solo = filter( nys1, !duplicated( ID ) )
nrow( nys1.solo )

# Check: each ID is here ONCE!
table( table( nys1.solo$ID ) )



#### The Sandwich Estimator : Heteroskedastic Robust Standard Errors  ####

# Fit simple model of attitude on age
M0 = lm( ATTIT ~ 1 + AGE + FEMALE, data=nys1.solo )
arm::display( M0 )

# Look at the Variance-Covariance of our fixed effect estimates
VC = vcov( M0 )
VC
diag( VC )

# Our standard errors (under homoskedasticity)
SE.homo = sqrt( diag( VC ) )
SE.homo



# Look at variance for each age
# (It goes up by age.)
nys1.solo %>% group_by( AGE ) %>%
  summarise( mean.ATTIT = mean( ATTIT ),
             sd.ATTIT = sd( ATTIT ) )


ggplot( nys1.solo, aes( y=ATTIT, group=AGE ) ) + 
  geom_boxplot()

nys1.solo$resids = resid( M0 )

# See heteroskedasticity: big age is bigger residuals!
ggplot( nys1.solo, aes( y=resids, group=AGE ) ) + 
  geom_boxplot()

nys1.solo %>% group_by( AGE ) %>%
  summarise( sd.resid = sd( resids ) )



# Huber-White / Sandwich / Heteroskedastic-Robust SEs
library( sandwich )
vcov_sand = vcovHC(M0, type = "HC0")
vcov_sand

SE.sand <- sqrt( diag( vcov_sand ) )
SE.sand

se.coef(M0)

# Weirdly, our sandwich estimators are SMALLER---this can happen depending on
# data structure.  They typically will be bigger.
SE.sand / SE.homo


library( lmtest )
coeftest( M0, vcov. = vcov_sand )

# Short-cut
coeftest( M0, vcov. = vcovHC, type = "HC0" )






##### Simulation Example where the correct SEs are larger #####

N = 100
dat = data.frame( X = runif( N, -2, 2 ) )
dat$Y.pred = dat$X * 2
dat$Y = dat$Y.pred + rnorm( N, mean = 0, sd = 2*abs( dat$X ) + 0.1 )

ggplot( dat, aes( X, Y ) ) +
  geom_point()


M0 = lm( Y ~ X, data=dat )
summary( M0 )

coeftest( M0, vcov. = vcovHC, type = "HC2" )




##### Simulation Example where the correct SEs are smaller #####

N = 100
dat = data.frame( X = runif( N, -2, 2 ) )
dat$Y.pred = dat$X * 2
dat$Y = dat$Y.pred + rnorm( N, mean = 0, sd = 4 - 2*abs( dat$X ) + 0.1 )

ggplot( dat, aes( X, Y ) ) +
  geom_point()


M0 = lm( Y ~ X, data=dat )
summary( M0 )

coeftest( M0, vcov. = vcovHC, type = "HC2" )
