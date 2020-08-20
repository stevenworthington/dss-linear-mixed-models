##
## This script illustrates random intercept models with the HS&B dataset
##

## In this script:
## Fit a random intercept and fixed slope model:
##  Y_i = alpha_j[i] + beta SES
##
## Calculate the ICC
##
## Look at level 2 covariate (sector) in two ways:
## (a) Aggregation
## (b) Fitting a MLM
##


library( lme4 )
library( arm )
library( tidyverse )


# read student data
library( foreign )
dat = read.spss( "hsb1.sav", to.data.frame=TRUE )
head( dat )

# fit the model
M0 = lmer( mathach ~ 1 + (1|id), data=dat )
display( M0 )

# including SES as a level 1 covariate
M1 = lmer( mathach ~ 1 + ses + (1|id), data=dat )

display( M1 )

sd( dat$mathach )






#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

##### Looking at variation in math achievement, and calculating the ICC   #####

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


display( M0 )

# We can just type in numbers...
2.93^2 / (2.93^2 + 6.26^2)

# These functions grab different parts of the model out.
VarCorr( M0 )

sigma.hat( M0 )

sigma( M0 )
sigma.hat( M0 )$sigma$id


# Calculate ICC for intercept only model
sigma.alpha = sigma.hat( M0 )$sigma$id
sigma.y = sigma( M0 )
ICC =  sigma.alpha^2 / (sigma.alpha^2 + sigma.y^2 )
ICC


# An aside: Is the estimate of the total variation close to actual variation in
# our sample? (It should be.)

# Our model says:
sigma.alpha^2 + sigma.y^2

# The data says:
var( dat$mathach )
# Yay!  They are close!!



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

#### Impact of level 1 covariates and the ICC when we have level 1 covariates #####

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Compare our two models
library( texreg )
screenreg( list( M0, M1 ) )
# Notice how ses has reduced our residual variance for BOTH the individual
# students and the schools.

# Now look at model that adjusting for SES.
# The ICC here is the proportion of UNEXPLAINED variation due to the school random effect.
sigma.alpha = sigma.hat( M1 )$sigma$id
sigma.y = sigma( M1 )
ICC =  sigma.alpha^2 / (sigma.alpha^2 + sigma.y^2 )
ICC


# It is _lower_ because we are explaining variation with SES and so systematic
# differences in SES across schools are no longer contributing to school
# variation.





#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

##### Thinking about Level 2 covariates.  #####

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# Libraries needed
library( lme4 )
library( arm )
library( tidyverse )

## We next explore the two methods for answering
##
## RQ: How is the variation of mean math achievement across schools associated
## with school type (public vs. catholic)?
## 


# read student data
library( foreign )
dat = read.spss( "hsb1.sav", to.data.frame=TRUE )
head( dat )



##### Method 1: Answering our question the aggregation way  #####


# Fit our random intercept model (no covariates; we just want to estimate school averages)
M0 = lmer( mathach ~ 1 + (1|id), data=dat )

display( M0 )

head( ranef( M0 )$id  )

# First we get our individual school regression lines
alphas = coef( M0 )$id
head( alphas )
str( alphas )

alphas = rename( alphas, alpha = `(Intercept)` )

# we need to add our id back in
alphas$id = rownames( alphas )
head( alphas )
nrow( alphas )


# Next we read in the school level data, merge it with our
# estimated alphas, and then make a boxplot.
sdat = read.spss( "hsb2.sav", to.data.frame=TRUE )
head( sdat )

schools = merge( sdat, alphas, by="id" )

head( schools )

# Look at our predicted school means and how they vary by sector
schools$sector = factor( schools$sector, levels=c(0,1), labels=c("public","catholic") )
ggplot( schools, aes( x=sector, y=alpha ) ) +
  geom_boxplot() +
  coord_flip()



# We can formally test to see if the intercepts are systematically different
t.test( alpha ~ sector, data=schools )


# Or with regression
M.sum = lm( alpha ~ sector, data=schools )
summary( M.sum )



##### Method 2: Answering our question the full multilevel modeling way ######

## We do this by including a school-level predictor in our random effects model directly!

head( sdat )
head( dat )

# Make single dataset with student and school level covariates
all.dat = merge( dat, sdat, by="id", all.x=TRUE )
head( all.dat )

M2 = lmer( mathach ~ 1 + sector + (1|id), data=all.dat )
display( M2 )


# Some inference.  Sector matters!
confint( M2 )




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

##### Digging in to R: Comparing fixef(), ranef() and coef()  #####

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# The fixed effects
fixef( M2 )

# The random effects
length( ranef( M2 ) )
names( ranef( M2 ) )
head( ranef( M2 )$id )

# This collapses the fixed and random for each group.  Very convienent!
head( coef( M2 )$id )
# Each row is a school.
# Note how intercept varies, but now it varies around the grand mean instead of 0, the fixed effects do not.


