##
## This script demos confidence intervals and inference on the HS&B dataset
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


options( digits = 3 )

#  ** STOP HERE FOR LECTURE ** 


###### Getting point estimates and standard errors  #####

M1 = lmer( mathach ~ 1 + ses * sector + (1 + ses|id), data=dat )


# fixed effects
b.hat = fixef( M1 )
b.hat

# standard errors for fixed effects
se.hat = se.fixef( M1 )
se.hat


###### Making confidence intervals   #####

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

# 90% confidence intervals using Wald (the SE based normal approximation classic ones)
CIs = confint( M1, method="Wald", level = 0.9 )
CIs


# Profile confidence intervals
# (The default of the confint() method.)

confint( M1 )


# For a specific parameter
confint( M1, "ses" )



##### t-tests for fixed effects  ######

# our t-values
fixef( M1 ) / se.fixef( M1 )

t = fixef( M1 ) / se.fixef( M1 )
t
2 * pnorm( t, lower.tail = FALSE )


# R can automatically add p-values etc to the summary output!
# (This is an end run around the maker of the lme4 package)
# You may need to install the lmertest package first:
# install.packages( "lmerTest" )

# NOTE: This package _overrides_ the classic lmer() stuff
# So you have to re-call lmer() after you load the library to get p-values.
# (Or just load the library at the top of your script.)

library( lmerTest )
M1 = lmer( mathach ~ 1 + ses * sector + (1 + ses|id), data=dat )
summary( M1 )
# Notice all the p-values and whatnot?


# Testing whether SES relates to math achievment
M1 = lmer( mathach ~ 1 + ses + sector + (1 + ses|id), data=dat )
display( M1 )

ts = fixef( M1 ) / se.fixef( M1 )
ts
pnorm( ts[ "ses" ], lower.tail = FALSE )

# Or automated way
summary( M1 )


# Looking at df: note how female has lots of df, and not other things.
M1b = lmer( mathach ~ 1 + female + ses + sector + (1 + ses|id), data=dat )
summary( M1b )


###### LR test for fixed effects   #######

mod = lmer( mathach ~ 1 + ses + sector + (ses|id), data= dat, REML = FALSE )
mod_null = lmer( mathach ~ 1 + sector + (ses|id), data= dat, REML = FALSE )


# Doing LR test by hand (for illustration)
ll1 <- logLik(mod)
ll1
ll0 <- logLik(mod_null)
ll0
delta_ll <- ll1 - ll0
2 * delta_ll
delta_ll = as.numeric( delta_ll )
2 * delta_ll
delta_p = 1
pchisq(2*as.numeric(delta_ll), delta_p, lower.tail = FALSE)

# The plot of results: see how unusual our observed ratio is compared to the
# distribution of what we would expect by random chance?
x = seq( 0, 1.1 * 2*delta_ll, length.out=300 )
y = pchisq( x, df=delta_p, lower.tail = FALSE )
qplot( x, y, geom="line" ) +
  geom_vline( xintercept=2*delta_ll, col="red" ) +
  labs( x = "2*LLR", y="probability density" )


# Automated!   lrtest and anova are doing the same thing.
library( lmtest )
lrtest( mod, mod_null )
anova( mod, mod_null )


###### Multiple parameters   #######

# Model
# Yij = beta_0j + beta_1j ses_ij + epsilon_ij
# beta_0j = gamma_00 + gamma_01 sector_j + u_0j
# beta_1j = gamma_10 + gamma_11 sector_j + u_1j

# Testing Null of two parameters being 0:
# We are testing H_0: gamma_01 = 0 and gamma_11 = 0
M1 = lmer( mathach ~ 1 + ses * sector + (1 + ses|id),
             data=dat, REML=FALSE )

M0 = lmer( mathach ~ 1 + ses + (1 + ses|id), 
             data=dat, REML=FALSE )

anova( M0, M1 )


# Side note: Contrasts!!
# Using the lmertest package (for testing contrasts)

library( lmerTest )

summary( M1 )

fixef( M1 )

# Does ses + ses:sector add up to a null of 0?
contest( M1, L = c( 0, 1, 0, -1 ) )
# It does not.



###### LR test for random effects  ######

# Let's test for whether there is a random slope, i.e.,
# do different schools have different SES-math achievement relationships.

# Testing our *NESTED MODELS* for random slope 
M0 = lmer( mathach ~ 1 + ses + sector + (1 |id), 
           data=dat, REML=FALSE )

M1 = lmer( mathach ~ 1 + ses + sector + (1 + ses|id),
           data=dat, REML=FALSE )

library( lmtest )

# These two commands are the same.  anova will refit your model with ML if
# needed, so it is nicer.
lrtest( M0, M1 )
anova( M0, M1 )


# Testing for random slope (beyond sector as slope)
M1 = lmer( mathach ~ 1 + ses * sector + (1 + ses|id),
           data=dat, REML=FALSE )

M0 = lmer( mathach ~ 1 + ses * sector + (1 |id), 
           data=dat, REML=FALSE )


anova( M0, M1 )


# Compare our two models. Sector is explaining some cross-school variation, so
# we are no longer significant once we take it into account.
library( texreg )
screenreg( list( M0, M1 ) )













