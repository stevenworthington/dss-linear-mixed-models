

##
## Illustration of generating Poisson data and
## seeing how the standardized residuals are nice and normal
## if we have correct model specification.
##
## Also illustrates how omitted variables or measurement error
## on exposure can mess up these residuals.
##

library( arm )
library( tidyverse )


# So we get the same random display each time we run the script
set.seed( 12345 )

#### Step 1: Generate the data  ####

# sample size
N = 200

# Our covariate.  Log makes the covariate linearly related to expected count.
X = log( runif( N, 0.1, 3 ) )
summary( X )

# How much each unit gets "exposed" (i.e., observation time)
exposure = runif( N, 0.5, 2 )

# our rate (this is our linear regression part of the GLM)
rate = 2 * X + 1
skimr::skim( rate )


# our outcome
Y = rpois( N,  lambda = exp( rate ) * exposure )
skimr::skim(Y)

# Collect our observed data into a data frame
# This is our simulated data.
df = data.frame( X=X, exposure=exposure, Y=Y,
                 logY = log( Y+1 ) )

ggplot( df, aes( X, Y ) ) +
  geom_point()

ggplot( df, aes( X, logY ) ) +
  geom_point()

qplot( df$Y )


# if we divide Y by rate
df$Y.norm = df$Y / exposure
qplot( X, Y.norm, data=df )





#### Fitting a correctly specified Poisson model  ####

# Fit our model
fit.1 <- glm ( Y ~ X, family=poisson, 
                offset=log(exposure), data=df )

display( fit.1 )
# We see we get roughly incercept = 1, coef for X of 2
# which aligns with our setup.


# This is a function (new command) that takes a fit model and our
# data and makes some plots and prints some stuff out.
explore.results = function( mod, df ) {
  # Predict our outcomes based on our model
  df$Y.hat = predict( mod, type = "response" )
  print( head( df$Y.hat ) )
  
  cat( "The residuals:\n" )
  print( skimr::skim( df$Y.hat ) )
  
  # How do predictions and residuals relate
  df = mutate( df, resid = Y - Y.hat )
  plt <- qplot( Y.hat, resid, data=df ) +
    labs( title = "Residual vs. Predicted")
  print( plt )
  
  # Make standardized z-scores
  df = mutate( df, z = (Y - Y.hat) / sqrt( Y.hat ) )
  cat( "The standardized residuals:\n" )
  print( skimr::skim( df$z ) )
  
  # Look at them with 95% confidence bands.  Most dots are
  # inside the bands
  plt <- qplot( Y.hat, z, data=df ) +
    geom_hline( yintercept=c(-2,2), lty=2 ) +
    labs( title = "Standardized residual vs. predicted" )
  print( plt )
  
  # Return modified dataframe, in case we want it later.
  # But mainly, this function just does stuff
  invisible( df )
}


explore.results( fit.1, df )


##
####  Now looking at how we might get an overdispersed Poisson model  #####
##
## One way we might have this is if we don't have a
## good exposure variable
##

# we add measurement error to our exposure
df$bad.exposure = df$exposure + runif( N, -0.4, 0.4 )

# see?  We roughly have exposure, but not exactly.
qplot( exposure, bad.exposure, data=df )

# Fit our model
fit.2 <- glm ( Y ~ X, family=poisson, 
               offset=log(bad.exposure), data=df )

display( fit.2 )

df = explore.results( fit.2, df )


# Note we have more points too far from what we expect (outside
# our 95% lines)


# Doing the hypothesis test of overdispersion
n = nrow(df)
n
k = 1
D = sum( df$z^2 ) 
D
D / ( n - k )
n - k
pchisq( D, n-k, lower.tail = FALSE )
# We see we have overdispersion if the p-value is low.



# Let's fix it:
# We fit an overdispersed model
fit.od <- glm ( Y ~ X, family=quasipoisson, offset=log(bad.exposure), data=df )

# compare the SEs: the overdispersed model accounts for greater uncertainty 
# (Note the larger SEs)
display( fit.od )
display( fit.2 )


##
## Another way of getting overdispersion:
##      missing predictors
##


# Fit our model without our X (oopse!)
fit.3 <- glm ( Y ~ 1, family=poisson, 
               offset=log(exposure), data=df )

display( fit.3 )

df = explore.results( fit.3, df )


# Doing the hypothesis test of overdispersion
n = nrow(df)
n
k = 1
D = sum( df$z^2 ) 
D
n - k
sqrt( D / ( n - k ) )
pchisq( D, n-k, lower.tail = FALSE )

# Again, our overdispersed model accounts for uncertainty better
fit.od <- glm ( Y ~ 1, family=quasipoisson, offset=log(exposure), data=df )
display( fit.od )
display( fit.3 )

