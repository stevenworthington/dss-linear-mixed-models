
##
## This script has two simulations illustrating how the sandwich and cluster
## robust SEs are better able to estimate the true variability of our impact
## estimates.
##
## The simulations generate 1000 datasets, and analyze them.  They then look at
## how the true variation in the beta coefficent estimates corresponds to the
## average SEs estimated by the different approaches considered.
##
## (C) Miratrix, 2018

library( tidyverse )
library( arm )
library( sandwich )



#### Simulation Demo #1 - Sandwich #####

N = 300


# Our data
dat = data.frame( X = runif( N, min = 3, max = 14 ) )
dat = mutate( dat, Y = 10 + 5 * X + rnorm( N )*X^2 / 3 )
ggplot( dat, aes( X, Y ) ) + geom_point()


one.run = function( N ) {
  
  # Generate new heteroskedastic dataset
  dat = data.frame( X = runif( N, min = 3, max = 14 ) )
  dat = mutate( dat, Y = 10 + 5 * X + rnorm( N )*X^2 / 3 )
  
  M1 = lm( Y ~ 1 + X, data = dat )
  
  SE.wrong = se.coef( M1 )
  SE.wrong
  
  vcov_sand = sandwich::vcovHC(M1, type = "HC1")
  vcov_sand
  SE.sand <- sqrt( diag( vcov_sand ) )
  SE.sand
  
  rs = data.frame( coef = names( SE.wrong ), est = coef( M1 ), SE.wrong=SE.wrong, SE.sand=SE.sand )
  rs
}

# Our function compares naive to sandwich SEs.
one.run( 300 )

# Do this lots to see trends.
rps = plyr::rdply( 1000, one.run( N ) )
head( rps )

# This shows how the sandwich is getting the true SEs more correct than the
# naive.
rps %>% group_by( coef ) %>% 
  summarise( mean.est = mean( est ),
             SE.true = sd( est ),
             mean.wrong = mean(SE.wrong ),
             mean.sand = mean(SE.sand ) )




#### Simulation Demo #2 - Cluster Robust #####


N = 3000

gen.dat = function( N ) {
  
  # Generate some random intercept data
  sdat = data.frame( G = LETTERS,
                     W = rnorm( 26 ),
                     u = rnorm( 26 ) )
  
  dat = data.frame( G = sample( LETTERS, N, replace=TRUE ) )
  dat = merge( dat, sdat, by = "G" )

  dat = mutate( dat, X = runif( N, W - 2, W + 2 ),
                Y = 10 + 5 * X + 10 * W + 10 * u + 6 * rnorm( N ) )
  dat
}

dat = gen.dat( N )
ggplot( dat, aes( X, Y, col=G ) ) + geom_point()


# Same game, but now clustered

one.run = function( N ) {
  
  # Generate new clustered dataset
  dat = gen.dat( N )
  
  M1 = lm( Y ~ 1 + X + W, data = dat )
  
  SE.wrong = se.coef( M1 )
  SE.wrong
  
  
  vcov_sand = sandwich::vcovHC(M1, type = "HC1")
  vcov_sand
  SE.sand <- sqrt( diag( vcov_sand ) )
  SE.sand
  
  vcov_clust = sandwich::vcovCL( M1, dat$G )
  vcov_clust
  SE.clust = sqrt( diag( vcov_clust ) )
  SE.clust
  
  rs = data.frame( coef = names( SE.wrong ), est = coef( M1 ), SE.wrong=SE.wrong, SE.sand=SE.sand, SE.clust = SE.clust )
  rs
}

# Our function compares naive to sandwich SEs.
one.run( 300 )

# Do this lots to see trends.
rps = plyr::rdply( 1000, one.run( N ) )
head( rps )

# This shows how the CRVE is getting the true SEs more correct than the
# naive and the sandwich (although all do ok with X).
rps %>% group_by( coef ) %>% 
  summarise( mean.est = mean( est ),
             SE.true = sd( est ),
             mean.wrong = mean(SE.wrong ),
             mean.sand = mean(SE.sand ),
             mean.clust = mean(SE.clust ) )




