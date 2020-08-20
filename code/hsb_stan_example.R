##
## Lecture example demonstrating fitting a model in Stan
##
##
## This script fits the following model:
## Y_{ij} = \beta_{0j} + \beta_{1j} ses_{ij} + \epsilon_{ij}
## \epsilon_{ij} ~ N( 0, sigma^2_y )
##
## \beta_{0j} = \gamma_{00} + \gamma_{01} sector_j + u_{0j}
## \beta_{1j} = \gamma_{10} + \gamma_{11} sector_j
## u_{0j} ~ N( 0, sigma^2_u )
##
## Note the lack of random effect for the slope, and the lack of sector in the
## main intercept term.

library( tidyverse )
library( rstan )
library( lme4 )

options(digits=3) #Avoid excessive precision in displayed values.

# This limits printout so it doesn't get too big
options( max.print = 200 )

#### Load and setup the data  ####

# Load data into R as usual
hsstud <- read.csv( "hsb1.csv", header=TRUE )  
hssch <- read.csv( "hsb2.csv", header=TRUE )

head( hssch )


# This creates a unique ID for each school in the order that the schools are
# in the school dataset.  This means we can refer to a school's data by looking
# at the row of the school's id.  Stan needs this numbering.
hssch$sid = 1:nrow(hssch)

# This copies over the new school IDs to the students so we can cross-index
# appropriately.
hsstud = merge( hsstud, hssch, by="id", all=TRUE )
head( hsstud )
str( hsstud$sid )

# Create data list of all our variables that we will feed into Stan.
# The names must match the names in the .stan file.
hsb_dat <- list(           
    nstudents = nrow(hsstud),
    nschools = nrow(hssch),
    sid = hsstud$sid,
    ses = hsstud$ses,
    sector = hsstud$sector,
    mathach = hsstud$mathach
)



#### Fit model and get samples from posterior  ####

# Our model is stored in a file.  
# We read in that file and compile the model
hsb_model <- stan_model(file="hsb_model1.stan")

# fit the model by handing the sampling() method our model and our data
model1mcmc <- sampling(hsb_model, data = hsb_dat, chains=4, iter=1000)  

# Wat we get
model1mcmc


# Make traceplots of some of the main parameters to check for mixing.
# (We want to see a mess.)
traceplot(model1mcmc, pars=c("gamma_00","gamma_10","gamma_01","sigma_y") )


# We can extract the values out of the "stanfit" object that Stan returned and
# then work with it like we do for regular dataframes. Each row is a single
# posterior draw across all the parameters.  Each column is a parameter.
all_samps <- as.data.frame(model1mcmc) 
dim( all_samps )

# The first 10 draws of the first 8 parameters
all_samps[1:10, 1:8]



###### Exploring the output  #####


qplot( all_samps$gamma_00 )


# Plot the posterior of a few parameters
pparam = all_samps[1:9]
pparam = gather( pparam, key="param", value="draw" )
ggplot( pparam, aes( draw ) ) +
  facet_wrap( ~ param, scales = "free_x" ) +
  geom_histogram()




#### Doing inference with the posterior draws  #####

# Another way of getting our posterior draws is with extract().
# Here, we see each variable is on a list
draws = extract( model1mcmc )
names( draws )


# Vectors of parameters (e.g., our random intercepts) 
# are bundled into a matrix, each row being
# a sample and each column being a school.
u0s = draws$u0
dim( u0s )


# Look at the joint posterior distribution of gamma_00 and gamma_10
qplot( draws$gamma_00, draws$gamma_10 )


# We can just extract some of our parameters if we wish:
draws = extract( model1mcmc, pars=c("gamma_00","gamma_10","gamma_01","gamma_11", "sigma_y","sigma_u") )
length( draws )


#### Summarizing our posterior draws  #####

# a utility function for calculating nice things
pointAndInterval = function(samps, conf=0.95) {
    trim.m=mean(samps,trim=0.1)
    m = mean(samps)
    med = median(samps)
    cred = quantile(samps,c((1-conf)/2,1-(1-conf)/2) )
    names( cred ) = c( "C.low", "C.high" )
    trim.sd = sqrt(sum((samps-trim.m)^2)/(length(samps)-1))
    data.frame( mean=m, 
       se=sd(samps), 
       cred.l = cred[[1]],
       cred.h = cred[[2]],
       median=med, 
       trim.mean=trim.m, 
       trim.se=trim.sd )
}

# Calculate our summaries for each parameter of interest
# The below gives us point estimates and credible intervals for each parameter.
# WARNING: make sure draws doesn't have any matrices in it (e.g., from u0)
estimates = map_df( draws, pointAndInterval, .id="parameter" )
estimates


# Get the individual school effects by running our method on each column of our
# u0s matrix
alp.est = plyr::adply( u0s, 2, pointAndInterval )

# We get a dataframe of our schools.  Each row is a summary of what we
# believe about that school's intercept.
dim( alp.est )
class( alp.est )
head( alp.est )


# The following shows a potentially weak positive correlation between
# academic climate and the school intercept (which makes sense)
par( mfrow=c(1,1) )
plot( alp.est$mean ~ hssch$pracad )
abline( lm (  alp.est$mean ~ hssch$pracad ) )


##### Plotting posteriors ######

# Plot the posterior distributions of the model parameters

# First we make a long format where we have each row be a parameter name and value
draws = extract( model1mcmc, pars=c("gamma_00","gamma_10","gamma_01","gamma_11", "sigma_y","sigma_u") )
draws = as.data.frame( draws )
draws = gather( draws, key="parameter", value="draw" )
head( draws )

# Now plot
head( draws )
estimates
ggplot( draws, aes( draw ) ) +
  facet_wrap( ~ parameter, scales="free_x" ) +
  geom_histogram() +
  geom_vline( data=estimates, aes(xintercept=median ), col="red" )




# Plot the posterior distributions for some schools
u0s = extract( model1mcmc, pars="u0" )
u0s = u0s$u0
dim( u0s )

u0s = gather( as.data.frame( u0s ), key="school", value="draw" )
head( u0s )

u0.hat = u0s %>% group_by( school ) %>%
  summarise( u0.hat = mean( draw ),
             SE.hat = sd( draw ) )

s9 = sample( u0.hat$school, 9 )

u0.sub = filter( u0s, school %in% s9 )
u0.hat.sub = filter( u0.hat, school %in% s9 )

ggplot( u0.sub, aes( draw ) ) +
  facet_wrap( ~ school ) +
  geom_histogram() +
  geom_vline( data=u0.hat.sub, aes( xintercept=u0.hat ), col="red" )





##### Comparing to lmer  #####


# Same model fit with lmer
hsb_lm = lmer(mathach ~ 1 + sector*ses + (1|sid), data=hsstud)
arm::display(hsb_lm)

estimates

# Note how the parameter estimates (and SEs for fixed effects) are very similar.


## Investigation: EB estimates vs our estimates

ests = coef( hsb_lm )$sid
head( ests )

# Our EB estimates from lmer and bayesian estimates from Stan are basically the same:
plot( ests[,1] ~ alp.est$mean )

