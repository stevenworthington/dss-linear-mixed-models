##
## Stepping through the make fake data method
##
library(arm)
library(lme4)
library( plyr )
library( mosaic )

##-------------------------------------------------------------------##
##  load the data
##-------------------------------------------------------------------##

## Use HIV CD4 data
hivdat <- read.csv("cd4.csv", header=TRUE)

## Eliminate missing data & just consider the "control" patients (treatmnt==1)
## with initial age between 1 and 5 years
keep <- with( hivdat, treatmnt==1 & !is.na(CD4PCT) & (baseage > 1 & baseage < 5) )
table( keep, useNA = "always" )
dat <- subset( hivdat, keep )
nrow(dat)

names(dat)

## Redefine variables and make new dataframe with only the variables we want
dat = data.frame( person = dat$newpid,        # ids for the kids
                  y = sqrt(dat$CD4PCT),                 # transform cd4 percentage to square root scale to better fit an additive model
                  age.baseline = dat$baseage,           # kid's age (yrs) at the beginning of the study
                  age.measurement = dat$visage,         # kid's age (yrs) at the time of measurement
                  treatment = dat$treatmnt,             # don't know why they left out the letter 'e'
                  time = with(dat, visage - baseage) )   # time (yrs) between visit and beginning of study
head( dat )

## Re-index patients from 1 to J by converting their ID to a factor and then to a numeric scale.
## (This removes the dropped patients from above.  Note newpid was a number to start with.
str( dat$person )
dat$person = as.numeric( as.factor( dat$person ) )

# count the number of people:
J = length( unique( dat$person ) )
J

##-------------------------------------------------------------------##
##  Explore the data a bit
##-------------------------------------------------------------------##

favstats( time, data=dat )

# when did we see patients?
plot( person ~ time, data=dat )

# what are outcomes like?
favstats( y, data=dat )

##-------------------------------------------------------------------##
##  Random-intercept, random-slope model for longitudinal CD4 data   ##
##-------------------------------------------------------------------##

## Fit random-intercept, random-slope model
# y_i ~ N( alpha_j[i] + beta_j[i]*t_i , sigma_y^2)
# (alpha_j, beta_j) ~ BivariateNormal( (gam.0a, gam.0b), V )
# i indexes measurements taken at time t_i on kid j
M1 <- lmer( y ~ 1 + time + (1 + time | person), data=dat )
display(M1)  


## Plot observed sqrt(cd4%) vs time for 83 control kids in HIV study
## Figure 20.5 (a) in Gelman & Hill
plot( NA, xlim=range(dat$time), bty="n",
      xlab="time (years)", ylab="sqrt (CD4%)",
      main="observed data", ylim=c(0,8))

for (j in 1:J){
    pers = subset( dat, person == j )
    points( pers$time, pers$y, type="l", col="slateblue")
}

# An alternative plotting method using the plyr package
d_ply( dat, .(person), function ( per ) {
    points( y ~ time, data=per, type="l", col="slateblue" )
} )


## PLot individual estimated trend lines, alphahat_j + betahat_j*t
## using mean estimates from random-intercept, random-slope model (M1)
## Figure 20.5 (b) in Gelman & Hill
alphahat <- coef(M1)$person$`(Intercept)`
betahat <- coef(M1)$person$time

plot( 1, 1, xlab="time (years)", ylab="sqrt (CD4%)", type='n',
      main="estimated trend lines", xlim=c(0,2), ylim=c(0,8))
for (j in 1:J){
    curve(alphahat[j] + betahat[j]*x, add=TRUE, col="plum4")
}



##------------------------------------------------##
##   A hypothesized model of treatment effects    ##
##------------------------------------------------##

# Based on model (M1) fitted to untreated HIV-positive kids,
# (untreated kids were those who didn't receive zinc supplement)
# we found an average decline in sqrt(CD4%) of 0.47 per year.
# Let's conduct power calculations assuming zinc treatment reduces
# this average sqrt(CD4%) decline to zero per year.
# (i.e. Assume kids who take zinc supplement then have,
# on average, no decline of CD4% over time)

# See p.450 in Gelman & Hill for assumed working model

# Set model parameters using estimates from M1 fitted on controls.
# For simplicity, assume correlation between intercepts and slope is 0,
# even though it was estimated to be 0.15.

# Study design: Assume J HIV-positive kids randomly assigned into one of two treatments.
# J/2 receive regular care (control); other J/2 also receive zinc supplements (treatment).
# Measure CD4% every 2 months over 1 year, giving K=7 measurements per kid.
#-----------------------------------------------------------------------------



##-------------------------------------------------------------------##
##  Get parameters out of our initial model and make fake data
##-------------------------------------------------------------------##


M1 <- lmer( y ~ 1 + time + (1 + time | person), data=dat )
display(M1)

mu.a <- 4.85
g.0 <- -0.47
g.1 <- 0.47
sigma.y <- 0.75
sigma.a <- 1.33
sigma.b <- 0.68

# number of time points per year
K = 7

# number of people to simulate for
J = 3

# make time variables
time <- rep( seq(0, 1, length=K), J )  # K measurements during the year
time

table( time )

person <- rep(1:J, each=K)             # kid IDs
table( person )

# assign people to treatment
treatment <- as.numeric( sample( J ) <= J/2 )
treatment

# make a treatment label for each individiual person-time point
treatment1 <- treatment[person]    # expand treatment indicator to be a data-level predictor
treatment1
# so that it can be used in lmer() later

## personal-level parameters
a <- rnorm(J, mu.a, sigma.a)
a

b <- rnorm(J, g.0 + g.1*treatment, sigma.b)
b


## data
y.hat =  a[person] + b[person]*time
y.hat

y <- y.hat + rnorm( J*K, sd=sigma.y)
y

# the final fake data frame!
dat = data.frame( y=y, time=time, person=person, treatment1=treatment1 )
dat



# Aside: look at data we generated

library( lattice )
xyplot( y ~ time|as.factor(person), group=as.factor(treatment1), data=dat, type="l" )

# adding the y-hat true growth lines - a messy way with lattice
dd = data.frame( y = c( dat$y, y.hat ), pred=rep( c("no","yes"), each=length(y.hat) ), time=c(dat$time,dat$time), person=c(dat$person,dat$person) )
head( dd )
table( dd$person, dd$pred )

xyplot( y ~ time|as.factor(person), group=pred, data=dd, type="b" )




##
## Now we bundle it all up
##

# our make fake data function
make.fake.data <- function(J, K,
                           mu.a, g.0, g.1,
                           sigma.y, sigma.a, sigma.b){
    
    time <- rep( seq(0, 1, length=K), J )  # K measurements during the year
    person <- rep(1:J, each=K)             # kid IDs
    treatment <- as.numeric( sample( J ) <= J/2 )   # assign half to treat, half to control
    treatment1 <- treatment[person]    # expand treatment indicator to be a data-level predictor
    # so that it can be used in lmer() later
    
    ## personal-level parameters
    a <- rnorm(J, mu.a, sigma.a)
    b <- rnorm(J, g.0 + g.1*treatment, sigma.b)
    
    ## data
    y <- rnorm(J*K, a[person] + b[person]*time, sigma.y)
    
    return( data.frame(y, time, person, treatment1) )
}


# test the code
make.fake.data( J, K,
                mu.a, g.0, g.1,
                sigma.y, sigma.a, sigma.b )