##
## This script demos how to fit various random-slope models on the HS&B dataset
##
## The second half looks at a level-2 covariate in such a model.
##

# various libraries
library( arm )
library( foreign )
library( tidyverse )

# read student data
dat = read.spss( "hsb1.sav", to.data.frame=TRUE )
head( dat )

# read school data
sdat = read.spss( "hsb2.sav", to.data.frame=TRUE )
head( sdat )



#### Three plots of three different schools ####

head( dat )

# Get three schools
sids = c( 3377, 9397, 2655 )
subdat = subset( dat, id %in% sids )

ggplot( subdat, aes( ses, mathach ) ) +
  facet_wrap( ~ id ) +
  geom_point() +
  geom_smooth( method="lm", se=FALSE ) +
  geom_vline( xintercept = 0, lty=2, col="darkgrey" ) +
  geom_hline( yintercept = 0, lty=2, col="darkgrey" )



#  ** STOP HERE FOR LECTURE ** 



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#### Fitting a random slope model ####

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# The model
#  Y_i = alpha_j[i] + beta_j[i] SES_i + epsilon_i


# fit the model
library( lme4 )

M1 = lmer( mathach ~ 1 + ses + (1 + ses|id), data=dat )

display( M1 )


# individual school random effects ("re's")
res = coef( M1 )$id
head( res )
names( res ) = c( "alpha", "beta" )
res$id = rownames( res )
head( res )


# how do alphas and betas covary?
ggplot( res, aes( alpha, beta ) ) +
  geom_point()

cor( res$alpha, res$beta )



# Make a plot of our 160 schools
#
# Note: We have to specify our x and y limits since there is no data to tell us
# what to plot.  If we omit the scale lines, we get nothing!
ggplot( data=res ) +
    scale_x_continuous(limits=range( dat$ses ) ) +
    scale_y_continuous(limits=range( dat$mathach ) ) +
  geom_abline( aes( intercept = alpha, slope=beta ), alpha=0.25) +
  labs( x="SES", y="Math Achievment" )


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#### Fitting a model to each school independently ####

# ***** Note: this code is "extra curricular" *****

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# Fancy coding: fitting a simple linear regression model to each school
library( broom )

# First we make a school level dataset where there is a column of all the
# student data.
#
# I.e. the value of the 'data' variable for each school is a dataframe of all
# the students in the school.  Neat!
schools = dat %>% group_by( id ) %>% nest()
schools

# Our "data" variable has a dataframe for each value.  E.g.,
schools$data[[1]]


# Now fit a model to each school and get our coefficients
fit_school = function( df ) {
    M = lm( mathach ~ ses, data=df )
    data.frame( alpha = coef( M )[[1]],
                beta = coef( M )[[2]] )
}
schools = mutate( schools,
                  coefs = map( data, fit_school ) )
schools

# Note how our results are packed into a single column of our dataframe.  It is
# 'nested'
# But our answer is there:
schools$coefs[[1]]

# So we next get out the coefficients into columns using "unnest"
# (The dropping data is so it doesn't try to unnest the data column also.)
schools = schools %>%
    unnest( coefs )
schools


# And plot (same plotting code as with our fit lines)
# Make a plot of our 160 schools
ggplot( data=schools ) +
    scale_x_continuous(limits=range( dat$ses ) ) +
    scale_y_continuous(limits=range( dat$mathach ) ) +
    geom_abline( aes( intercept = alpha, slope=beta ), alpha=0.25) +
    labs( x="SES", y="Math Achievment" )


rm( schools )


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

##### Simulating realistic data #####

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# A plot of 160 NEW schools generated from our model (to see how our partially
# pooled estimates are OVER SHRUNK/OVER SMOOTHED

display( M1 )
fixef( M1 )
VarCorr( M1 )$id

res.fake = mvrnorm( 160, mu=fixef( M1 ), Sigma=VarCorr( M1 )$id  )
head( res.fake )
class( res.fake )

res.fake = data.frame( res.fake )
names( res.fake ) = c( "alpha", "beta" )

ggplot( data=res.fake ) +
    scale_x_continuous(limits=range( dat$ses ) ) +
    scale_y_continuous(limits=range( dat$mathach ) ) +
    geom_abline( aes( intercept = alpha, slope=beta ), alpha=0.25) +
    labs( x="SES", y="Math Achievment" )




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
##
#####  Looking at level-2 covariates   #####
##
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-


# Aggregation approach to see how the random effects vary by school type.
# To do this we read in the school level data, merge it with our
# estimated res, and then make a boxplot.
sdat = read.spss( "hsb2.sav", to.data.frame=TRUE )
head( sdat )

head( res )
sdat.all = merge( sdat, res, by="id" )

head( sdat )

ggplot( sdat.all, aes( sector, alpha, group=sector ) ) +
    geom_boxplot() +
    coord_flip()
ggplot( sdat.all, aes( sector, beta, group=sector ) ) +
    geom_boxplot() +
    coord_flip()


# Coding Super-Powers Aside: If you want to do both at once
sdat.long = gather( sdat.all, alpha, beta, key="coef", value="value" )
head( sdat.long )

ggplot( sdat.long, aes( sector, value, group=sector ) ) +
    facet_wrap( ~ coef, ncol=1 ) +
    geom_boxplot() +
    coord_flip()

# or to give different axes for each plot
ggplot( sdat.long, aes( sector, value, group=sector ) ) +
    facet_wrap( ~ coef, ncol=1, scales = "free" ) +
    geom_boxplot() +
    coord_flip()



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
##
##### Fitting random slope model with  level-2 covariates            ######
##
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

dat = merge( dat, sdat, by="id", all.x=TRUE )
head( dat )

# sector impacts the intercept
M2 = lmer( mathach ~ 1 + ses + sector + (1+ses|id), data=dat )
display( M2 )

# sector impacts both the intercept and the slope
M2 = lmer( mathach ~ 1 + ses * sector + (1+ses|id), data=dat )
display( M2 )




