# Looking at an analysis of our toy 10 school dataset.

# Data set: 1982 High School and Beyond Study
#
# This dataset is an example of students nested in schools
#
# For further discussion and illustration, see Chapter 4 of Raudenbush and Bryk

# Note on student-level data variables
# SES: a mix of family education, income, and occupation
# mathach: math achievement
#
# Note on school-level data
# himinty: 40%+ minority
# meanses: mean (average) ses of students in school


# Libraries so we can do stuff
library( foreign )
library( tidyverse )


##
## Loading data and looking at individuals
##

# read student data
dat = read.spss( "hsb1.sav", to.data.frame=TRUE )
str( dat )
head( dat )

nrow( dat )


# This controls the random number generation.
# This makes it so we all have the same 10 schools
set.seed( 12345 )




##
## Subset 10 schools
##

sids = unique( dat$id )
length( sids )

winners = sample( sids, 10 )
winners

nrow( dat )
dat.ten = filter( dat, id %in% winners )
nrow( dat.ten )

table( dat.ten$id )


# We have kept all the other school id as levels of our factor.  We don't want that
# so we clean our dataframe with droplevels to make our factors only have the levels
# that are in the remaining data.
dat.ten = droplevels( dat.ten )
table( dat.ten$id )


# What does our data look like?
head( dat.ten )
tail( dat.ten, 3 )


##
## Running a model on just the students (ignoring schools)
##

M0 = lm( mathach ~ 1 + ses, data=dat.ten )


# Residual plot
dat.ten$resid = resid( M0 )
ggplot( dat.ten, aes( x=id, y=resid ) ) +
  geom_boxplot() +
  geom_jitter( width=0.1, size=0.5) +
  geom_hline( yintercept = 0, col="red" )

M.resid = lm( resid ~ id, data=dat.ten )
summary( M.resid )
# This shows us our residuals are clustered by school--i.e., some schools have
# lots of positive residuals and some lots of negative.  This violates our
# assumption of ignoring the school.





##
## Fixed effect model
##

# Running some regressions with "fixed effects" for school
M1 = lm( mathach ~ ses + id, data=dat.ten, x=TRUE )
summary( M1 )
coef( M1 )


# Grab row 1, 51, 101, 151, and 201 from our matrix
# The "," separates row and column indices
# Nothing after the "," means take all columns
M1$x[ c(1, 51, 101, 151, 201 ), ]


dat.ten$resid.fe = resid( M1 )
ggplot( dat.ten, aes( x=id, y=resid.fe ) ) +
  geom_boxplot() +
  geom_jitter( width=0.1, size=0.5) +
  geom_hline( yintercept = 0, col="red" )

M.resid = lm( resid.fe ~ id, data=dat.ten )
summary( M.resid )




## 
## Other fixed effect models
##


# The "- 1" means NO INTERCEPT, so each school gets its own coefficient
# which is the mean
M2 = lm( mathach ~ id - 1, data=dat.ten )
summary( M2 )
coef( M2 )


# We can also have ses and no intercept
M3 = lm( mathach ~ ses + id - 1, data=dat.ten, x=TRUE )
summary( M3 )
coef( M3 )


# What is our "design matrix" or "model matrix" (R automatically makes dummy
# variables when we give it a factor like 'id'
head( M3$x )



# A model with interaction terms
M4 = lm( mathach ~ ses * id - 1, data=dat.ten, x=TRUE )
summary( M4 )
coef( M4 )


# Look at all the stuff R did for us!
head( M4$x )



## Making a plot looking at individual school regression lines
## and an overall regression line
## (school is a "fixed effect")

# Our data
ggplot( dat.ten, aes( ses, mathach ) ) +
  facet_wrap( ~ id ) +
  geom_point()

# with a best fit line
ggplot( dat.ten, aes( ses, mathach ) ) +
  geom_point() +
  geom_smooth( method="lm", se=FALSE )

ggplot( dat.ten, aes( ses, mathach, group=id ) ) +
  geom_point() +
  geom_smooth( method="lm", se=FALSE, fullrange=TRUE )



# No intercept, fixed effects
M1 = lm( mathach ~ 0 + ses + id, data=dat.ten )
coef( M1 )


# make a table of our best fit lines

lines = data.frame( inter = coef(M1)[2:11],
                    slope = coef(M1)[[1]] )

# plot our data and our 10 best fit lines on top of it
# (also add our overall line ignoring school in blue)
ggplot( dat.ten, aes( ses, mathach ) ) +
  geom_point( size=0.75, alpha=0.5 ) +
  geom_smooth( method="lm", se=FALSE, col="blue" ) +
  geom_abline( data=lines, aes( slope=slope, 
                                intercept=inter ), 
               col="red", lty=2 )




# A best fit line for each group fit separately (using geom_smooth())
ggplot( dat.ten, aes( ses, mathach, group=id ) ) +
  facet_wrap( ~ id ) +
  geom_point() +
  geom_smooth( method="lm", se=FALSE, fullrange=TRUE ) 






