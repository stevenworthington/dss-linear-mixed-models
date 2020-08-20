# Demonstration of manipulating two-level clustered data.

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


# What does SES look like
qplot( dat$ses, bins=30 )


# Make a plot of how many students are sampled from each school.
counts = table( dat$id )
library( ggthemes )
theme_set( theme_tufte() )
qplot( as.numeric( counts ), bins=30, fill=I("grey" ) )



# How does achievement correlate with SES?
ggplot( dat, aes( ses, mathach ) ) +
  geom_point( alpha=0.25, size=0.3 ) +
  geom_smooth( method="lm", se=FALSE, col="red" )


## Regression line, ignoring school clustering altogether
ll = lm( mathach ~ 1 + ses, data=dat )
summary( ll )




##
## Connecting schools and students: Representations of data and merging data
##


# read in school data
sdat = read.spss( "hsb2.sav", to.data.frame=TRUE )

head( sdat )
nrow( sdat )




# make single data frame with all variables
dat = merge( dat, sdat, by="id", all.x=TRUE )
head( dat )
nrow( dat )




##
## Part II:
## Looking at 10 schools from the schools of choice dataset
## And calculating group-level variables for those 10 schools
##


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

# Simple regression on SES (bad because clustering not taken into account)
M0 = lm( mathach ~ 1 + ses, data = dat.ten )
summary( M0 )

# Running a simple regression with some of our school-level covariates
# (Still bad: clustering still not taken into account)
M0 = lm( mathach ~ 1 + ses + sector + pracad + himinty, data = dat.ten )
summary( M0 )


# Running some regressions with "fixed effects" for school
M1 = lm( mathach ~ 1 + ses + id, data=dat.ten, x=TRUE )
summary( M1 )
coef( M1 )


# Grab row 1, 51, 101, 151, and 201 from our matrix
# The "," separates row and column indices
# Nothing after the "," means take all columns
M1$x[ c(1, 51, 101, 151, 201 ), ]



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
# variables when we give it a factor like 'id')
head( M3$x )

