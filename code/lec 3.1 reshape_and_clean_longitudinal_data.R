#
# This script gives an example of reshaping
# In particular, this script illustrates going from wide to long,  and long to wide format
# (Useful for longitudinal data in particular.)
#
# This also demonstrates making a time variable out of a factor (e.g., wave)
# and how to plot some growth curves
library( tidyverse )


# First make a toy dataset in long format
orig.dat = data.frame( id=rep( c("A","B","C"), each=3 ),
                  time = rep(1:3, 3 ) )
orig.dat$X = c( 1.3, 1.3, 1.3, 2.2, 2.2, 2.2, 1.5,1.5,1.5 )
orig.dat$Y = orig.dat$X + orig.dat$time + rnorm( 9 )

# drop an observation so we have a missing observation.
orig.dat = orig.dat[ -5, ]

# Let's look at what we got!
orig.dat



##
## Going to wide format from long
##


# Going to wide format (something we rarely want to do)
dta = spread( orig.dat, key="time", value="Y" )
dta




# Alternate approach: going wide with reshape() method
# (Ignore unless you need reshape()'s power)
dta = reshape( orig.dat, 
               idvar="id", 
               timevar="time", 
               direction="wide", 
               v.names = "Y" )
names(dta)[3:5] = c("wave1","wave2","wave3")
dta



##
## Going to long format from wide.
##

# Now let's use our wide data and see if we can get back to long.

# Our recommendation: use tidyverse
# See http://r4ds.had.co.nz/tidy-data.html#spreading-and-gathering

library( tidyverse )
dat = gather( dta, wave1, wave2, wave3, key='wave', value='score' )
dat


# Alternate approach: using the reshape call
dat <- reshape(dta, direction = 'long', 
               varying = list(c('wave1', 'wave2', 'wave3') ),
               times = c('wave1', 'wave2', 'wave3'),
               v.names = 'score', timevar = 'wave', idvar = 'id')
dat


# Another alternate approach
# Using the reshape2 package  (we do not recommend this, but it can be 
# nicer than base reshape)
library( reshape2 )

melt( dta, id.vars=c("id","X"), variable.name="time",
      value.name="score" )

# bad.  ALL covarites need to be in the id.  Annoying.
melt( dta, id.vars=c("id"), variable.name="time",
      value.name="score" )

# can also do this, and name the measured ones
melt( dta, measure.vars=c("wave1","wave2","wave3"), variable.name="time",
      value.name="score" )



##
## Saving and loading data
##

# If you want to save your tidy data, you should!  Then you load it
# later and work on it.
write.csv( dat, file="clean_data.csv", row.names=FALSE )

# then you would load in seperate file:
dat = read.csv( file="clean_data.csv" )
dat


saveRDS( dat, file="clean_data.rds" )

# then you would load in seperate file:
loaded.dat = readRDS( file="clean_data.rds" )
loaded.dat



##
## Converting a factor to a time variable
##


# Now we want to make a real time variable
dat$time = 0
dat$time[ dat$wave=="wave2" ] = 1
dat$time[ dat$wave=="wave3" ] = 4

dat

library( ggplot2 )
dat

# Our points
ggplot( data=dat, aes(x=time, y=score, group=id) ) +
    geom_point()


# Missing data kills one of our lines!
ggplot( data=dat, aes(x=time, y=score, group=id) ) +
    geom_line()


# Drop missing data explicitly, don't let R do it.
dat.comp = dat[ complete.cases( dat ), ]
dat.comp

# You can also use 'na.omit' in one step
dat.comp = na.omit( dat )


# Both points and lines with complete data (we get all our lines back)
ggplot( data=dat.comp, aes(x=time, y=score, group=id) ) +
    geom_line() +
    geom_point()

# Each person gets their own plot
ggplot( data=dat.comp, aes(x=time, y=score) ) +
  facet_wrap( ~ id ) +
  geom_line() +
  geom_point()


# Looking at our waves
ggplot( data=dat.comp, aes( x = wave, y=score) ) +
    geom_boxplot() 
