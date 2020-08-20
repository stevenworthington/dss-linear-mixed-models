
##
## Lecture 02: Representations of data and merging data
##
##
## This file generates aggregate, school-level variables and analyzes them using
## ordinary linear regression.
##
## WARNING: This is not _quite_ a valid analysis, because it does not take
## heteroskedasticity into account. In a future lecture we will talk about
## robust standard errors which would give correct p-values and standard errors.


# read student data
library( foreign )
dat = read.spss( "hsb1.sav", to.data.frame=TRUE )
head( dat )


# read in school data
sdat = read.spss( "hsb2.sav", to.data.frame=TRUE )

head( sdat )
nrow( sdat )





# Make aggregate variables using individual data, collapsing by school
library( tidyverse )

col.dat = dat %>% group_by( id ) %>% 
    summarize( per.fem = mean(female),
                 per.min = mean(minority),
                 mean.ses = mean(ses),
                 mean.ach = mean(mathach),
                 n.stud = n() )


head(col.dat)

# combine our school-level variables (ours and theirs) into one data.frame
sdat = merge( sdat, col.dat, by="id", all=TRUE )
head( sdat )



##
## Looking at our school level data
##


# mean school achievement vs mean school ses
ggplot( sdat, aes( x=mean.ses, y=mean.ach ) ) +
  geom_point( pch=19 )

ggplot( dat, aes( x=ses, y=mathach ) ) +
  geom_point( pch="." )

ggplot( dat, aes( x=ses, y=mathach ) ) +
  geom_point( pch="." ) +
  geom_point( data=sdat, aes( x=mean.ses, y=mean.ach ), col="red" )

library( skimr )
skim( sdat$mean.ach )
skim( dat$mathach )


# the standard deviation of the school-level means.
# BUT THIS IS GOING TO BE TOO LARGE, due to measurement error from having only
# a sample of students at each school.
sd( sdat$mean.ach )


# look at differences by sector.  Math achievement differs, but so does
# the mean ses.  How do we compare schools of different sectors which also
# have similar ses?
sdat$sec = factor( sdat$sector, levels = c(0,1), labels=c("public","catholic" ) )
ggplot( data=sdat, aes( x=sec, y=mean.ach ) ) +
  geom_boxplot()

  





##
## Explaining variation across schools with school-level predictors
##

ll.exp = lm( mean.ach ~ sector + size + pracad + disclim + himinty, data=sdat )
summary( ll.exp )




