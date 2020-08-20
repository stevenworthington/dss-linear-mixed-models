# Vocab growth example
# 
# For further discussion and illustration, see Chapter 6 of Raudenbush and Bryk

# The level 1 file has a count of the cumulative
# number of unique vocabulary words uttered and various functions of child age
# in months. Each record is an occasion nested within children.
# 
# The level-2 file has one record per child; "mom-speak" is the cumulative
# number of words uttered by mom and "log-Mom" is
# the log of momspeak. It also has "group" --- see below.
# 
# A couple of tricky features
# 
# * The researchers gave us cumulative counts of words. So the assumption is
# that once a child utters a new word, we assume that child will not forget that
# word. However, this seems to work against using the model one might naturally
# envision, namely an exponential regression (eg log linear) model for the
# latent true vocab size at an occasion. That is why we used a polynomial model
# for change.
# 
# * The researchers observed essentially no words at 12 months and imputed 1 for
# everyone.
# 
# * There were two sets of data collection. The first 11 cases were intended to
# be observed every two months. Then the team got short on money and observed
# the second set of 11 cases every 4 months. This depresses the estimate of the
# cumulative vocabulary of the second group. You will see this very clearly by
# plotting the observed curves for the 22 kids. So we use "group" as a kind of
# blocking variable to account for this methodological artifact.


library( foreign )
library( tidyverse )


###### Loading data and getting it ready   #####

# read individual time data
dat = read.spss( "vocab1.sav", to.data.frame=TRUE )
str( dat )
head( dat )
nrow( dat )

# read child information data
sdat = read.spss( "vocab2.sav", to.data.frame=TRUE )
names( sdat ) = tolower( names( sdat ) )
nrow( sdat )
head( sdat )



# merge data to make single long form
dat = merge( dat, sdat, by="pers", all=TRUE )
nrow( dat )

# Note: I believe the 12 mo readings are actually just generated
# after the fact and are not real data.  We drop those.
filter( dat, age == 12 )
dat = filter( dat, age != 12 )

# Comment: We can not drop these to see how the models change.  They will
# change a great deal.


# we make group a factor
dat$group = as.factor( dat$group )


# We change our person variable to a factor.
dat = mutate( dat, pers = as.factor( pers ) )



###### Some exploratory EDA   ########

# looking at group and sex
table( sdat$group, sdat$sex )


# quickly looking at individidual curves with the "ggplot2" package.
library(ggplot2) 
ggplot( data=dat, aes( x=age, y=vocab, group=pers, col=group ) ) +
    geom_line()

ggplot( data=dat, aes( x=age, y=vocab, group=pers, col=group ) ) +
    facet_wrap( ~ pers ) +
    geom_line()

# we can merge both ways of plotting
ggplot( data=dat, aes( x=age, y=vocab, group=pers, col=group ) ) +
    facet_wrap( ~ group ) +
    geom_line()


# Looking at just 6 kids
set.seed( 101974 )
kids = sample( sdat$pers, 6 )
dat.sub = filter( dat, pers %in% kids ) 

ggplot( data=dat.sub, aes( x=age, y=vocab, group=pers, col=group ) ) +
    facet_wrap( ~ pers ) +
    geom_line() +
    geom_point( cex=2 )





##### Fitting the unconditional longitudinal data models  #####

library( lme4 )
library( arm )

head( dat )

# we look only at the 11 kids with fairly complete data to start.
dat.g0 = filter( dat, group==0 )
table( dat.g0$age12 )
table( dat$pers )


# Make our shifted age variable and our quadratic term
dat = mutate( dat, age12 = age - 12,
                  age12sq = (age - 12) ^ 2 )


# Our full quadratic model with three random effects (intercept, slope, acceleration)
M1 = lmer( vocab ~ 1 + age12 + age12sq + (1 + age12 + age12sq|pers), data=dat.g0 )
display( M1 )






###### Getting the growth curves   #####


# We can generate predictions for different ages by making a "typical kid"
# dataframe for a single kid with a whole bunch of ages.  We then predict the 
# outcome for each age given our model.
ages = 12 + seq( 0, 14, by = 1 )
head( ages )

typ.kid = data.frame( pers = -1, 
                      age = ages )
typ.kid = transform( typ.kid, age12 = age - 12,
                     age12sq = (age-12)^2 )
head( typ.kid )
typ.kid$vocab = predict( M1, typ.kid, allow.new.levels=TRUE )


# Here is an alternative way to do it by hand by just using the fixed effect coefficients.
# Here we are just using our functional form to calculate our predictions for each value 
# in our age sequence
fes = fixef( M1 )
fes
typ.kid.pred = fes[[1]] + fes[[2]]*(ages-12) + fes[[3]]*(ages-12)^2

# see, we get the same thing!   (our predictions were all the same)
typ.kid$vocab - typ.kid.pred



# Plot the individual raw-data curves with the mean "typical kid" curve on top 

ggplot( data=dat.g0, aes( x=age12, y=vocab ) ) +
    geom_line( aes( group=pers ) ) +
    geom_line( data=typ.kid, col="red", lwd=2 )


##
## to put the mean line on all facets, 
##

typ.kid$pers = NULL  
# if we leave the id for typ.kid then it makes a NEW facet for it.  Annoying.

ggplot( data=dat.g0, aes( x=age12, y=vocab ) ) +
    facet_wrap( ~ pers ) +
    geom_line( ) + geom_point() +
    geom_line( data=typ.kid, col="red", lwd=1, lty=2 )






##### Exploring the individual random effect estimates  ####


# Looking at predictions of random effects for each kid
refs = ranef( M1 )$pers
refs

# including the fixed effects
coefs = coef( M1 )$pers
coefs


# The following code takes a dataframe and then turns each row into three rows
# with a variable with the name of the original column, and a variable with
# the name of the value of that column.
head( refs )
refs.l = gather( refs, key="coef", value="estimate" )

ggplot( refs.l, aes( x=coef, y=estimate ) ) +
  geom_boxplot() +
  coord_flip()


#or we can look at each kid coefficient by coefficient
ggplot( refs.l, aes( x = estimate ) ) + 
  facet_wrap( ~ coef, ncol=1, scales="free" ) +
  geom_dotplot( ) + 
  labs( title = "Intercept, Slope, and Acceleration" ) +
  geom_vline( xintercept=0, col="red")




## 
##### Indivudal growth curves  #####
##

# we can predict what each kid should have at each timepoint with predict()
dat.g0$vocab.hat = predict( M1 )

ggplot( data=dat.g0, aes( x=age12, y=vocab.hat ) ) +
  facet_wrap( ~ pers ) +
  geom_line( ) + geom_point() +
  geom_line( data=typ.kid, aes( y = vocab ), col="red", lwd=1, lty=2 )


ggplot( data=dat.g0, aes( x=age12, y=vocab.hat ) ) +
  geom_line( aes( group=pers ) ) +
  geom_line( data=typ.kid, aes( y = vocab ), col="red", lwd=1, lty=2 )


# Individual curves and overall curve and raw data.
ggplot( data=dat.g0, aes( x=age12, y=vocab ) ) +
  facet_wrap( ~ pers ) +
  geom_line( aes( y = vocab.hat ), col="red", lwd=1, lty=1 ) +
  geom_line( data=typ.kid, col="grey", lwd=0.5, lty=2 ) +
  geom_line() + geom_point() 




# We can also predict for an old kid to get an estimate for his or her growth
# curve for more than the original data points we have.
# Here we by hand make one such plots for our first kid

# First, what are our ids?
unique( dat.g0$pers )

# Here we do 1 kid
typ.kid1 = typ.kid
typ.kid1$pers = 1  # the first kid has id 1 (see list of IDs, above)
typ.kid1$vocab = predict( M1, typ.kid1 )

ggplot( typ.kid1, aes( age, vocab ) ) +
  geom_line()


# Now we do all the kids  The way we do this is by "expand.grid" which will come up
# with all combinations of one list and another.
kids = expand.grid( pers = unique( dat.g0$pers ),
                    age = ages )
nrow( kids )
table( kids$pers )

# We have 100 ages for each kid in our first group.
# Note the 0s?  This is because "pers" is a factor, and it remembers we do not
# have any group 2 kids yet.

kids = mutate( kids, age12 = age - 12,
               age12sq = age12^2 )
kids$vocab = predict( M1, newdata=kids )



# Individual curves and overall curve and raw data with augmented curves
ggplot( data=dat.g0, aes( x=age12, y=vocab ) ) +
  facet_wrap( ~ pers ) +
  geom_line( data=kids, col="red", lwd=1, lty=1 ) +
  geom_line( data=typ.kid, col="grey", lwd=0.5, lty=2 ) +
  geom_line() + geom_point() 

# See how the lines are smoother and extend to the edges?


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
##
#####      Model refinement and simplification      ######
##
## Simplifying our model using knowledge of the science itself
##
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

# Let's try taking out the random variation of the intercept only.
M2a = lmer( vocab ~ 1 + age12 + age12sq + (0+age12+age12sq|pers), data=dat.g0 )
display( M2a )


# Is zapping random intercept OK?
library( lmtest )
M1.ML = lmer( vocab ~ 1 + age12 + age12sq + (1 + age12 + age12sq|pers), data=dat.g0, REML=FALSE )
M2a.ML = lmer( vocab ~ 1 + age12 + age12sq + (0 + age12 + age12sq|pers), data=dat.g0, REML=FALSE )
lrtest( M1.ML, M2a.ML )
# This says that this is a bad idea (but we do it anyway for illustration
# and due to scientific theory)


# Here we drop the intercept entirely, so both the fixed intercept
# and random component are removed
M2 = lmer( vocab ~ 0 + age12 + age12sq + (0+age12+age12sq|pers), data=dat.g0 )
display( M2 )


# Fix linear slope fixed effect to 0, but random slope effect still varies
M3 = lmer( vocab ~ 0 + age12sq + (0+age12+age12sq|pers), data=dat.g0 )
display( M3 )


# Note the display doesn't give us the name of age12sq.  But it is there:
fixef( M3 )
summary( M3 )

# Check to see if we can drop the random slope too
# Fix linear slope fixed effect to 0 and no random slope either
M3.ML = lmer( vocab ~ 0 + age12sq + (0 + age12 + age12sq|pers), data=dat.g0, REML= FALSE )
M3.simp.ML = lmer( vocab ~ 0 + age12sq + (0 + age12sq|pers), data=dat.g0, REML=FALSE )
lrtest( M3.ML, M3.simp.ML )
# Nope, need to keep varying slope!

library( texreg )
screenreg( list( M1, M2, M3) )

# Lr test of our three models (using anova, to auto-get ML estimates)
anova( M1, M2, M3 )



# Our simplest model is a fine refinement over the middle model (i.e., we don't need the fixed slope)
# but only scientific theory is what is driving us to drop the random intercept.  The testing
# says we should keep it.




##
##### Plots on new model M3  #####
## 

coef( M3 )$pers

typ.kid$pers = -1
typ.kid$vocab = predict( M3, typ.kid, allow.new.levels=TRUE )
typ.kid$pers = NA

dat.g0$vocab.hat = predict( M3 )

head( dat.g0 )
ggplot( dat.g0, aes( x=age, vocab, group=pers ) ) +
  geom_line( aes( y = vocab.hat ) ) +
  labs( x="Age of Kid (months)", y="Vocab of Kid" )





################################################################
##
###### Fitting the conditional model using predictors   ######
##   
################################################################


# First, just for our first group
M4A = lmer( vocab ~ 0 + age12sq * (sex + logmom) + (0 + age12 + age12sq|pers), 
           data=dat.g0 )
display( M4A )

# But this might be better?
M4A.2 = lmer( vocab ~ 0 + age12sq:sex + age12sq:logmom + (0 + age12 + age12sq|pers), 
            data=dat.g0 )
display( M4A.2 )

anova( M4A, M4A.2 )


# Make group number and sex coded to +1/-1 to make interactions easier.
# This -1/1 coding makes sex and group a departure from a grand mean, rather than having a reference
# level of one group vs. another.
dat$group = ifelse( dat$group == 0, -1, 1 )
table( dat$group )
dat$sex = ifelse( dat$sex == 0, -1, 1 )
table( dat$sex )

# Warning: Turning group into a factor will MESS UP the interaction
# generation lower down.  So we leave it as a 0/1 binary dummy 
# variable which we can multiply by age for our interaction effects.
# See for yourself: uncommment this line and watch things fall apart!
# dat$sex = factor( dat$sex )
# dat$group = factor( dat$group )
# (What happens is if it is a factor, then multiplying doesn't multiply the
# number but instead the factor level of 1 or 2).


# Aside: R does some weird stuff sometimes with interaction terms
if ( FALSE ) {
    M.aside.A = lmer( vocab ~ 0 + age12sq + age12sq:as.factor(group) + (0 + age12 + age12sq|pers), data=dat )
    display( M.aside.A )
    fixef( M.aside.A )

    M.aside.B = lmer( vocab ~ 0 + age12sq + age12sq:as.numeric(group) + (0 + age12 + age12sq|pers), data=dat )
    display( M.aside.B )
    fixef( M.aside.B )
    # Note warning of "rank difficient matrix" in first run
}


nrow( dat )

# Simplified model is picked, now fit on full data
M4 = lmer( vocab ~ 0 + age12sq + age12sq:group + age12sq:sex + age12sq:logmom + age12sq:sex:group + age12sq:logmom:group + (0 + age12 + age12sq|pers), 
           data=dat )
display( M4 )


# Drop our three-way interactions
M5 = lmer( vocab ~ 0 + age12sq + age12sq:group + age12sq:sex + age12sq:logmom  + (0 + age12 + age12sq|pers), data=dat )
display( M5 )
# we get some warnings.

# we should not drop... so let's back up and use model 4
lrtest( M4, M5 )



##
## Plotting some curves for the individual kids for all our groups
##

display( M4 )

dat$vocab.hat = predict( M4 )

ggplot( dat, aes( x=age, vocab.hat, group=pers, col=as.factor(group) ) ) +
  geom_line( ) +
  geom_point( ) +
  labs( x="Age of Kid (months)", y="Vocab of Kid" )



# Make the individual growth curves for each kid based on their
# random effect estimates.
# What we are doing is making a range of ages for each kid and then predicting
# the outcome for each age.  We then collect this together and plot it.
coefs = coef( M4 )$pers
head( coefs )

# our list of ages to evaluate at (made above)
ages


# Now we use merge to make our big matrix of multiple ages for each kid
# (Cleverness!)

kids = dplyr::select( dat, pers, sex, group, logmom )
head( kids )
kids = unique( kids )
nrow( kids )
agelist = data.frame( age=ages )
nrow( agelist )
kids = merge( kids, agelist )
head( kids )
nrow( kids )
table( kids$pers )

kids = mutate( kids, age12 = age - 12,
               age12sq = age12^2 )

kids$vocab.hat = predict( M4, newdata=kids )

# Make our factors
kids = mutate( kids, group = factor( (1+group)/2 ),
               sex = factor( sex, levels=c(-1, 1), labels=c("boy","girl") ) )

# And finally our extended lines!
ggplot( data=kids, aes( x=age, y=vocab.hat, group=pers, col=group ) ) +
    geom_line()


# Look at growth by gender
ggplot( data=kids, aes( x=age, y=vocab.hat, group=pers, col=sex ) ) +
    geom_line()





##
## Looking at explained variance of our models
##


# Calculating variance explained
M4 = lmer( vocab ~ 0 + age12sq + age12sq:group + age12sq:sex + age12sq:logmom + age12sq:sex:group + age12sq:logmom:group + (0 + age12 + age12sq|pers), 
           data=dat )
display( M4 )

M3.full = lmer( vocab ~ 0 + age12sq + age12sq:group + (0 + age12 + age12sq|pers), data=dat )

display( M3.full )

screenreg( list( M3=M3.full, M4=M4 ) )


# The variances
VarCorr( M3.full )$pers
varsM3 = diag( VarCorr( M3.full )$pers )

VarCorr( M4 )$pers
varsM4 = diag( VarCorr( M4 )$pers )

varsM3
varsM4

# Now calculate the ratio
(varsM3 - varsM4) / varsM3


# Testing: the predictors matter!
lrtest( M3.full, M4 )
# or 
anova( M3.full, M4 )










