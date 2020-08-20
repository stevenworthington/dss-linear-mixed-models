##
## "Toenail" case study (from RH&S text).
##
## This illustrates a longitudinal growth model context with binary outcomes.
## So we use generalized multilevel modeling.
##

library( tidyverse )
library( lme4 )
library( lmerTest )
library( arm )
library( foreign )

options( digits=3 )

##
## Load data
##

toes = read.dta( "toenail.dta" )
head( toes )

table( toes$treatment )
toes$Tx = factor( toes$treatment, levels=c(0,1),
                            labels=c("Terbinafine", "Itraconazole") )
table( toes$Tx )

# The "mosaic" package has some nice functions for simple EDA.
# My favorite is "favstats" which gives you summary statistics by group.
# VERY USEFUL!
# (The mosaic::favstats name says "favstats in mosaic package"---and then we
# don't have to load the package which is good since it can mess up other
# stuff.)
mosaic::favstats( outcome ~ Tx, data=toes )

##########################################################################################
##
#### Looking at missingness patterns ####
##
# #########################################################################################

# how many reads per patient?
table( table( toes$patient ) )

# Looking at how data-hacking can be very flexible
# This code, for each patient,
# makes a string of "." and then replaces all
# dots with an "X" if we have data for that visit number.
# (Optional code)
summarise.patient = function( patient ) {
    pat = rep( ".", 7 )
    pat[ patient$visit ] = 'X'
    paste( pat, collapse="" )
}

# This groups by patient.  The do() command is then called on each chunk of our dataframe.
# The "." means "the chunk" (it is a pronoun, essentially).  It creates a list of the output
# (so a list of character vectors).
# The unnest() takes our character vector out of this list made by "do"
miss = toes %>% group_by( patient ) %>%
    do( pattern = summarise.patient(.) ) %>%
    unnest()

head( miss )

sort( table( miss$pattern ), decreasing=TRUE )

miss = miss %>% group_by( pattern ) %>%
    summarise( n=n() )
miss = arrange( miss, -n )
miss

# percent missing data (224 complete cases)
224 / sum( miss$n )




##
## Looking at some patterns of detachment
##

# we don't want to fiddle with month, so we drop it.
toes2 = toes
toes2$month = NULL

dat.wide = reshape( toes2, direction="wide", v.names="outcome",
                    idvar="patient", timevar = "visit" )
head( dat.wide )


# looking at missing data with 'mice' package
# (This is a package that does missing data imputation with "multiple imputation chained equations")
library( mice )
md.pattern( dat.wide )



# Another example of generating our missingness pattern thing, but slightly fancier
# This function takes the visits and outcomes and puts a 1 or 0 if there is an
# outcome and a dot if missing.
make.pat = function( visit, outcome ) {
    pat = rep( ".", 7 )
    pat[ visit ] = outcome
    paste( pat, collapse="" )
}

# call our function on all our patients.
outcomes = toes %>% group_by( patient ) %>%
    summarise( tx = Tx[[1]],
               num.obs = n(),
               num.detach = sum( outcome ),
               out = make.pat( visit, outcome ) )

head( outcomes, 20 )

# how many folks have no detachments?
table( outcomes$num.detach )

163 / nrow(outcomes)

# how many always detached?
sum( outcomes$num.detach == outcomes$num.obs )

16 / nrow(outcomes)



##########################################################################################
##
#### Looking at proportion with toe issues  ####
## (The marginal probabilities)
##
##########################################################################################

sumstat = toes %>% group_by( visit,Tx ) %>%
    summarise(   prop = mean(outcome),
                 mnmonth=mean(month),
                 n=length(outcome) )
head( sumstat )

ggplot( data=sumstat, aes( x=visit, y=prop, col=Tx ) ) +
    geom_point() + geom_line()


# Mean month cause each visit was at a slightly different time and so we can average
# across to get the general time of the visit (notice visits closer together earlier
# in study)
ggplot( data=sumstat, aes( x=mnmonth, y=prop, col=Tx ) ) +
    geom_point() + geom_line()



##
## Model without random effects
##

# This is bad without robust standard errors since we don't take into account
# individual nesting and correlation within individuals.
M0 = glm( outcome ~ Tx * month,
          family=binomial,
          data=toes )
display( M0 )

# our odds ratios and odds ratio multipliers
exp( coef( M0 ) )


# predictions (with no random effects)
# here we predict the probability for each observation given the covariates (which is
# just Tx and month).
toes$preds = predict( M0, type = "response" )

# Print the predicted Tx curves (marginal across people)
ggplot( data=toes, aes(x=month, y=preds, col=Tx ) ) +
    geom_line( lwd=2 )

# plotting two plots on top of each other to see the raw proportions
# as well as the predicted curve from our model
ggplot( data=toes, aes( x=month, y=preds ) ) +
    facet_wrap( ~ Tx ) +
    geom_line( size=1, col="green" ) +
    geom_line( data=sumstat, aes( x=mnmonth, y=prop ) ) +
    geom_point( data=sumstat, aes( x=mnmonth, y=prop ), size=3 )



########################################################################################
##
#### Fitting random-intercept model  ####
##
########################################################################################

M1 = glmer( outcome ~ Tx * month + (1|patient),
            family=binomial,
            data=toes )


display( M1 )

# odds ratio formulation
exp( fixef( M1 ) )

# odds multiplier per month for the Tx=1 group
0.67 * 0.872


# percent decrease
100 * (0.67 - 1)
100 * (0.67 * 0.872 - 1)


# Looking at confidence intervals for our parameters
# (takes a while to run---lots of optimization.)
cis = confint( M1 )
cis

# put them into multiplicitave factors on the log-odds
exp( cis[-1,] )


##
## Comparing the two models
##

coef( M0 )
fixef( M1 )

exp( coef( M0 ) )
exp( fixef( M1 ) )



########################################################################################
##
#### Exploring the random effects  ####
##
########################################################################################


# each row is an individual patient
# This gives their parameters that give their growth curve.
head( coef( M1 )$patient )

# Now many patients?
nrow( coef( M1 )$patient )

# Just the random effect part
head( ranef( M1 )$patient )
mosaic::favstats( ranef( M1 )$patient[,1] )

# What are our random intercepts?  These are NOT normally distributed.
qplot( ranef( M1 )$patient[,1] )



##
## Plotting individual growth curves with the corresponding
## actual data
##


# predictions from our random intercept model
toes$pred.RI = predict( M1, type="response" )
head( toes )

pid = sample( unique( toes$patient ), 20 )
toe.sub = subset( toes, patient %in% pid )

plt <- ggplot( data=toe.sub, aes( x=month, y=pred.RI, col=Tx ) ) +
    facet_wrap( ~ patient ) +
    geom_line( lwd=1 ) +
  geom_point()
plt

# now add the raw data as an overlay
plt + geom_point( aes(y=outcome), col="black" )




########################################################################################
##
#### Making smooth curves of the predicted probabilities ####
##
## Here we use our estimated coefficients to calculate probabilities by hand.
##
########################################################################################

median.kids = expand.grid( patient = -1,
                       month = seq( min( toes$month ), max( toes$month), length.out=100 ),
                       Tx = levels( toes$Tx ) )

median.kids$prob = predict( M1, newdata=median.kids, allow.new.levels=TRUE, type="response" )

# Aside: Let's make our plots the way we want!
library( ggthemes )
my_t = theme_few() + theme( legend.position="bottom", 
                            legend.direction="horizontal", legend.key.width=unit(1,"cm") )
theme_set( my_t )


# compare Txs
ggplot( data=median.kids, aes(x=month, y=prob, col=Tx) ) +
    geom_line()


#### Individual plots for each kid #####
my_16 = sample( toes$patient, 16 )
pats = expand.grid( patient = my_16,
                        month = seq( min( toes$month ), max( toes$month), length.out=100 ),
                        Tx = levels( toes$Tx ) )

pats$prob = predict( M1, newdata=pats, allow.new.levels=FALSE, type="response" )

ggplot( data=pats, aes( x=month, col=Tx ) ) +
    facet_wrap( ~ patient ) +
    geom_line( aes( y=prob ) )



##
## Now let's look at different quantiles
## of the random intercept and see how the curve changes
##
## To do this we need to make a function to calculate probabilities using our model since we have to
## specifically set our random effects.
##


# what range of random effects do we have?
u.hats = ranef( M1 )$patient[,1]
mosaic::favstats( u.hats )

# or read off our fitted model
display( M1 )
sqrt( VarCorr( M1 )$patient )

# get the deciles
us = qnorm( seq( 0.1, 0.9, by=0.10 ), mean=0, sd=4.56 )
us


# This function will use our model to predict probability of detachment
# for a sequence of times (so months is a list of numbers)
# Z = 1 means Tx is Itraconazole
# u is our random intercept term.
calc.probs = function( months, Z, M1, u = 0 ) {
    fe = fixef( M1 )
    lin.pd = fe[[1]] + fe[[2]] * Z + fe[[3]] * months + fe[[4]] * Z * months + u
    invlogit( lin.pd )
}

# We TEST OUR CODE by trying it out on a pretend person
# Here is probability 0, 6, and 12 months out for someone with a random intercept
# 1 sd below average and who gets the high level of Tx
round( calc.probs( c(0,6,12), 1, M1, u=-1 ), digits=3 )


# Now make 20 typical patients with different random intercepts at each decile.
# One treated, one control
pats = expand.grid( u = us,
                    Tx = levels( toes$Tx ),
                    month = seq( min( toes$month ), max( toes$month), length.out=50 ) )

quantile.probs = pats %>% group_by( u, Tx ) %>%
    mutate( prob = calc.probs( month, Tx[[1]] == "Itraconazole", M1, u[[1]] ) )

head( quantile.probs )



# plot!
# (We reuse our median probs to get our heavy black median line.)
ggplot( data=quantile.probs, aes( x=month, y=prob ) ) +
    facet_wrap( ~ Tx ) +
    geom_line( aes( group=u ), col="red" ) +
    geom_line( data=median.kids, size=2 )


