

library( foreign )
library( tidyverse )
library( lme4 )
library( arm )

# THAILAND EDUCATION DATA The Thailand education data are one of the example data
# sets that are included with the software HLM (also in the student version of
# HLM). They are discussed at length in 2 Appendix: Data and Stories the HLM
# user’s manual. 

# They stem from a large survey of primary education in Thailand (Raudenbush &
# Bhumirat, 1992). The outcome variable is dichotomous, an indicator whether a
# pupil has ever repeated a class (0= no, 1= yes). The explanatory variables are
# pupil gender (0= girl, 1= boy), pupil pre-primary education experience (0 =no,
# 1= yes) and the school’s mean SES.
#
# There are 8582 cases in the file THAIEDUC, but school mean SES is missing in
# some cases; there are 7516 pupils with complete data.


##### Read and prepare data ####
# read student-level data 
dat = read.spss( "thaieduc.sav", to.data.frame=TRUE )
head( dat )
nrow( dat )

dat = subset( dat, !is.na( MSESC ) )

# make sure schools are factors
dat$SCHOOLID = as.factor( dat$SCHOOLID )

nrow( dat )


qplot( as.numeric( table( dat$SCHOOLID ) ), main="# Students in each school", binwidth=1 )


# percent retention is 14%
mosaic::tally( dat$REPEAT, format = "percent" )

head(dat)
sdat = dat %>% group_by( SCHOOLID ) %>%
  summarize( n = n(), 
             n.rep = sum( REPEAT=="yes" ), 
             p.rep = n.rep / n, 
             sd.ses=sd(MSESC),
             MSESC=MSESC[[1]] )

# all the MSESC's should have no variation within school if original datafile
# is not corrupted.  Here we check:
all( sdat$sd.ses == 0 )

# How retention correlates with mean SES (centered)
# This is a raw plot, not adjusted via modeling.
qplot( MSESC, p.rep, data = sdat )

# Almost centered; probabily not exact 0 due to dropping some schools with missing data.
mean( sdat$MSESC )
mean( dat$MSESC )

qplot( sdat$p.rep )

skimr::skim( sdat$MSESC )
qplot( sdat$MSESC )



#### Fit the no-covariates model  ####


M0 = glmer( REPEAT ~ 1 + (1|SCHOOLID), data=dat,
            family=binomial(link="logit") )
display( M0 )

# Does not converge.  Model is too simple and we should not
# interpret it.

# But onward, let's look at variation

# exploring the variance terms
VarCorr( M0 )
as.data.frame( VarCorr( M0 ) )

# getting our fixed intercept and sqrt_tau_00
inter = fixef( M0 )

sqrt_tau_00 = as.data.frame( VarCorr( M0 ) )$sdcor
sqrt_tau_00
    


#### OPTIONAL CODE:  A fun plot example ####

## Just for fun!  This uses the "base plot" package of R (we have not covered this in this class.)
##
## This code illustrates making two windows not the same size so you can
## have nicely aligned plots (in this case the distribution and the predicted
## probability)
##

# Number in matrix number your screens
# Each row and column has a width and height
par( oma=c(0,0,0,0) )
nf <- layout(matrix(c(1,2), nrow=2), widths=c(6), heights=c(3,1), TRUE)

# If you want to see your layout, this command will create a chart labelling it clearly.
# It is for debugging
layout.show(nf)

# First plot, goes in position one, so lower left. This is our large plot.

# Further Reading:
# help(layout)
# help(par)
# help(matrix)

par( mar=c(0,3,0.5,0.5), mgp=c(1.8,0.8,0) )

rand.ints = seq( inter - 3*sqrt_tau_00, inter + 3*sqrt_tau_00, length.out=100 )
probs = invlogit( rand.ints )

plot( probs ~ rand.ints, ylim=c(0,1), ylab="Probability", xlim=c(-7,2), xlab="", xaxt="n", bty="n",lwd=5, type="l" )
abline( v=inter, col="red", lwd=3 )
abline( v=inter + c(-2,2)*sqrt_tau_00, lwd=3, lty=2, col="red" )
abline( h= invlogit( inter + c(-2,-1,0,1,2)*sqrt_tau_00 ), lty=2 ) 

# add density curve
par( mar=c(3,3,0,0.5), mgp=c(1.8,0.8,0) )
plot( rand.ints, dnorm( rand.ints, mean=inter, sd=sqrt_tau_00 ), xlim=c(-7,2), type="l", bty="n",
      xlab="Random Intercept of School", ylab="", yaxt="n" )
abline( v=inter, col="red", lwd=3 )
abline( v=inter + c(-2,2)*sqrt_tau_00, lwd=3, lty=2, col="red" )

# +/- 1 SE
invlogit( inter + c(-2,-1,0,1,2)*sqrt_tau_00 )



#### Fit the random intercept model with covariates  ####


M1 = glmer( REPEAT ~ 1 + SEX + PPED + MSESC + (1|SCHOOLID), 
            data=dat,
            family=binomial(link="logit") )
display( M1 )
# Note: Ignore the residual std dev---not meaninful here.  We have no such term!

summary( M1 )
# Note summary does not list this residual std. dev.  display() is a bit busted?

# To interpret we exponetiate to get our multiplicative factors to the odds
exp( fixef( M1 ) )


sd( sdat$MSESC )

# This shows the change in odds for a 1 SD increase in MSESC
exp( sd( sdat$MSESC ) * fixef(M1)["MSESC"] )


# Looking at predict method
dat$eta.hat = predict( M1 )
sample( dat$eta.hat, 7 )

dat$mu.hat = predict( M1, type="response" )
sample( dat$mu.hat, 7 )



# Do we need an interaction of gender and PPED?
M2.c = glmer( REPEAT ~ 1 + SEX * PPED + MSESC + (1|SCHOOLID), data=dat,
              family=binomial )
anova( M1, M2.c )
# Nope!



##### Using predict to get all our school estimates for the four types of kid ######

ndat = expand.grid( SCHOOLID = unique( dat$SCHOOLID ),
                    SEX = unique( dat$SEX ),
                    PPED = unique( dat$PPED ) )
head( sdat )
head( ndat )

# Add in the school mean ses
ndat = merge( ndat, 
              dplyr::select( sdat, SCHOOLID, MSESC ),
              by="SCHOOLID" )
head( ndat )

ndat$prob = predict( M1, newdata=ndat, type="response" )

# We now have the predicted retention rate for each school for each of our 4
# groups. These rates have been shrunk via emperical bayes to handle small
# sample sizes, etc. We are also assuming the relative rates between groups is
# the same (in the log-odds/linear predictor space).
head( ndat )


# We can make a wide dataset, with one column for each of our 4 groups, as follows
ndat$group = paste( ndat$SEX, ndat$PPED, sep="." )
table( ndat$group )
ndat.w = ndat %>% dplyr::select( SCHOOLID, MSESC, group, prob) %>%
  spread( group, prob )
head( ndat.w )


## 
#### Plot: Looking at how much schools vary  ####
##

ggplot( ndat.w, aes( MSESC, boy.no ) ) + 
  geom_point() + 
  geom_smooth( method="lm", se=FALSE, col="red" ) +
  labs( y="Probability of retention", x = "School Mean SES")


# if you want to plot girls, you can. 
# They are a constant scaling down from boys
# (We are dropping the 0 terms from our equation)
ggplot( ndat.w, aes( MSESC, boy.no ) ) + 
  geom_point() + 
  geom_point( aes( y = girl.no ), col="green" ) +
  geom_smooth( method="lm", se=FALSE, col="red" ) +
  labs( y="Probability of retention", x = "School Mean SES") +
  geom_smooth( aes( y = girl.no ), method="lm", se=FALSE, col="darkgreen" )


# Alternate, but harder to compare
# if you want to plot girls, you can. 
# They are a constant scaling down from boys
# (We are dropping the 0 terms from our equation)
ggplot( ndat, aes( MSESC, prob ) ) + 
  facet_grid( SEX ~ PPED ) +
  geom_point() + 
  geom_smooth( method="lm", se=FALSE, col="red" ) +
  labs( y="Probability of retention", x = "School Mean SES")
  

# Final alternate, weight each of the 4 groups equally
mosaic::tally(  dat$PPED, format="percent" )
mosaic::tally(  dat$SEX, format="percent" )

agg.ndat = ndat %>% group_by( SCHOOLID, MSESC) %>%
  summarise( prob = mean( prob ) )
ggplot( agg.ndat, aes( MSESC, prob ) ) + 
  geom_point() + 
  geom_smooth( method="lm", se=FALSE, col="red" ) +
  labs( y="Probability of retention", x = "School Mean SES")

# Compare to raw data: make side-by-side
sdat2 = merge( sdat[c("SCHOOLID","p.rep")], agg.ndat, by="SCHOOLID" )
head( sdat2 )
sdat2 = gather( sdat2, p.rep, prob, key="estimate", value="prob" )
head( sdat2 )
table( sdat2$estimate)
ggplot( sdat2, aes( MSESC, prob ) ) +
  facet_wrap( ~ estimate ) +
  geom_point() +
  geom_smooth( se=FALSE )
# This is just to show how the Emperical Bayes shrinkage adjusts our point
# estimates a bit.  We see some come up from 0 due to shrinkage, primarily.  The
# extreme high ones come down a bit as well.


## 
#### Illustrating the problem with the linear model  ####
##

M1.lin = lmer( as.numeric(REPEAT =="yes") ~ 1 + SEX + PPED + MSESC + (1|SCHOOLID), data=dat )
display( M1.lin )

# our predictions for all students
pd = predict( M1.lin )

# see some are below 0, indicating negative chance of retention.
skimr::skim( pd )

# about 5% of our predictions are negative!
mean( pd <= 0 )




##
##### Plot: Making girl and boy curves for a 0 mean SES school  ####
##

## This code makes a plot that shows how the retention rate for a school changes
## with different values of the school random intercept.  It allows us to assess
## how much the random effects matter.

# Approach 1(a): Use prior predict() and merge with the estimated random intercept
#  Problem: Mean school SES messes up our smooth lines

coefs = coef( M1 )$SCHOOLID
head( coefs )
coefs$SCHOOLID = rownames( coefs )

ndat2 = merge (ndat, coefs, by="SCHOOLID" )
head( ndat2 )

ggplot( ndat2, aes( `(Intercept)`, prob, color=SEX, lty=PPED ) ) +
  geom_line()



# Approach 1(b): As above, except we will zero out mean SES in our predictions

head( ndat )
ndat2 = ndat
ndat2$MSESC = mean( sdat$MSESC )
ndat2$prob = predict( M1, newdata=ndat2, type="response" )

coefs = coef( M1 )$SCHOOLID
head( coefs )
coefs$SCHOOLID = rownames( coefs )

ndat2 = merge (ndat2, coefs, by="SCHOOLID" )
head( ndat2 )

# First we look at the range of random intercepts (with the fixed
# effect added in)
sqrt_tau_00 = as.data.frame( VarCorr( M1 ) )$sdcor
sqrt_tau_00

int = fixef( M1 )[[1]]
range( coefs$`(Intercept)`)
bars = fixef( M1 )[[1]] + c(-2,0,2)*sqrt_tau_00
bars
qplot( coefs$`(Intercept)` ) +
  geom_vline( xintercept=bars ) +
  geom_vline( xintercept=mean(coefs$`(Intercept)`), col="red" )
# Note how many of our random intercepts are positive.  We don't have huge
# negative ones.  We have skew in our emperical random effect distribution.
# (This is fine.)

ggplot( ndat2, aes( `(Intercept)`, prob, color=SEX, lty=PPED ) ) +
  geom_line() +
  geom_vline( xintercept=bars )





# Approach 2 (Optional): Do the math by hand

display( M1 )
fixef( M1 )

exp( fixef( M1 ) )

# getting our fixed intercept and sqrt_tau_00
fes = fixef( M1 )
sqrt_tau_00 = as.data.frame( VarCorr( M1 ) )$sdcor
sqrt_tau_00

rand.ints = seq( -3*sqrt_tau_00,  3*sqrt_tau_00, length.out=100 )

lin.pred.boy = fes[[1]] + fes[[2]] * 1 + fes[[3]] * 0 + fes[[4]] * 0
lin.pred.girl = fes[[1]] + fes[[2]] * 0 + fes[[3]] * 0 + fes[[4]] * 0
lin.pred.boy.pre = fes[[1]] + fes[[2]] * 1 + fes[[3]] * 1 + fes[[4]] * 0
lin.pred.girl.pre = fes[[1]] + fes[[2]] * 0 + fes[[3]] * 1 + fes[[4]] * 0

# Calculate the probabilities for each category and different values of possible
# random intercepts
probs = data.frame( rand.ints = rand.ints,
                    boy.no = invlogit( rand.ints + lin.pred.boy ),
      girl.no =  invlogit(rand.ints + lin.pred.girl ),
      boy.yes = invlogit( rand.ints + lin.pred.boy.pre ),
      girl.yes = invlogit( rand.ints + lin.pred.girl.pre ) )
head( probs )
probs = gather( probs, boy.no, girl.no, boy.yes, girl.yes, key="group", value="prob" )
head( probs )
probs = separate( probs, group, into=c("SEX", "PPED") )
head( probs )

inter

ggplot( probs, aes( rand.ints, prob, col=SEX, lty=PPED ) ) +
  geom_line() +
  geom_vline( xintercept=inter, col="red", lwd=1 ) +
  geom_vline( xintercept=inter + c(-2,2)*sqrt_tau_00, lwd=1, lty=2, col="red" )



#### Plot: Looking at canonical school, range of MSESC ####


range( dat$MSESC )
curve.dat = expand.grid( SCHOOLID = -1,
                         SEX = unique( dat$SEX ),
                         PPED = unique( dat$PPED ),
                         MSESC = seq( -0.77, 1.49, by=0.03 ) )

curve.dat$prob = predict( M1, newdata=curve.dat, type="response",
                          allow.new.levels =TRUE)

# This plot shows, for a school with a random effect of 0, how much we would
# expect the retention rates to change for different values of school mean SES.
ggplot( curve.dat, aes( MSESC, prob, col=SEX, lty=PPED ) ) +
  geom_line() +
  geom_vline( xintercept=quantile( sdat$MSESC, c(0.1,0.9) ) )
# Now how most of our schools are low SES, with a right tail of high values.

qplot( sdat$MSESC )


#### Fit random slope model allowing prior experience to vary by school ####


M2 = glmer( REPEAT ~ 1 + SEX + PPED + MSESC + (1+PPED|SCHOOLID), data=dat,
            family=binomial )
display( M2 )


library( lmtest )
M1.b = glmer( REPEAT ~ 1 + SEX + PPED + MSESC + (1|SCHOOLID), data=dat,
            family=binomial )
M2.b = glmer( REPEAT ~ 1 + SEX + PPED + MSESC + (1+PPED|SCHOOLID), data=dat,
            family=binomial )
anova( M1.b, M2.b )
# No evidence that we need the PPED as as random slope.  I.e., 
# no evidence that schools have different retention gaps for PPED vs not.




