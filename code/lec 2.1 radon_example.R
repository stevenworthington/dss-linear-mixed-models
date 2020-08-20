##
## Radon example
##
## Code heavily adapted from code for G&H Chapter 12 & 13
## Miratrix, 2019
##

library( arm )
library( tidyverse )


##
## Read & generate county-level summary statistics
##

houses = read.csv( "houses.csv", as.is=TRUE )
counties = read.csv( "counties.csv", as.is=TRUE )
houses = merge( houses, counties, by="cty_id" )

head( houses )

# Let's get some good variable names
houses = rename( houses, radon=y,
                 floor = x )


# What is overall mean across all houses
ybar.houses = mean(houses$radon)


# Calc number of counties
J = length( unique( houses$cty_id ) )
J



###############################################################################################
##
## For each county, calculate mean and SE for the mean (without any pooling or joint model)
##
###############################################################################################

houses$county = as.factor( houses$county )
levels( houses$county )

counties = houses %>% group_by( cty_id, county ) %>%
  summarise(       mean = mean( radon ),
                   var = var( radon ),
                   n = n() )
head( counties )
nrow( counties )

counties = mutate( counties, SE = sqrt( var ) / sqrt( n ) )

ggplot( counties, aes( n, mean ) ) +
  geom_point() +
  geom_errorbar( aes( ymin = mean - SE, ymax = mean + SE ), width=0 ) +
  labs( title= "No Pooling") +
  geom_hline( yintercept = ybar.houses, col="red" )


# Change the x-axis to be prettier.

# We first wiggle the n so we can see all the counties with same number of houses
counties$n.jitter = jitter( counties$n )

# We then use log axis to see the small n areas
ggplot( counties, aes( n.jitter, mean ) ) +
  geom_point() +
  geom_errorbar( aes( ymin = mean - SE, ymax = mean + SE ), width=0 ) +
  labs( title= "No Pooling") +
  geom_hline( yintercept = ybar.houses, col="red" ) +
  scale_x_log10( breaks=c(1,3,10,30, 100 ) )




###############################################################################################
##
## Fixed effect regression
##
###############################################################################################



lm.FE <- lm ( radon ~ 0 + county, data=houses)
display( lm.FE )

coef( lm.FE )

# This code makes our county-level aggregate dataset have the same order of counties as our 
# factor levels in 'county'.
# We need this to make sure that when we add in our coefficients we are getting all the things matched up right.
counties = arrange( counties, county )
all( levels( houses$county ) == as.character( counties$county ) )


FE.results = data.frame( radon = coef( lm.FE ),
                         SE = se.coef( lm.FE ) )
head( FE.results )
head( counties )

# Just checking: check our order
rbind( rownames( FE.results ), as.character( counties$county ) )

FE.results$n.jitter = counties$n.jitter

ggplot( FE.results, aes( n.jitter, radon ) ) +
  geom_point() +
  geom_errorbar( aes( ymin = radon - SE, ymax = radon + SE ), width=0 ) +
  labs( title= "FE Regression") +
  geom_hline( yintercept = ybar.houses, col="red" ) +
  coord_cartesian( ylim=c(-1, 4 ) ) +
  scale_x_log10( breaks=c(1,3,10,30, 100 ) )





###############################################################################################
##
## Random intercept, no predictors
##
###############################################################################################


## Fitting a random intercept (no predictor) model and then
## Generating G&H, Figure 12.1 (b)

library( lme4 )
mn.mod = lmer( radon ~ 1 + (1|county), data=houses )

# Coef is the pooled regression line for each county (so fixed + random). coef()
# gives back an entire set of results for each random effect term (we only have
# one, in general, but this allows for what are called "crossed-effect models"
# later on).
coefs = coef( mn.mod )$county
head( coefs )

# Here are the random effects (without the fixed intercept being added in; note
# negative values).
ranefs = ranef( mn.mod )$county
head( ranefs )

# And fixed effects
fixef( mn.mod )

# And SEs of the random effects
# Warning: we convert to dataframe because arm package leaves it as a matrix
ses = as.data.frame( se.ranef( mn.mod )$county )
head( ses )

# Here we put all our pieces together
coefs = coef( mn.mod )$county
coefs = rename( coefs, radon.hat = `(Intercept)`)
coefs = mutate( coefs, 
                county = rownames( coefs ),
                SE.MLM = ses$`(Intercept)` )
head( coefs )

counties = merge( counties, coefs, by= "county" )
head( counties )
nrow( counties )

ggplot( counties, aes( n.jitter, radon.hat ) ) +
  geom_point() +
  geom_errorbar( aes( ymin = radon.hat - SE.MLM, ymax = radon.hat + SE.MLM ), width=0 ) +
  labs( title= "Multilevel model") +
  geom_hline( yintercept = ybar.houses, col="red" ) +
  coord_cartesian( ylim=c(-1, 4 ) ) +
  geom_hline(yintercept=ybar.houses, col="red") +
  geom_hline(yintercept=fixef(mn.mod)[[1]], col="blue" ) +
  scale_x_log10( breaks=c(1,3,10,30, 100 ) )





###############################################################################################
##
## Random intercept, Level-1 predictor (of floor)
##
###############################################################################################

# No pooling version
Munpooled = lm( radon ~ 0 + county + floor , data = houses )
coef( Munpooled )
unpooled.lines = data.frame( alpha_j = coef(Munpooled)[1:85],
                             floor = coef(Munpooled)[[86]] )
head( unpooled.lines )

# make county ID by doing string substitution (removing "county" from all our row names)
unpooled.lines$county = gsub( "county", "", rownames(unpooled.lines) )


# With random intercept
M2 = lmer( radon ~ 1 + floor + (1|county), data=houses )
display( M2 )

fit.counties = coef( M2 )$county
head( fit.counties )

# We get back a data frame, one row per county
class( fit.counties )

fit.counties = rename( fit.counties, 
                       alpha_j = `(Intercept)` )
fit.counties$county = rownames( fit.counties )


# Plot all of our individual county lines
ggplot( houses, aes( floor, radon ) ) +
  geom_jitter( width=0.2, height=0 ) +
  geom_abline( data=fit.counties, aes( intercept = alpha_j, slope=floor ), alpha=0.5 )


##
## The following code subsets our data (and all our fit models) to 8 counties
## so we can see individual county stories
##

# Filter to a subset of counties
display8 <- c ( 27073, 27001, 27071, 27041, 27027, 27145, 27123, 27137 ) # counties to be displayed (by id)
counties = counties$county[ pmatch( display8, counties$cty_id ) ]
counties
houses.8 <- filter( houses, cty_id %in% display8 )

# This identifies the county names for our 8 counties.
display8 = unique( houses.8$county )
display8

# Find all our fit county info for our 8 counties
fit.8 = filter( fit.counties, county %in% display8 )
fit.8

# Get our unpoooled lines as well
unpooled.8 = filter( unpooled.lines, county %in% display8 )


# Get our completely pooled model for reference (this is the same for all
# counties)
Mpool = lm( radon ~ 1 + floor, data = houses )
coef( Mpool )


# Plot all of our individual county lines
ggplot( houses.8, aes( floor, radon ) ) +
  facet_wrap( ~ county, nrow=2 ) +
  geom_jitter( width=0.2, height=0 ) +
  geom_abline( data=fit.8, aes( intercept = alpha_j, slope=floor ) ) +
  geom_abline( data=unpooled.8, aes( intercept = alpha_j, slope=floor), col="blue" ) +
  geom_abline( intercept = coef(Mpool)[[1]], slope=coef(Mpool)[[2]], col="red", lty=2 )



