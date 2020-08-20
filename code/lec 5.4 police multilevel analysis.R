##
## Data from NYC police stops
##
## Multilevel Poisson model example
##

# Our usual libraries
library( tidyverse )
library( mosaic )
library( arm )


##
#### Read in data and clean it  ####
##

stops <- read.table ("frisk_with_noise.dat", sep=" ", header=TRUE, skip = 5)
head( stops )
table( stops$precinct)

# aggregate to get totals across the four crime types
stops = stops %>% group_by(precinct, eth) %>%
    summarize( past.arrests = sum(past.arrests),
               stops = sum( stops ),
               pop = pop[[1]] )

head( stops )
nrow( stops )

# Make ethnic group a proper factor
stops$eth = factor(stops$eth, levels=c(1,2,3), labels = c("Black","Hispanic","White") )
table( stops$eth )

# Make precinct a factor as well
# (If we don't it will be treated as a continuous variable)
stops$precinct = as.factor( stops$precinct )


mosaic::favstats( stops$stops )
mosaic::favstats( stops$past.arrests )

# make info on precincts (aggregate across the ethnic groups)
precinct.frame = stops %>% group_by(precinct) %>%
    summarize( tot.stops = sum( stops ),
                        pop = pop[[1]],
                        tot.past.arrests = sum( past.arrests ) )

nrow( precinct.frame )


##
##### Model fitting  #####
##


# Multilevel analysis of NYC police stops (without overdispersion)

Mall = glmer( stops ~ 1 + eth + (1|precinct),
             offset = log(past.arrests),
             family = poisson(link="log"),
             data=stops )

display(Mall)


head( coef( Mall )$precinct )

df = coef( Mall )$precinct
names( df ) = c("baseline", "Hispanic","White")
df$precinct = rownames( df )
df = merge( df, precinct.frame, by="precinct", all=TRUE )

head( df )

nrow(df)

plot( baseline ~ log(pop), data=df )

plot( baseline ~ log(tot.past.arrests), data=df )




##### Random effects to model over-dispersion ######

## Allowing random intercept per observation to allow for
## over-dispersion

# quasipoisson (old way) with no precinct
fit.3 <- glm (stops ~ eth, family=quasipoisson,
              offset=log(past.arrests),
              data=stops )
display( fit.3 )

# now with precinct.  We still have over dispersion
fit.4 <- glm (stops ~ eth + precinct, family=quasipoisson,
              offset=log(past.arrests),
              data=stops )
display( fit.4 )


# The random intercept way.

# First make a new id for each observation
head( stops )
stops$id = with( stops, paste( precinct, eth, sep="-" ) )
head( stops$id )

# Then fit
fit.4b = glmer( stops ~ 1 + eth + precinct + (1|id),
                offset = log(past.arrests),
                family = poisson(link="log"),
                data=stops )
display( fit.4b)


# alternate way of making individual ids for each row of a dataset
stops$id = 1:nrow(stops)



##### Multilevel models with over-dispersion  #####


# Just have both individual and precinct random intercepts!
Mall = glmer( stops ~ 1 + eth + (1|id) + (1|precinct),
              offset = log(past.arrests),
              family = poisson(link="log"),
              data=stops )

display( Mall )

fes = fixef( Mall )
fes[2:3] = fes[1] + fes[2:3]
fes
mean( fes )
fes - mean( fes )

# The G&H way with ethnicity as a random effect
# (This is a bridge too far, for me personally.)
Mall = glmer( stops ~ 1 + (1|eth) + (1|id) + (1|precinct),
              offset = log(past.arrests),
              family = poisson(link="log"),
              data=stops )

display( Mall )

coef( Mall )$eth
ranef( Mall )$eth
fixef( Mall )

alphas = ranef( Mall )$eth$`(Intercept)`
alpha.bar = mean( alphas )
alpha.adj = alphas - mean( alphas )
alpha.adj
mu.adj = fixef(Mall) + alpha.bar
mu.adj






# Note: quasipoisson doesn't work with glmer.
Mall = glmer( stops ~ 1 + eth + (1|precinct),
              offset = log(past.arrests),
              family = quasipoisson(link="log"),
              data=stops )
# This gives an error!
