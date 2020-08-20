##
## This script demos how to fit random-slope models with overall and group
## centered covariates on the HS&B dataset
##
## 

# various libraries
library( arm )
library( foreign )
library( tidyverse )
library( lme4 )

# read student data
dat = read.spss( "hsb1.sav", to.data.frame=TRUE )
head( dat )

# read school data
sdat = read.spss( "hsb2.sav", to.data.frame=TRUE )
head( sdat )


dat = merge( dat, sdat, by="id", all.x=TRUE )


# Fitting a baseline model with ses 
M0 = lmer( mathach ~ 1 + ses + (1|id), data=dat )
display( M0 )


#  ** STOP HERE FOR LECTURE ** 





##
## Fitting a random intercept model with both mean ses and ses:
## Y_i = alpha_j[i] + beta SES_i + epsilon_i
## alpha_j = gamma_00 + gamma_01 meanSES_j + u_0j
##

# get our mean SES (it is already computed for this dataset, but someday you
# might want to know how to do it!)
dat = dat %>% group_by( id ) %>%
  mutate( meanses = mean( ses ) )


# fit the model
M1 = lmer( mathach ~ 1 + ses*meanses + (1|id), data=dat )
display( M1 )






##
## Group mean centering SES:
##
## Y_i = alpha_j[i] + beta (SES_i - meanSES_j[i]) + epsilon_i
## alpha_j = gamma_00 + gamma_01 meanSES_j + u_0j
##

# get our group mean centered individual SES
dat = dat %>% group_by( id ) %>%
  mutate( ses.cent = ses - mean( ses ) )


# fit the model
M2 = lmer( mathach ~ 1 + ses.cent + meanses + (1|id), data=dat )

display( M2 )



# Aside: looking at variance decomposition
var( dat$ses )
sum.dat = dat %>% group_by( id ) %>%
  summarise( var.ses = var( ses ),
             mean.ses = mean( ses ) )

var( sum.dat$mean.ses )
mean( sum.dat$var.ses )
var( sum.dat$mean.ses ) + mean( sum.dat$var.ses )
var( dat$ses )
# These last two numbers are close.  If we want exact, we need to actually
# account for different school sizes.



##
## Comparing the models
##

library( stargazer )

stargazer( M0, M1, M2, type="text")




##
## Aside: Decomposition of mean SES and ses impacts when we also have random slopes:
##
## Our model
## Y_i = alpha_j[i] + beta_j[i] SES_i + epsilon_i
## alpha_j = gamma_00 + gamma_01 meanSES_j + u_0j
## beta_j = gamma_10 + gamma_11 meanSES_j + u_1j
##

M0.rs = lmer( mathach ~ 1 + ses + (1 + ses|id), data=dat )
display( M0.rs )

M1.rs = lmer( mathach ~ 1 + ses + meanses + (1 + ses|id), data=dat )
display( M1.rs )

M2.rs = lmer( mathach ~ 1 + ses*meanses + (1 + ses|id), data=dat )
display( M2.rs )


stargazer( M0.rs, M1.rs, M2.rs, type="text")

# How much do our random components vary?
sigma.hat(M0.rs)$sigma$id
sigma.hat(M1.rs)$sigma$id
sigma.hat(M2.rs)$sigma$id








