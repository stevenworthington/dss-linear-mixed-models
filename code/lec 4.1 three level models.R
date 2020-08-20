##
## Code for three-level models
##
##

# Note: Some people say require()---same thing as library()
require(stringr)
require(lme4)
require(nlme)
require(arm)
require(lmerTest)
library( tidyverse )
library( texreg )

#### Load and prepare the data ####
wide_dat <- read.csv('dat_y4.csv')

wide_dat$schid <- str_trim(wide_dat$sch)

wide_dat$poverty_school <- factor(wide_dat$poverty_school, levels = 1:2, labels = c('Not poverty', 'Poverty'))
head( wide_dat )

table( wide_dat$grade_fall2013 )

wide_dat = filter( wide_dat, grade_fall2013 == 3 )

dat = gather( wide_dat, s3, f3, s4, f4, key="wave", value="read" )

dat = select( dat, id, schid, wave, read, gender_new, poverty_school, lep_new )
dat = rename( dat, gender = gender_new )

dat$time = 0
dat$time[ dat$wave == "s3" ] = 3
dat$time[ dat$wave == "f4" ] = 12
dat$time[ dat$wave == "s4" ] = 15

head( dat )
sample_n( dat, 15 )

dat = mutate( dat, time.c = time - mean( time ) )

dat = rename( dat, ell_new = lep_new )

table( dat$time )

dat = na.omit( dat )

##### Basic three level models  #####


# Three level
mod <- lmer(read ~ 1 + time + (1 + time|id) + (1 + time|schid), data = dat)
display(mod)

mod_red <- lmer(read ~ 1 + time + (1 + time|id) + (1|schid), data = dat)

# Do we need the extra slope by school?
anova(mod, mod_red)

screenreg(list(mod, mod_red))

head( dat )


#set.seed( 1019 )
#sch.4 = sample( unique( dat$sch ), 4 )
#sch.4

# We are cherry picking extreme schools for illustration
sch.4 = c( "920560", "320310", "320315", "920320" )

ranefs = coef( mod )$sch
ranefs[ sch.4, ]
summary( ranefs$`(Intercept)` )
summary( ranefs$time )

dat.4 = filter( dat, schid %in% sch.4 )
nrow( dat.4 )

# Reduce number of students to make plot more clear
stu.id = sample( unique( dat.4$id ), 100 )
dat.4 = filter( dat.4, id %in% stu.id )

# Raw data
ggplot( dat.4, aes( time, read ) ) +
  facet_wrap( ~ schid ) +
  geom_line( aes( group=id ), alpha=0.5 ) +
  geom_smooth( method="lm", se=FALSE, col="red" ) +
  coord_cartesian( ylim=c(120, 250 ) )
ggsave( "lec 4.1 raw student.pdf", width=5, height=3.25 )

# Predicted growth lines
dat.4$read.hat = predict( mod, newdata=dat.4 )

# Predicted school growth lines
dat.sch.4 = dplyr::select( dat.4, schid, time ) %>%
  unique()
dat.sch.4
dat.sch.4$id = -1
dat.sch.4$read.hat = predict( mod, newdata=dat.sch.4, allow.new.levels=TRUE )

ggplot( dat.4, aes( time, read.hat ) ) +
  facet_wrap( ~ schid ) +
  geom_line( aes( group=id ), alpha=0.5 ) +
  geom_line( data=dat.sch.4, col="red", lwd=1 ) +
  coord_cartesian( ylim=c(120, 250 ) )

ggsave( "lec 4.1 smooth student.pdf", width=5, height=3.25 )




#### Adding 2nd level covariates ####


# Looking at ELL

ell_mod <- lmer(read ~ 1 + time*ell_new +
                  (1 + time|id) + 
                  (1 + time * ell_new|schid), 
                data = dat)


display( ell_mod )


screenreg( list( ell_mod ) )




#### Adding 3nd level covariates ####

pov_mod <- lmer(read ~ time * ell_new * poverty_school + 
                  (time|id) + 
                  (time * ell_new|schid), data = dat)

display( pov_mod )


screenreg( list( pov_mod ) )


# pooling coefficients

pov_modB <- lmer(read ~ time * ell_new * poverty_school + 
                  (1 + time|id) + 
                  (1|schid), data = dat)


# More restrictions
pov_modC <- lmer(read ~ 1 + time + ell_new +
                  poverty_school + 
                  time:poverty_school +
                  (1 + time|id) + 
                  (1 + time + ell_new|schid), 
                data = dat)


screenreg( list( pov_mod, pov_modB, pov_modC ) )
