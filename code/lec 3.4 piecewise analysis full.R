##
## This script loads and analyzes (with piecewise linear growth models) the
## North Carolina ITBS dataset.
##
## McIntyre, Miratrix
## 

require(stringr)
require(lme4)
require(nlme)
require(arm)
require(lmerTest)
library( tidyverse )
require(texreg)


##### Load the data and make our time variables  ######

wide_dat <- read.csv( 'NC_ITBS_data.csv' )
head( wide_dat )


# Here we make our time variables in wide format, and then reshape. (This code
# inherited from Joe M., I would have gone to long and _then_ made my time
# variables. But code here for reference.)
wide_dat$t1 <- 0
wide_dat$t2 <- 3
wide_dat$t3 <- 12
wide_dat$t4 <- 15

# Months of school at each wave
wide_dat$school1 <- 0
wide_dat$school2 <- 1
wide_dat$school3 <- 10
wide_dat$school4 <- 11

# Months of summer at each wave
wide_dat$summer1 <- 0
wide_dat$summer2 <- 2
wide_dat$summer3 <- 2
wide_dat$summer4 <- 4

# Here is a scary reshape to make our data in long form!
# We are reshaping multiple variables.
dat <- reshape(wide_dat, direction = 'long', varying = list(c('s3', 'f3', 's4', 'f4'),
                                                            c('t1', 't2', 't3', 't4'),
                                                            c('school1', 'school2', 'school3', 'school4'),
                                                            c('summer1', 'summer2', 'summer3', 'summer4')),
               v.names = c('read', 'time', 'school', 'summer'))

keeps <- c("id", "district", "sch", "poverty_school", "grade_fall2013", "frl_new", "lep_new", "gender_new", "time", "read", "school",
           "summer")

dat <- dplyr::select( dat, keeps )
head( dat )

# Take our cohort
dat <- filter(dat, grade_fall2013 == 3)

# Drop missing data (all entries with missing read scores tossed).
dat <- dat[complete.cases(dat$read), ]



##### Preliminary data exploration  ####


# Get average read for each time point and plot
col_dat <- dat %>% group_by( time ) %>%
    summarize( read = mean(read) )

ggplot(data = dat, mapping = aes(x = time, y = read, group = time)) + geom_boxplot()

ggplot(data = dat, mapping = aes(x = time, y = read)) +
    geom_line(aes(group = id), alpha = .1) +
    geom_line(data = col_dat, col = 'red', lwd = 2) + geom_point( data = col_dat, col="red", size = 4 )


# Look at 16 kids
samp <- as.character(sample(dat$id, 16))

ggplot(data = filter(dat, id %in% samp), mapping = aes(x = time, y = read)) + geom_line() + geom_point() + facet_wrap( ~ id, ncol = 4)
ggsave( "lec3.4_individual.png" )

ggplot(data = filter(dat, id %in% samp), mapping = aes(x = time, y = read)) +
    geom_line() + geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    facet_wrap( ~ id, ncol = 4)
ggsave( "lec3.4_individual_smooth.png" )


# Number of students
length( unique( dat$id ) )



##
## Preliminary linear growth model
##

# The Linear growth model
mod <- lmer(read ~ time + (time|id), data = dat)

k <- vcov(mod)
k

display(mod)
screenreg(mod)
# confint(mod)


dat$gender <- factor(dat$gender_new, levels = 1:2, labels = c('female', 'male'))

mod_gender <- lmer(read ~ time*gender + (time|id), data = dat)
display(mod_gender)
screenreg(mod_gender)

k <- vcov(mod_gender)
k

sqrt(diag(k)) # standard errors for the terms

# Here we compare getting a contrast by combining terms via our matrix
# multiplication to making or gender level have a different baseline (to examine
# growth rate of boys).
dat$gender_rev <- relevel(dat$gender, ref = 'male')
mod_gender2 <- lmer(read ~ time*gender_rev + (time|id), data = dat)
display(mod_gender2)

# using orig model with contrasts
sqrt(c(0, 1, 0, 1) %*% k %*% c(0, 1, 0, 1))

# Using releveled model
se.fixef(mod_gender2)



##
## Piecewise linear growth models
##

mod_school_summer <- lmer(read ~ 1 + school + summer + (1 + school + summer|id), data = dat)
display(mod_school_summer)
# confint(mod_school_summer)

mod_time_summer <- lmer(read ~ 1 + time + summer + (1 + time + summer|id), data = dat)
display(mod_time_summer)

screenreg(list(mod_school_summer, mod_time_summer))




# Making our growth curves for our students.  

# Make a matrix so all students have all time points for the times when we
# switch from summer to school, or back

studs = expand.grid( id = unique( dat$id ),
                     time = c( 0, 1, 3, 13, 15, 16 ) )

# Calculate months of summer for each time
studs$summer = 0
studs$summer[ studs$time == 1 ] = 0
studs$summer[ studs$time == 3 ] = 2
studs$summer[ studs$time == 13 ] = 2
studs$summer[ studs$time == 15 ] = 4
studs$summer[ studs$time == 16 ] = 4

# Also make a new student so we can have overall population line
nstuds = studs
nstuds$id = -1
nstuds = unique( nstuds )
nstuds

# Now predicted scores
studs$predicted = predict( mod_time_summer, newdata=studs )
nstuds$predicted = predict( mod_time_summer, newdata=nstuds, allow.new.levels=TRUE )

ggplot(data = studs, mapping = aes(x = time, y = predicted, by = id)) + 
  geom_line(lty = 2, alpha = .5, lwd = .5) +
  geom_line( data=nstuds, col="red", lwd=2 )


# Just look at 30 students
set.seed(1211198)
sampID = sample( unique( dat$id ), 30 )
stud.30 = filter( studs, id %in% sampID )

ggplot(data = stud.30, mapping = aes(x = time, y = predicted, by = id, col=id)) +
  geom_line( alpha = .5, lwd = .5) +
  geom_point() +
  geom_line( data=nstuds, col="red", lwd=2 )


ggsave( "lec3.4_individual_piecewise.png", width = 4.5, height=4.5 )





