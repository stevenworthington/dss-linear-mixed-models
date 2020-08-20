#
# Code for longitudinal data 
#
# Code modified version from Faraway book chapter 9 (see iPac on Canvas)
#
# See chapter to get explanations, etc.
# or just run code line by line to see what you get!
#
# Note: this code uses ggplot.  Book uses another plotting package called 'lattice'
library( arm )
library( ggplot2 )
library( plyr )

# Install package from textbook to get the data by 
# running this line once.
#install.packages( "faraway" )

#
# First example
#

# load the data
library(faraway)
data(psid)
head(psid)

# Make log-transform of income
psid$log_income = with( psid, log( income + 100 ) )
                            
                            
# Look at some plots
psid.sub = subset( psid, person < 21 )
ggplot( data=psid.sub, aes( x=year, y=income ) ) +
    facet_wrap( ~ person ) +
    geom_line()

ggplot( data=psid.sub, aes( x=year, y=log_income, group=person ) ) +
    facet_wrap( ~ sex ) +
    geom_line()


# Simple regression on a single person
lmod <- lm( log_income ~ I(year-78), subset=(person==1), psid)
coef(lmod)

# Now do linear regression on everyone
sum.stat = ddply( psid, .(person), function( dat ) {
    lmod <- lm(log(income) ~ I(year-78), data=dat )
    cc = coef(lmod)
    names(cc) = c("intercept","slope")
    c( cc, sex=dat$sex[[1]] )
} )
head( sum.stat )

plot( slope ~ intercept, data=sum.stat, xlab="Intercept",ylab="Slope")

boxplot( slope ~ sex, data=sum.stat )

# Is rate of income growth different by sex?
t.test( slope ~ sex, data=sum.stat )

# Is initial income different by sex?
t.test( intercept ~ sex, data=sum.stat )


# Fitting our model
library(lme4)
psid$cyear <- psid$year-78
mmod <- lmer(log(income) ~ cyear*sex + age + educ + (cyear|person), psid)
display(mmod)

# refit with the lmerTest library to get p-values
library( lmerTest )
mmod <- lmer(log(income) ~ cyear*sex + age + educ + (cyear|person), psid)
summary(mmod)



##
## Diagnostics
##


# First add our residuals and fitted values to our original data
# (We can do this since we have no missing data so the rows will line up
# correctly)
psid = transform( psid,  resid=resid( mmod ),
                  fit = fitted( mmod ) )
head( psid )

# Here is a qqplot for each sex
ggplot( data=psid ) +
    facet_wrap( ~ sex ) +
    stat_qq( aes( sample=resid ) )

# If you want to add the lines, you have to do a little more work
slopes = ddply( psid, .(sex), function( dat ) {
    y <- quantile(dat$resid, c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- as.numeric( diff(y)/diff(x) )
    int <- y[[1]] - slope * x[[1]]
    c( slope=slope, int=int )
} )
slopes


ggplot( data=psid ) +
    facet_wrap( ~ sex ) +
    stat_qq( aes( sample=resid ) ) +
    geom_abline( data=slopes, aes( slope=slope, intercept=int ) )



# This is doing it from the lattice pacage
library( lattice )
qqmath(~resid(mmod) | sex, psid)

# fancier with some lines.  The points should lie on the line
# if we have normal residuals.  (We don't.)
qqmath(~ resid(mmod)  | sex, data = psid,
       panel = function(x, ...) {
           panel.qqmathline(x, ...)
           panel.qqmath(x, ...)
       })


psid$educ_levels = cut(psid$educ, c(0,8.5,12.5,20), labels=c( "Less than HS", "HS", "Beyond HS" ) )
ggplot( data=psid, aes( x=fit, y=resid ) ) +
    facet_wrap( ~ educ_levels ) +
    geom_point()




# 
# Second example from chapter
#

data(vision)
vision$npower <- rep(1:4,14)

ggplot( data=vision, aes(x=npower, y=acuity, lty=eye ) ) +
    facet_wrap( ~ subject ) +
    geom_line()



mmod <- lmer(acuity~power+(1|subject)+(1|subject:eye),vision)
summary(mmod)

4.64^2/(4.64^2+3.21^2+4.07^2)
(4.64^2+3.21^2)/(4.64^2+3.21^2+4.07^2)
anova(mmod)

mmodr <- lmer(acuity~power+(1|subject)+(1|subject:eye),vision,subset=-43)
anova(mmodr)
summary(mmodr)

# change options on making contrasts with cat variables.  Save old options in 'op'
op <- options(contrasts=c("contr.helmert", "contr.poly"))
mmodr <- lmer(acuity~power+(1|subject)+(1|subject:eye),vision,subset=-43)
summary(mmodr)

# put old options back
options(op)


# what are our coefficients looking like?
contr.helmert(4)
plot(resid(mmodr) ~ fitted(mmodr),xlab="Fitted",ylab="Residuals")
abline(h=0)

# look at diagnostic of normality
qqnorm(ranef(mmodr)$"subject:eye"[[1]],main="")



# 
# Third example
#
data(jsp)
head( jsp )
jspr <- jsp[jsp$year==2,]
mjspr <- data.frame(rbind(jspr[,1:6],jspr[,1:6]),
                    subject=factor(rep(c("english","math"),
                                       c(953,953))),
                    score=c(jspr$english/100,jspr$math/40))


ggplot( data=mjspr, aes( x=raven, y=score ) ) +
    facet_grid( gender ~ subject ) +
    geom_point()


mjspr$craven <- mjspr$raven-mean(mjspr$raven)
mmod <- lmer(score ~ subject*gender+craven*subject+social+(1|school)+(1|school:class)+(1|school:class:id),mjspr)

anova(mmod)
summary(mmod)
0.1013^2/(0.1013^2+0.1166^2)

mjspr = transform( mjspr, resid = residuals( mmod ), fit = fitted( mmod ) )
ggplot( data=mjspr, aes( x=fit, y=resid ) ) +
    facet_wrap( ~ subject ) +
    geom_point()

