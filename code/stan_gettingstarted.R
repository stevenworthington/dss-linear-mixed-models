## ----installation, echo=TRUE, eval=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
## # Run this once to install everything.
## install.packages("rstan", dependencies = TRUE)
## 
## # In your files you then load the installed package:
## library(rstan)
## # Read startup messages and decide if you want to follow the recommendations


## ----setup, echo=TRUE, warning=FALSE, results='hide', message=FALSE-----------------------------------------------------------------------------------------------------------------------
library(rstan)    # For running Stan
options(digits=4) # Avoid excessive precision in displayed values


## ----loading data, cache=TRUE, echo=TRUE, warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------------
# Load data into R as usual
hsstud <- read.csv( "hsb1.csv", header=TRUE )  

# Create data list we will feed into Stan
model1dat <- list(           
  nstudents = nrow(hsstud),
  ses = hsstud$ses,
  mathach = hsstud$mathach
)


## ----compiling model, echo=TRUE, cache=TRUE, warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------------
model1stan <- stan_model(file="model1code.stan")


## ----fitting model, echo=TRUE, cache=TRUE, message=FALSE, warning=FALSE,results='hide'----------------------------------------------------------------------------------------------------
model1mcmc <- sampling(model1stan, data = model1dat, chains=4, iter=1000)  


## ----print out results, echo=TRUE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------
model1mcmc   


## ----plotting traceplots, echo=TRUE, message=FALSE, warning=FALSE, fig.height=3-----------------------------------------------------------------------------------------------------------
traceplot(model1mcmc)


## ----getting posteriors, echo=TRUE, message=FALSE, warning=FALSE, fig.height=3------------------------------------------------------------------------------------------------------------
model1samps_with_warmup_draws <- as.data.frame(model1mcmc) 

# Exclude the MCMC warm-up draws, which is set in sampling() by default 
# to be the first iter/2 draws
model1samps <- model1samps_with_warmup_draws[-c(1:500), ]  
str(model1samps)   

# Plot the posterior distributions of the model parameters
par(mfrow=c(1,3))
for(param in 1:3){
  plot(density(model1samps[ ,param]), main=names(model1samps)[param])
}


## ----calculating credible intervals, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------
for(param in 1:3){
  cat(paste(names(model1samps)[param], ":"), 
      quantile(model1samps[ ,param], probs=c(0.05, 0.95) ), '\n')
}

