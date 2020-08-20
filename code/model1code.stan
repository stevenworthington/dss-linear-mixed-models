// This is Stan code. 
// It is a simple OLS model for the regression of
// math achievement on ses.
// It is for the "getting started with stan" tutorial and miniassignment.
//
// Each complete line of code ends with a semicolon.
// As for the rest of the structure, it's best to explain by example:
// Also, see section 27.2 of Stan manual for overview of Stan's program blocks

// Define variables in data:
data {
	
	int<lower=0> nstudents;  // Total number of students, constrained to be non-negative integers
	
	// student covariates
	vector[nstudents] ses;   // Define ses as a vector of length nstudents
	
	// outcome
	vector[nstudents] mathach;   // Define mathach as a vector of length nstudents
}

// Define parameters that will be estimated:
parameters {
	
	// Population intercept
	real mu;   // 'real' specifies that mu takes on real-valued numbers
	
	// Population slope for SES
	real beta_ses;
	
	// Error term
	real<lower=0, upper=100> sigma_y;  // Specify sigma_y to be a non-negative
	// real number less than or equal to 100
}

transformed parameters  {
	// In some cases, it can be convenient to calculate derived values, such as
	// predicted means or residuals, here. This can help make the code in the 'model'
	// section below simpler. However, there is a tradeoff; any variables you create here
	// will show up in the raw output of various Stan functions, so you'd probably have to
	// filter it out.
}

model {
	// Priors for parameters
	// The default in Stan is to provide uniform (or 'flat') priors on parameters
	// over their legal values as determined by their declared constraints.
	// A parameter declared without constraints is thus given a uniform prior on
	// (-inf, inf) by default, whereas a scale parameter declared with a lower bound
	// of zero gets an improper uniform prior on (0, inf).
	
	// Here, we'll use default uniform priors for mu and beta_ses,
	// but placing a uniform(-inf, inf) prior on the SD is usually a bad idea
	// so we've constrained it to have a finite support in the next line:
	
	sigma_y ~ uniform(0, 100) ;
	// As stated on p. 46 of the Stan manual, it's important
	// that the support and constraints of parameters match.
	// See the definition of the sigma_y parameter above under 'parameters'
	
	
	// Finally, our simple linear regression model
	mathach ~ normal(mu + ses * beta_ses, sigma_y);
	// This says that mathach_i is Normally distributed with mean (mu + ses_i*beta_ses) and 
	// variance sigma_y^2
	// See Chap 41 of Stan manual for parameterizations of different distributions.
}