// Simple Hierarchical Model for Stan
// Used for the HS&B dataset

// Define all the variables in your model in data
data {
    int<lower=0> nstudents; //Total number of students
    int<lower=0> nschools; //Number of schools
    
    // Cluster IDs (these need to be 1, 2, 3,... so you might have to convert original IDs)
    int<lower=1> sid[nstudents];
    
    // student covariates
    vector[nstudents] ses;
    
    // school covariates
    vector[nstudents] sector;
     
    //Two notes:
    //1. We're declaring this as one per student, for faster computation
    //2. This is actually a vector of integers, but
    //this declaration allows reals. If we wanted to ensure
    //integer values, we could have used:
    //int sector[nstudents];
    //However, keeping it as a vector can allow faster vectorized math.
    
    // outcome
    vector[nstudents] mathach;
    
}


// Define all the parameters we wish to estimate
parameters {
    // Population intercept (a real number)
    real gamma_00;
    // Fixed effect of school sector
    real gamma_01;
    // Population slope by SES
    real gamma_10;
    // Interaction of SES and school type (sector)
    real gamma_11;
    
    // Level-1 error
    real<lower=0.001> sigma_y;
    
    // Level-2 random effect
    real<lower=0.001> sigma_u;
    
    // These are our individual random intercepts, which are parameters
    // in the Bayesian approach.
    vector[nschools] u0; 
    
    //Remark: Some might instead use:
    //real u0[nschools]; 
    //The above line shows how you would create an array rather than a vector.
}


// The Model
model {

    //u0s vectorized by student. 
    //We don't put this in 'transformed parameters' because we don't need bounds checking,
    //and never want to see it in output.
    vector[nstudents] student_u0;
    
    // Priors are uninformative.  Many choices are possible.  
    // Approximately 2.5 * sd( Y ) is one option
    sigma_y ~ cauchy( 0, 2.5 * 6 );
    sigma_u ~ cauchy( 0, 2.5 * 6 );
    
    gamma_10 ~ cauchy(0,10); 
    //The interquartile range of SES is about 1, of mathach is about 10. 
    //This effect can be no larger than the ratio of the standard deviations,
    //so this gives it plenty of room. We could have left a flat prior here, but
    //explicit priors is good practice.
    //gamma_00 ~ cauchy(10,10); //commented out, because a flat prior is fine here; 
    //but if you wanted to be explicit, this would work.
    gamma_01 ~ cauchy(0,10);
    gamma_11 ~ cauchy(0,5); 
    //We're being a bit tighter for the int term as we add more-involved parameters. 
    //We could probably afford to go tighter still, but this is a fair start. 
    

    // Random effects distribution
    u0 ~ normal(0, sigma_u);
    
    // translate the u0s from an array over schools to a vector over students
    for (i in 1:nstudents)
        student_u0[i] = u0[sid[i]];
    
    // Our observations are normally distributed around the predicted values
    // (These are the residuals)
    mathach ~ normal(
        gamma_00 + student_u0 + gamma_01 * sector +  
            (gamma_10 + gamma_11 * sector
            ) .* ses, //Note the '.*', not '*' for elementwise multiplication
        sigma_y);
}
