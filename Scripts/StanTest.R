#stantest 10/12/18
# Without loading stan, Check to see if the toolchaion is working
fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
                           return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
                           ' )
fx( 2L, 5 ) # should be 10



#then set directory to where we saved the file
setwd("H:/Rwork/Scripts")

Sys.setenv(USE_CXX14 = 1)
library("rstan") # observe startup messages




"
These options respectively allow you to automatically save a bare version of a compiled Stan program 
to the hard disk so that it does not need to be recompiled and to execute multiple Markov chains in parallel.
"

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



"
Example 1: Eight Schools This is an example in Section 5.5 of Gelman et al (2003), 
which studied coaching effects from eight schools. For simplicity, we call this example 'eight schools.'

We start by writing a Stan program for the model and saving it in a new file 8schools.stan. 
Be sure to include a carriage return after the final closing brace.
"


"
// saved as 8schools.stan
data {
  int<lower=0> J; // number of schools 
  real y[J]; // estimated treatment effects
  real<lower=0> sigma[J]; // s.e. of effect estimates 
}
parameters {
  real mu; 
  real<lower=0> tau;
  real eta[J];
}
transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] = mu + tau * eta[j];
}
model {
  target += normal_lpdf(eta | 0, 1);
  target += normal_lpdf(y | theta, sigma);
}
"


"
In this model, we let theta be transformed parameters of mu and eta instead of directly declaring 
theta as parameters. By parameterizing this way, the sampler will run more efficiently 
(see detailed explanation). We can prepare the data in R with:
"

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
"
And we can get a fit with the following R command. Note that the argument to file = should point 
to where the file is on your file system unless you have put it in the working directory of R in 
which case the below will work.

"
fit <- stan(file = '8schools.stan', data = schools_dat, 
            iter = 1000, chains = 4)





#example 2 rats demo
y <- as.matrix(read.table('https://raw.github.com/wiki/stan-dev/rstan/rats.txt', header = TRUE))
x <- c(8, 15, 22, 29, 36)
xbar <- mean(x)
N <- nrow(y)
T <- ncol(y)
rats_fit <- stan(file = 'rats.stan')














# #Set rtools file path for compiler
# cat('Sys.setenv(BINPREF = "C:/Rtools/mingw_$(WIN)/bin/")',
#     file = file.path(Sys.getenv("HOME"), ".Rprofile"),
#     sep = "\n", append = TRUE)

# dotR <- file.path(Sys.getenv("HOME"), ".R")
# if (!file.exists(dotR))
#   dir.create(dotR)
# M <- file.path(dotR, "Makevars")
# if (!file.exists(M))
#   file.create(M)
# cat("\nCXX14FLAGS=-O3 -Wno-unused-variable -Wno-unused-function",
#     "CXX14 = g++",
#     file = M, sep = "\n", append = TRUE)


