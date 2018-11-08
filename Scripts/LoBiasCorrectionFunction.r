Lo_biasCorr_fun <- function(cpu_var,lcpu,c_var,mc)  # residual variance of glm model, LSM_estimate, estimated variance of each LSMean (se^2), degrees of freedom LSMean
	{
	 # Implementation of the Lo's Bias correction method as used in the SAS cpue delta standardization model Mortiz
	 # it works in a single observation, so it need to be within an for loop type construction for each LSMean
	 # output a vector with 2 items: [1] var of each LSMean [2] bias correction factor gc (see SAS code)
	 nObs <- length(cpu_var)
	 tc <- vector(mode="numeric",length=nObs)
	 td <- vector(mode="numeric",length=nObs)
	 gc <- vector(mode="numeric",length=nObs)
	 gd <- vector(mode="numeric",length=nObs)
	 tmp <- matrix(data=0,nrow=nObs,ncol=2)
	 for (r in 1:nObs)
	 {
	 tc[r] <- (cpu_var[r]-c_var[r])*(mc[r]+1)/(2*mc[r])
	 td[r] <- (cpu_var[r]-2*c_var[r])*(mc[r]+1)/mc[r]
	 gc[r] <- 0; gd[r] <- 0; d <- 1;
	 for (p1 in 0:50) {
			 p <- p1
			 if(p < 1) { gc[r] <- 0; gd[r] <- 0; d <-1; }   # this resets the variables gc/gd to zero, & d to 1
       d <- (mc[r]+2*p)*d;
		   a <- (mc[r]^p)*(mc[r]+2*p);
		   b <- (mc[r]/(mc[r]+1))^p;
		   c <- gamma(p+1);
		   cc <- (tc[r]^p)/c;
		   cd <- (td[r]^p)/c;
		   prgc <- gc[r]; prgd <- gd[r];
		   gc[r] <- gc[r] + (a/d)*b*cc;
		   gd[r] <- gd[r] + (a/d)*b*cd;
		   if(p > 2) {
		      tol <- gc[r]-prgc; tol2 <- gd[r]-prgd;
		   if(abs(tol) < 0.0000001 & abs(tol2) < 0.0000001) break }
	   }   # end the for loop for @ obs
	 var_c <- exp(2*lcpu[r])*(gc[r]^2-gd[r])
	 tmp[r,1] <- var_c; tmp[r,2] <- gc[r];
	 } # end of the nobs loop
	 return(tmp)
 	}
