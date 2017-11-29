##### permutation test for pearson correlation
##
perm.approx.r <- function(x,y,R)
{
	## approximate permutation distribution of sample correlation coefficient r
	n <- length(x)
	results <- rep(NA,R)
	for (i in 1:R) results[i] <- cor(x,y[sample(1:n,n)])
	results
}



##
##### permutation test for Kendall's Tau
##
getTau.notie <- function(x,y)
{
	n <- length(x)
	mat <- cbind(x,y)
	mat <- mat[order(mat[,1]),]
	concord <- 0
	for (i in 1:(n-1)) concord <- concord + sum( mat[i,2] < mat[(i+1):n,2] ) +
		                               .5 * sum( mat[i,2] == mat[(i+1):n,2] )
	2*concord/(n*(n-1)/2) - 1
}

getTau <- function(x,y)
{
# can handle tied observations, but slower
        n <- length(x)
        mat <- cbind(x,y)
        mat <- mat[order(mat[,1]),]
        concord <- 0
        for(i in 1:(n-1))
        {
        	for(j in (i+1):n)
        	{
        	  tmp = (x[i]-x[j])*(y[i]-y[j])
        	  concord = concord + 1*(tmp>0) + 1/2*(tmp==0)	
        		}
        	}
        2*concord/(n*(n-1)/2) - 1
}

perm.approx.tau <- function(x,y,R)
{
	n <- length(x)
	results <- rep(NA,R)
	for (i in 1:R) results[i] <- getTau(x,y[sample(1:n,n)])
	results
}

