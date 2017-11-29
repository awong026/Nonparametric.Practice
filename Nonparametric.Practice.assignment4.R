#5.2 Generate the permutation distribution of Spearman's r, (see Section 5.2) and Kendall's t (Section 5.3) for teh data in exercise 1
H <- c(68, 70, 74)
W <- c(145, 155, 160)

#Spearman correlation
(rs.obs <- cor(rank(H),rank(W)))

#perm tes for spearman correlation
permr <- perm.approx.r(rank(H),rank(W), 1000)
permr #Use word to show large perm distribution


#kendalls
permtau <- perm.approx.tau(H,W, 1000)
permtau #Use word to show distribution


#5.3 
#a) Plot the data to show a nonlinear relationship. Compute Pearson's correlation
#Spearman's correlation and Kendall's tau

A <- c(3, 7, 15, 24, 85, 180, 360)
S <- c(2500, 3200, 4300, 5300, 5900, 6700, 6900)
plot(A,S)
rank(A)
rank(S)

#Pearson's correlation
(rp.obs = cor(A, S))

#Spearman's correlation
(rs.obs = cor(rank(A), rank(S)))

#Kendall's tau
Tau <- getTau.notie(A,S)


#b) Test for significant association using each of teh measures of assocation in part a

#Pearson's check for association
permr <- perm.approx.r(A, S, 1000)
mean(permr >= rp.obs) #pvalue is 0 so H0 that there is no assication rejected. There is an assocation
cor.test(A, S , method = "pearson") #pvalue is .03, so reject no assocation at alpha =.05
#Spearman's check for assocation
permr <- perm.approx.r(rank(A), rank(S), 1000)
mean(permr >= rs.obs) ##pvalue is 0 so H0 that there is no assication rejected. There is an assocation
cor.test(A, S , method = "spearman") #pvalue is below .005 so reject H0. 


#Kendall's check for assocation
permt <- perm.approx.tau(A,S, 1000)
mean(permt >= Tau) ##pvalue is 0 so H0 that there is no assication rejected. There is an assocation
cor.test(A,S, method = "kendall") ##pvalue is 0 so H0 that there is no assication rejected. There is an assocation

#8.5 treat mean soil moisture readings as the response variable in Ex5.5.
#(a)  Use an appropriate bootstrap method to make a 90% confidence interval for ??1, the slope of the linear regression line y = ??0 + ??1x
M <- c(355, 370, 380, 380, 380, 400, 400, 415, 415, 415, 415, 430, 440, 440, 470)
SA <- c(.5, 2, 2, .5, 3, 0, 4, .5, 2, 4, 4, 1, 2, 5, 7)

### define my function of obtaining slope estimate
library(boot)
lm.func <- function(data, d)
{
  lm(data[d,1]~data[d,-1])$coef[2]
}

slope.boot = boot(cbind(M,SA), lm.func , R=1000)
quantile(slope.boot$t, c(0.05,0.95))
boot.ci(slope.boot, conf=0.90, type="bca")


#(b)  Use an appropriate bootstrap method to test H0 : ??1 = 0 versus Ha : ??1 > 0 and state your conclusion under significance level of 0.05.

## approach 2: based on the pivotal
summary(lm(M~SA))
bhat = 9.136
t2 = (bhat-0)/3.488 #se of beta1 is 3.488
t2.star <- NULL
n = length(M)

#bootstrap 
for(b in 1:1000){
  d = sample(1:n, n, replace=TRUE)
  coef = summary(lm(M[d]~SA[d]))$coef
  se = coef[2,2]
  slope = coef[2,1]
  t2.star = c(t2.star, (slope - bhat)/se)
}
t2.star.nna <- t2.star[!is.na(t2.star)]
pval2 <- mean(t2.star.nna > t2)
pval2
#.008, thus we reject H0 at alpha = .05

