#Assignment: Pratice using different nonparametric methods

##############################################################

#Question 1: The carapace lengths (in mm) of crayfsh were recorded for samples from two sections of a stream in Kansas.

##############################################################


s1 <- c(5,11,16,8,12)
s2 <- c(17,14,15,21,19,13)

#(a)Test for differences between the two sections using a permutation test.
#H0: u(s1) = u(s2)
#Ha: u(s1) is no equal u(s2)
library(gtools)
  
#Create permutation function
perm.2sample = function(x, y, alternative = c("two.sided", "less", "greater"), stat=c("meandiff", "mediandiff", "trmdiff", "sumX"), trim=0)
{
  # exact permutation
  # test $H_0: delta=mu_x-mu_y=0$ versus $H_a: delta>0 (<0) (\neq 0)
  # stat="meandiff": difference of means
  # stat="mediandiff": difference of medians
  # stat="trmdiff": difference of trimmed means 
  # trim: the fraction (0 to 0.5) of observations to be trimmed from each end of the data set before the mean is computed. 
  #stat="sumX": sum of observations from the x group (equivalent with meandiff)
  
  m = length(x)
  n = length(y)
  N = m+n
  nperm = choose(N, m)
  if(nperm>5000){ warning("The number of permutations is over 5000, consider random permutation!")
    stop()
  }
  
  xy = c(x, y)
  idx = combinations(n=N, r=m)
  R = nrow(idx)
  permut = NULL
  for(i in 1:R)
  {
    permut = rbind(permut, c(xy[idx[i,]], xy[-idx[i,]]))
  }
  if(stat=="meandiff") trim=0
  if(stat %in% c("meandiff", "trmdiff"))
    D = apply(permut, 1, function(x) mean(x[1:m], trim)-mean(x[-(1:m)], trim))
  if(stat=="mediandiff")
    D = apply(permut, 1, function(x) median(x[1:m])-median(x[-(1:m)]))
  
  if(stat=="sumX")
    D = apply(permut, 1, function(x) sum(x[1:m]))
  
  if(alternative=="greater")
    pval = mean(D >= D[1])
  if(alternative=="less")
    pval = mean(D <= D[1])
  if(alternative=="two.sided") 
    pval = mean(abs(D) >= abs(D[1]))
  
  return(list(pval=pval, Dobs=D[1]))
}

#Use new permutation function
perm.2sample(s1,s2, alternative = "two.sided", stat = "meandiff") #pvalue is .023 and is less than the common level of signfiance of alpha = .05, which means reject the null hypothesis. 
## The two sections have different means. 

##(b) Test for differences using the Wilcoxon rank-sum test.

wilcox.test(s1,s2, alternative = "two.sided") # pvalue is .0303. Different p value, but the result is the same. Still a low pvalue so reject null and accept that two sections have different means. 


#################################################################################

##Question 2: Student in an introductor stats class were asked how many brothers and sisters
#they have and whether their hometown is urban or rural

################################################################################


R<- c(3, 2, 1, 1, 2, 1, 3, 2, 2, 2, 2, 5, 1, 4, 1, 1, 1, 1, 6, 2, 2, 2, 1, 1) 
U <- c(1, 0, 1, 1, 0, 0, 1, 1, 1, 8, 1, 1, 1, 0, 1, 1, 2)

#(a)Test for a signi???cant di???erence between rural and urban areas using the Wilcoxon rank-sum test. 
#Use signi???cance level 0.05.


wilcox.test(R,U, alternative = "two.sided")  ##ties, pvalue is .001 signficant

#(b)Test for a signi???cant di???erence between rural and urban areas using the two-sample t-test.
#Use significance level 0.05.

t.test(R,U, altnerative = "two.sided") # pvalue is .131 so non signficant

#(C)  Compare the results in parts (a) and (b). Why are the results di???erent?
#The tests give different pvalues because using non parametric method (Uses ranks), and t test assumes normal
shapiro.test(R) ##pvalue less than .05 so not normal
shapiro.test(U) ##pvalue less than .05 so no normal
qqplot(R,U) # the graph doesn't look normal at all. So wilcox.test would be better than t.test for determining pvalues. 



##############################################################

#Question 3:  The carapace lengths (in mm) of cray???sh were recorded for samples from two sections of a stream in Kansas.

#############################################################


#(a)Obtain the Hodges-Lehmann estimate of ???.
#(b)Get 90% CI for delta

S1 <- c(5,11,16,8,12)
S2 <- c(17,14, 15, 21, 19,13)
pwd = outer(S1,S2, "-")
pwd = sort(pwd)

library(Rmisc)

mean(pwd)  ##mean is -6.1

##OR.... 
library(DescTools)
HodgesLehmann(S1, S2, conf.level = .90, na.rm=FALSE) #est = -6 and lower CI is -10 and upper is -2 (90% CI)

##OR
wilcox.test(S1,S2, conf.int = "T", conf.level = .9) #est = -6 and lower CI is -10 and upper is -2 (90% CI)

#(c) The difference of location between S1 and S2 is estmated to be -6. That means there is about a -6 swift from S1 to S2 location. 
#The CI for the values are -10 and -2 at alpha = .1


######################################################

##Question 4: Nest heights (in meters) of two species of woodland nesting birds were measured.

######################################################


A <- c(5.1, 9.4, 7.2, 8.1, 8.8)
B <- c(2.5, 4.2, 6.9, 5.5, 5.3)

#Question: Test for differences between the distributions of the nesting heights of the two species.
##Kolmogorov-Smirnov Test
plot(ecdf(A), vertical = T)
lines(ecdf(B), vertical = T, col = "red", lty = "dashed")

ks.test(A,B)
#pvalue is .07937 which means the distributions of the nesting heights of the two species are the same
#The distributions are same at alpha =.05
