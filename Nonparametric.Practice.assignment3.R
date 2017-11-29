#4.2: use paired comparison permutation test to determine whether responses
# to this question changed sigificantly from the beginning to the end of the
#semester

before <- c(2,3,4,4,3,1,3,4,4,5,3,4,2,2,4,3,4,2,2)
after <-  c(2,4,4,4,4,4,3,5,4,4,4,5,4,2,5,5,4,1,2)
length(before)
length(after)

d <- after - before
d

#How many permutations
2^length(before) #524288 perms 

#Since possible number of perms is large, use perm.approx.dbar
dbar <- mean(d)
dbar

permbars = perm.approx.dbar(d, R = 1000)

#Since want to know if answered changed, it's a two sided test. 
pvalue.twotail = mean(abs(permbars) >= abs(dbar)) 
pvalue.twotail # pvalue is .033 so reject H0
#Conclusion: There is a significant difference between answers at
#beginning of semester and end of semester. 


#4.3: Obtain the permutation distribution of the signed-rank statistic Sr
#for exercise 1. 
T1 <- c(100, 250, 50, 80)
T2 <- c(112, 240, 58, 82)
d <- T2 -T1
d

#Get signed-rank statistic SR
signrank <- function(x){rank(abs(x))* sign(x) }

#Number of perms
2^length(T1)

##perm dist 
permdbars <- perm.dbar(signrank(d))
permdbars

#Perm dist in table format (Got values from permdbars)
#Perm:  1     2     3     4     5     6     7    8     9    10     11   12    13     14    15   16`
#SR:  -2.5  -2.0  -1.5  -1.0  -1.0  -0.5   0.0  0.5  -0.5   0.0   0.5   1.0   1.0   1.5   2.0   2.5

#4.7
#a) Test for differences among cutting dates using a perm test for a RCBD
x <- c(1.5, 1.8, 1.9, 2.1, 2, 2.5, 1.9, 2, 2.5, 2.8, 2.7, 2.6, 1.4, 1.6, 2.1, 1.8, 2.3, 2.4)
length(x)
blocks <- rep(1:6, each = 3)
blocks
grps <- rep(1:3,6)
grps
cbind(x,grps,blocks)

#Perm test for RCBD
#Get SSTM: sum of squared group means
SSTMobs <- getSSTM(x,grps)
SSTMobs

#Get perm dist 
permSSTM <- perm.approx.RCBD(x,grps, blocks, R = 1000)

#Get pvalue
(pval = mean(permSSTM >= SSTMobs)) #.027
#This means that there is at least one that difference among cutting dates.

#b) Analyze using anova for RCBD compare with part a. 
summary(aov(x~factor(grps) + factor(blocks)))

#Both are factors are sigificant, which means there is a difference between among cutting dates when using anova
#This agrees with what the permuation test said. 
#The difference is that permutation tests do not assume normally-distributed errors, and Anova assumes normally disbributed. 
#The permuatation pvalue will be different for each iteration, but the anova test will always find that factor(grps) and factor(blocks) 
#are significant for showing that at least one cutting date is different. 