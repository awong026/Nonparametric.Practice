#3.1
#The data are samples from three simulated distributions
#a) perm F test
set.seed(12345)
x<- c(2.9736, .9448, 1.6394, .0389, 1.2958, .7681, .8027, .2156, .074, 1.5076, 4.8249, 2.2516, 1.5609, 2.0452, 1.0959)
grps <- rep(1:3, each = 5)
perm.F = perm.approx.F(x, grps, R=300)
Fobs = getF(x,grps)
(perm.pval = mean(perm.F >= Fobs)) #pvalue is .06333 
#alpha is not set, but if alpha was set at .05 then this test says
#that the means of the 3 groups are the same

#b) One way anova
summary(aov(x ~ factor(grps))) #pvalue is .0833
#The end result of the ANOVA test is the same as the perm F test if alpha = .05
#The difference is between the two p value is because anova assumes normal  dist and equal variances. 
#The perm F test can be used for other distributions and still get a reasonable result.


#########################################################################################################


#3.5
#An agronomist gave scores form 0 to 5 to denote insect damage to wheat plants that were treated with four insecticides.
#The data are given in the following table. Use the Kruskal-Wallis test with ties to test whether or not there is a difference among treatments. 
x <- c(0,2,1,3,1,1, 2,0,3,1,3,4, 1, 3, 4, 2, 2, 1, 3, 4, 2, 5, 3, 4)
grps <- rep(1:4, each = 6)
kruskal.test(x,grps)
##pvalue is .0541
#If alpha is .05 then this says that the four treatments have the same mean. 
#Since the pvalue is close to .05, it would be up to the reseacher/requester if they 
#accept that the four treatments have the same mean. 


#####################################################################################################


#3.6
#Test for differences among groups using the kruskal-Wallis test.
#Separate means using the rank versions of the LSD and HSD criteria. 

x <- c(791, 846, 1024, 1007, 399, 1279, 407, 1656, 1036, 1226,
       423, 541, 517, 1328, 471, 533, 863, 786, 551, 1068, 
       551,  1068, 757, 1114, 920, 1809, 1238, 918, 1339, 603, 
       712, 435, 298, 733, 1200, 1701, 707, 790, 800, 480,
       1345, 1269, 1477, 758, 996, 1306, 968, 943, 1026, 1564,
       985, 1074, 742, 985, 1342, 1184, 977, 1465, 892, 1074, 
       805, 2613, 903, 949, 1183, 1051, 1387, 1320, 1434, 1603)
grps <- rep(1:7, each = 10)
kruskal.test(x,grps) #pvalue = .005547
#At alpha = .05 we can say that using kruskal wallis test that the means of each vehicle is different. 
k<- 7
### LSD with ranks
Fisher.LSD.rank(x, grps, k, alpha=0.05, R=1000)
#Fisher rank test says there are different means amoung type of vehicles. There are 8 pairs of vehicles that have differnt means

###### Tukey's HSD with ranks
Tukey.HSD.rank(x, grps, k, alpha=0.05, R=1000)
#Tukey HSD rank test says that there are no pairs that have different means. This disagrees with Kruskal-Wallis test


######################################################################################################################



#3.9
x<- c(574, 976, 789, 805, 361, 529,
      791, 1146, 394, 767, 1385, 1021, 2073, 803, 1263, 1016, 1101, 945, 139, 
      865, 775, 729, 1721, 1113, 820, 1613, 1404, 1201, 205, 1380, 580, 1803, 
      998, 1049, 736, 782, 730, 742, 1219, 705, 1260, 611, 1350, 1657, 1143,
      1154, 541, 406, 1529, 1132, 767, 1224, 314, 1728)
grps <- c(rep(1,each = 6), rep(2:4, each = 13), rep(5, each= 9))
k <- 5
### based on rank-sum statistic
#Ha: responses tend to increase as weight increases
set.seed(1235662)
JT.ranksum(x, grps, k, R=1000) #pvalue = .191
#If alpha = .05 then we can't accept that responses tend to increase as weight increases

###based on Mann-Whitney statistic
set.seed(1235662)
JT.MW(x, grps, k, R=1000) #pvalue = .191

#Both version of the Janckheere test gave the same pvalue of .191. If alpha was set at .05 then
#we can't reject H0. The means of response are either decreasing or the same as weight increases. 


#####################################################################################################

#3.11
#a)
y <- c(49.17, 48.19, 48.93, 
       4.48, 4.43, 4.79,
       6.36, 7.03, 5.36,
       5.34, 4.86, 6.64,
       5.17, 6.47, 6.58,
       3.99, 5.89, 6.61,
       6.02, 4.96, 6.30,
       3.98, 6.30, 4.96,
       5.94, 6.03, 5.56,
       6.29, 5.11, 5.93)
grps <- rep(1:10, each = 3)
k<-10

#### LSD
Fisher.LSD(y, grps, k, alpha=0.05, R=1000) #pvalue = 0
#With every treatment, the Fisher LSD caught that their was a difference of means


#Try without treatment 1

x1 <- c(4.48, 4.43, 4.79,
        6.36, 7.03, 5.36,
        5.34, 4.86, 6.64,
        5.17, 6.47, 6.58,
        3.99, 5.89, 6.61,
        6.02, 4.96, 6.30,
        3.98, 6.30, 4.96,
        5.94, 6.03, 5.56,
        6.29, 5.11, 5.93)
grps <- rep(1:9, each = 3)
k<-8
Fisher.LSD(x1, grps, k, alpha=0.05, R=1000) #pvalue .398
#This time the fisher test says that the means are the same between treatments
#because if alpha is .05 then pvalue of .398 is too big to reject H0. 

#Shortcomings of LSD
#LSD does not adjust for mutltiple comparisons. The numbers don't account for multiple comparisons, so 
#researcher would have to compute that themselves. This means there is a higher chance of detecting a difference of means when there might not be one. 


#b) Is it likely or unlikely that Bonferroni or Tukey HSD would find differences among means of
#treatments 2 through 10?

##### Bonferroni
set.seed(123456)
Bonf.adj(x1, grps, k, alpha = .05) #From looking at the table that was produced none of the pairs are significantly different using the Bonferroni adjustment
#The Bonferroni is less likely to detect a difference because it adjusted for mutiple comparisons. This usually makes the alpha value lower, which would reduce the chance 
#of it detecting a difference. For this example, the alpha was set to .05, but the bonferroni procedure made the new alpha .0017. This means it is less likely than
#Fisher at alpha .05 to detect a difference of means among treatments. 

###### Tukey's HSD 
Tukey.HSD(x1, grps, k, alpha = .05) ##From looking at the table that was produced none of the pairs are signfiicantly different using the Tukey HSD adjustment
#Studized adjustment here. This also means that this test is more strict than Fisher on detecting differences among treatment means. It is also less likely to detect a difference of means
#among treatments than fisher LSD. 



