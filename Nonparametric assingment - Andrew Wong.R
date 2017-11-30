
##############################################################################

#Quesetion 1: Test whether probability of beliving in heaven and not hell is the same as believing in hell but not heaven. 


belief <- matrix(c(955, 9, 162, 188), nrow = 2, dimnames = list("Belief in Heaven" = c("yes", "no"), "Belief in Hell" = c("yes", "no")))
belief

#H0 is that they are the same. 
mcnemar.test(belief) #Reject H0 and say there is a difference in probablity of those who believe in heaven and not hell and to those who believe in hell but not heaven


##############################################################################

#Question 2: Test the dataset for trend in annual flow (Nile)

library(trend)
Nile
cs.test(Nile) #pvalue is below .0005, so reject H0 that there is no trend. There is trend. 


##Alternative way (manually calc)
length(Nile)

##split this into two vectors
fhalf <- Nile[1:50]
shalf <- Nile[51:100]
fhalf
shalf


diff <- shalf - fhalf
diff
### Only look at teh signs
signs <- sign(diff)
signs #1 differnce of zero which is a tie

#Remove the zero just for fun
signs <- signs[signs!=0]
signs

## now we count the positive and negatives 
pos <- signs[signs >0]
neg <- signs[signs <0]


binom.test(length(pos), length(signs)) #pvalue is below .0005 so reject H0, there is trend



#############################################################
#Question 3: Determine whether the three measures are correlated and wheter Duke Energy and
#Coca cola closing share prices can be used to predict Amazon closing shares. 

#Import data
library(readxl)
AMZN_DUK_KO <- read_excel("C:/Users/awong/Desktop/Memphis/Linear Modeling/HW/11-27-17/AMZN-DUK-KO.xlsx")
View(AMZN_DUK_KO)

data <- AMZN_DUK_KO
head(data)
#Couldn't find data so use tree data instead
#are girth and height related?

##Pearsons - requires normality - let's check it
attach(data)
qqnorm(AMZN)
shapiro.test(AMZN) #pvalue is below .0005 so probably not normal
qqnorm(DUK)
shapiro.test(DUK) #pvalue is below .0005 so probably not normal
qqnorm(KO)
shapiro.test(KO) #pvalue is below .0005 so probably not normal

#H0 correlation is 0, rho = 0. #Just for pratice since pearson only works if normal assumption is good, which it isn't in this situation
cor.test(AMZN, DUK, method = "pearson")
cor.test(AMZN, KO, method = "pearson")
cor.test(DUK, KO, method = "pearson")


#Also check spearman for nonpar way
cor.test(AMZN, DUK, method = "spearman") #pvalue below .005 so there is correlation
cor.test(AMZN, KO, method = "spearman") #pvalue below .005 so there is correlation
cor.test(DUK, KO, method = "spearman") #pvalue below .005 so there is correlation



##Kendall method
cor.test(AMZN, DUK, method = "kendall") #pvalue below .005 so there is correlation
cor.test(AMZN, KO, method = "kendall") #pvalue below .005 so there is correlation
cor.test(DUK, KO, method = "kendall") #pvalue below .005 so there is correlation


##All 3 methods rejected H0, so we know there is a linear relationship. 
#If we wnat to predict the AMZN given the DUK or KO, we can use regression

fit <- lm(AMZN ~ DUK)
summary(fit) #pvalue for predictor and overall F test are sig so yes can use to predict AMZ

fit2 <- lm(AMZN ~ KO)
summary(fit2) #pvalue for predictor and overall F test are sig so yes can use to predict AMZ

