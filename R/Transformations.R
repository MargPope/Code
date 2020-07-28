##################################################################
###########Data Distributions & Transformations###################
##################################################################

###Useful Links
#https://statsthewayilikeit.files.wordpress.com/2014/07/tests-for-normality.pdf
#http://www.math.uah.edu/stat/hypothesis/Likelihood.html

### Packages
library(moments)
library(grid)
library(vcd)
library(MASS)
library(fitdistrplus)
library(ggplot2)

### Reading in Data
FreedomHouse <- read.csv("~/Dropbox/Backpage_Data/FreedomHouse.csv")
Original <- FreedomHouse[[1]]
mean(Original) #3.381443
var(Original) #4.6724
sd(Original) #2.161573


####################################################
########### Probability Density Functions ##########
####################################################

#############
### dnorm ###
#############

# returns the height of the probability density function at a given point
# Y value given X value
# does not have much value when working with continuous function

test_scores <- c(84, 65, 92, 88, 75)  # Creates a vector with 5 values: 84, 65, 92, 88, 75
mean(test_scores) # 80.8
sd(test_scores) # 10.84896
dnorm(test_scores, 80.8, 10.84896) # Returns the density at a given score in a normal distribution

prob_scores <- dnorm(test_scores, mean=80.8, sd=10.84) 
plot(test_scores, prob_scores)

data <- data.frame(test_scores, prob_scores) # Creates a data frame of 2 vectors: scores and probability of each score
View(data)


#############
### pnorm ###
#############

# returns the cumulative density function
# probability that a number falls under a certain value x 

cdf_scores <- pnorm(test_scores, mean=80.8, sd=10.84)
plot(test_scores, cdf_scores)

data <- data.frame(test_scores, prob_scores, cdf_scores)
plot(test_scores, prob_scores)
plot(cdf_scores, prob_scores)

pnorm(q=90, mean=80.8, sd=10.84896) # probability of having a value lower than 90 (.80)

curve(dnorm(x, mean=80.8, sd=10.84896), xlim=c(0,200)) # graph the distribution; don't give a value to x
abline(h=0)
sequence <- seq(0, 90, by=.1)
polygon(x=c(sequence, 90,0),
        y=c(dnorm(c(sequence),80.8,10.84896),0,0),
        col="grey")
# shows graphically where a score of 90 falls in this distribution
# you can see why the probability of having a score below 90 is quite high (80%)

1-pnorm(q=90, mean=80.8, sd=10.84896) # probability of having a score higher than 90 (20%)

curve(dnorm(x, mean=80.8, sd=10.84896), xlim=c(0,200)) # graph the distribution; don't give a value to x
abline(h=0)
sequence <- seq(90, 200, by=.1)
polygon(x=c(sequence, 200, 90),
        y=c(dnorm(c(sequence),80.8,10.84896),0,0),
        col="grey")
# shows graphically where a score of 90 falls in this distribution
# you can see why the probability of having a score above 90 is quite low (20%)


#############
### qnorm ###
#############

# returns the inverse cumulative density function (quantiles)
# quantity of the area under the curve that falls to the left of a given value x
# with normal distribution of mean 0 and sd 1, qnorm returns Z score given a probability

qnorm(.75, 80.8, 10.84896) # returns the value that represents the 75th percentile of test scores, where the mean and sd are as specified
qnorm(.025, 80.8, 10.84896) # returns the value below which the probability is less than 2.5% (59.53643)
qnorm(.025, 80.8, 10.84896, lower.tail=FALSE) # returns the value above which the probability is less than 2.5% (102.0636)

curve(dnorm(x, mean=80.8, sd=10.84896), xlim=c(0,200)) # graph the distribution; don't give a value to x
abline(h=0)
sequence <- seq(0, 59.53643, by=.1)
polygon(x=c(sequence, 59.53643,0),
        y=c(dnorm(c(sequence),80.8,10.84896),0,0),
        col="grey")
sequence <- seq(102.0636, 200, by=.1)
polygon(x=c(sequence, 200, 102.0636),
        y=c(dnorm(c(sequence),80.8,10.84896),0,0),
        col="grey")
# shows graphically the areas that correspond with 5% probability 


#############
### rnorm ###
#############

simulated_scores <- rnorm(5, mean=80.8, sd=10.84896)
qqnorm(simulated_scores)
qqline(simulated_scores)

sequence <- seq(0,150, by=.1)
y1 <- pnorm(sequence, 80.8, 10.84896)
y2 <- pnorm(sequence, 81.70114, 10.99498)
df <- data.frame(sequence, y1, y2)
ggplot(df, aes(sequence))+
  geom_line(aes(y=y1), col="red")+
  geom_line(aes(y=y2), col="green")

plot(sequence, y1, type="l", col="red")
lines(sequence, y2, type="l", col="green")

plot(sequence, test_scores, axes=FALSE)
axis(1, sequence)
axis(2)
box()

new.data <- data.frame(test_scores, simulated_scores)
ggplot(new.data, aes(sequence)) +
  geom_curve(aes(y=simulated_scores), col="red")+
  geom_point(aes(y=test_scores), col="green")



####################################################
####################################################
########### Testing for Normality ##################
####################################################
####################################################

### Generate Random Normal Distribution with mean and stdev of your sample data
set.seed(3000)
data_normal <-rnorm(194, mean=3.381443, sd=2.161573)
data_normal

### Histograms
hist(data_normal)
hist(Original)


################
### QQ Plots ###
################

qqnorm(data_normal) 
# Compares normally distributed random variable against theoretical distribution that is assumed to be normal
# If linear, then data_normal is normally distributed
# Since we generated data_normal as a normal distribution, this should be linear
qqnorm(Original) 
# Compares your variable against theoretical distribution that's assumed to be normally distributed
# If linear, then data is normally distributed
qqplot(data_normal, Original)
abline(0,1)
# Compares your sample data to theoretical data
# In this case, the theoretical data is your randomly generated data_normal variable


#############################
### Skewness and Kurtosis ###
#############################

library(moments)
skewness(Original) 
# Result is low skewness in positive direction (0.34425)
skewness(data_normal) 
# Result is very low skewness in negative direction (-0.2045159)
# Skewness less than -1 or greater than 1 = highly skewed
# Skewness between -1 and -0.5 or 0.5 and 1 = moderately skewed
# Skewness between -0.5 and 0.5 = distribution is approximately symmetric

kurtosis(Original)
# Result is low kurtosis (1.653123)
kurtosis(data_normal)
# Result is normal kurtosis (3.130075)
# Kurtosis=3 for a standard normal

##################################
### Formal tests for normality ###
##################################

# Null: sample comes from a normally distributed population
# Alternative: sample comes from non-normally distributed population
# If p<.05, reject null. Thus, sample is non-normally distributed.
library(nortest) 
shapiro.test(Original) #Shapiro-Wilk
lillie.test(Original)  #Lilliefors
ad.test(Original)      #Anderson-Darling
cvm.test(Original)     #Cramer-von Mises
pearson.test(Original) #Pearson's
sf.test(Original)      #Shapiro Francia

fit.normal <-fitdist(Original, distr="norm")
fitdist(Original, 'norm')$loglik
plot(fit.normal)

##################################################
##################################################
########### Testing for Poisson ##################
##################################################
##################################################

########################
### A: Basic Fitting ###
########################

fit.poisson <- fitdist(Original, distr="pois", method="mle", lower=c(0,0))
fitdist(Original, 'pois', lower=c(0,0))$loglik
summary(fit.poisson)
plot(fit.poisson)
gof(fit.poisson)  # p>.05 then you have a good fit

##############################
### B: Specifying Expected ###
##############################

set.seed(1)
mean(Original)
xpois<-rpois(n=194,lambda=3.381443) # a vector of random variables from the Poisson distr.
hist(xpois,main="Poisson distribution")
lambda.est <- mean(Original) ## estimate of parameter lambda
(tab.os<-table(Original)) ## table with empirical frequencies
freq.os<-vector()
for(i in 1: length(tab.os)) freq.os[i]<-tab.os[[i]]
freq.ex<-(dpois(0:max(Original),lambda=lambda.est)*200)
acc <- mean(abs(freq.os-trunc(freq.ex)))
acc/mean(freq.os)*100
gf <- goodfit(Original,type= "poisson",method= "MinChisq")
plot(gf)
ks.test(xpois, Original) #are the expected and the observed drawn from same poisson distribution? 
# Null is that the samples are drawn from the same distribution (p>.05 then you have a good fit)

#########################################################
### C: Transforming According to Poisson Distribution ###
#########################################################

set.seed(124)
mean(Original)
ppois(4, lambda=3.381443) #the probability of having a score of 4 or less in Freedom House
Transformpois <- ppois(Original, lambda=3.381443) #transform original data to vector that represents CDF with poisson distribution
expected_pois <- ppois(xpois, lambda=3.381443) 

par(mfrow=c(2,2))
cdfpois <- ecdf(expected_pois) # Gives the empirical cdf of pois
plot(cdfpois) # Draws a plot of the empirical cdf
cdforiginal <- ecdf(Original) #Gives the empirical cdf of sample data
plot(cdforiginal) #Draws a plot of the empirical cdf of sample
cdftransform <- ecdf(Transformpois)
plot(cdftransform)

par(mfrow=c(2,2))
hist(Transformpois)
hist(expected_pois)
hist(Original)
hist(data_normal)

ks.test(Transformpois, expected_pois) #are the transformed data and a theoretical poisson drawn from same distribution? 



##################################################
##################################################
############ Testing for Weibull #################
##################################################
##################################################


########################
### A: Basic Fitting ###
########################

library(fitdistrplus)
fit.weibull <- fitdist(Original, distr="weibull", method="mle", lower=c(0,0))
fitdist(Original, 'weibull', lower=c(0,0))$loglik
summary(fit.weibull)
plot(fit.weibull)
gof(fit.weibull)


##############################
### B: Specifying Expected ###
##############################

set.seed(1000)
data_weibull <- rweibull(194, shape=1, scale=2) #scale gives you spread; shape determines how fast you degrade
par(mfrow=c(2,2))
hist(data_weibull)
hist(Original)
ks.test(data_weibull, Original)


#########################################################
### C: Transforming According to Weibull Distribution ###
#########################################################

transformweibull <- pweibull(Original, shape=1, scale=2)
expected_weibull <- pweibull(data_weibull, shape=1, scale=2)
par(mfrow=c(2,2))
cdfweibull <- ecdf(transformweibull) # Gives the empirical cdf of weibull
plot(cdfweibull) # Draws a plot of the empirical cdf
cdforiginal <- ecdf(Original) #Gives the empirical cdf of sample data
plot(cdforiginal) #Draws a plot of the empirical cdf of sample
cdftransform <- ecdf(transformweibull)
plot(cdftransform)

par(mfrow=c(2,2))
hist(transformweibull)
hist(data_weibull)
hist(Original)
hist(expected_weibull)

ks.test(transformweibull, expected_weibull) #does the transformed data fit the expected?


#####################################################
#####################################################
############ Testing for Chi-Square #################
#####################################################
#####################################################

########################
### A: Basic Fitting ###
########################

fit.chisquare <- fitdist(Original, distr="chisq", method="mle", lower=c(0,0), start=list(df=6))
fitdist(Original, 'chisq', lower=c(0,0), start= list(df=6))$loglik
summary(fit.chisquare)
plot(fit.chisquare)
gof(fit.chisquare)

##############################
### B: Specifying Expected ###
##############################

set.seed(1000)
data_chi <- rchisq(194, df=6) 
par(mfrow=c(2,2))
hist(data_chi)
hist(Original)
ks.test(data_chi, Original)

#############################################################
### C: Transforming According to Chi-Squared Distribution ###
#############################################################

transformchi <- pchisq(Original, df=6)
expected_chi <- pchisq(data_chi, df=6)
par(mfrow=c(2,2))
cdfchi <- ecdf(transformchi) # Gives the empirical cdf of weibull
plot(cdfchi) # Draws a plot of the empirical cdf
cdforiginal <- ecdf(Original) #Gives the empirical cdf of sample data
plot(cdforiginal) #Draws a plot of the empirical cdf of sample
cdftransform <- ecdf(expected_chi)
plot(cdftransform)

par(mfrow=c(2,2))
hist(transformchi)
hist(data_chi)
hist(Original)
hist(expected_chi)

ks.test(transformchi, expected_chi)


#####################################################
#####################################################
############### Testing for Uniform #################
#####################################################
#####################################################


########################
### A: Basic Fitting ###
########################

fit.unif <- fitdist(Original,"unif", method="mle")
fit.dist(Original, 'unif', method="mle")$loglik
summary(fit.unif)
plot(fit.unif)
gofstat(fit.unif)


##############################
### B: Specifying Expected ###
##############################

set.seed(1000)
data_u <-runif(194, min=1, max=7)
par(mfrow=c(2,2))
hist(data_u)
hist(Original)
ks.test(data_u, Original)


#############################################################
### C: Transforming According to the Uniform Distribution ###
#############################################################


transformu <- punif(Original, min=1, max=7)
expected_u <- punif(data_u, min=1, max=7)
par(mfrow=c(2,2))
cdfu <- ecdf(transformu) # Gives the empirical cdf of weibull
plot(cdfu) # Draws a plot of the empirical cdf
cdforiginal <- ecdf(Original) #Gives the empirical cdf of sample data
plot(cdforiginal) #Draws a plot of the empirical cdf of sample
cdftransform <- ecdf(expected_u)
plot(cdftransform)

par(mfrow=c(2,2))
hist(transformu)
hist(data_u)
hist(Original)
hist(expected_u)

ks.test(transformu, expected_u)
qqplot(expected_u, transformu)
abline(0,1)


##############################################################
##############################################################
############### Testing for Beta Distribution ################
##############################################################
##############################################################

########################
### A: Basic Fitting ###
########################

transform_original <- pnorm(Original, mean=3.381443, sd=2.161573)
fit.beta <- fitdist(transform_original, "beta", method="mle")
summary(fit.beta)
plot(fit.beta) 
gofstat(fit.beta)


##############################
### B: Specifying Expected ###
##############################

mean(transform_original)
var(transform_original)

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
  }
estBetaParams(0.4862939, 0.1003898)
set.seed(3000)
data_beta <-rbeta(transform_original, shape1=0.7238103, shape2=0.7646112, ncp=0)
qqplot(data_beta, transform_original)
hist(data_beta)
hist(transform_original)
ks.test(transform_original, data_beta)


##########################################################
### C: Transforming According to the Beta Distribution ###
##########################################################

transformbeta <- pbeta(transform_original, shape1=0.7238103, shape2=0.7646112, ncp=0)
expected_beta <- pbeta(data_beta, shape1=0.7238103, shape2=0.7646112, ncp=0)
par(mfrow=c(2,2))
cdfbeta <- ecdf(transformbeta) 
plot(cdfbeta) 
cdforiginal <- ecdf(transform_original) 
plot(cdforiginal) 
cdftransform <- ecdf(expected_beta)
plot(cdftransform)

par(mfrow=c(2,2))
hist(transformbeta)
hist(data_beta)
hist(transform_original)
hist(expected_beta)

ks.test(transformbeta, expected_beta)
qqplot(expected_beta, transformbeta)
abline(0,1)


#####################################################################
#####################################################################
############### Testing for Exponential Distribution ################
#####################################################################
#####################################################################

########################
### A: Basic Fitting ###
########################

fit.exponential <-fitdist(Original, distr="exp", method="mle")
fitdist(Original, 'exp', lower=c(0,0))$loglik
summary(fit.exponential)
plot(fit.exponential)
gof(fit.exponential)

##############################
### B: Specifying Expected ###
##############################

FreedomHouse$exp <- exp(FreedomHouse$Original)
exp <- FreedomHouse$exp
set.seed(3000)
data_exp <- rexp(194)
par(mfrow=c(2,2))
hist(data_exp)
hist(exp)

ks.test(data_exp, exp)
qqplot(data_exp, exp)
abline(0,1)


#############################################################
### C: Transforming According to Exponential Distribution ###
#############################################################



###################################################################
###################################################################
############### Testing for the Gamma Distribution ################
###################################################################
###################################################################


########################
### A: Basic Fitting ###
########################

fit.gamma <- fitdist(Original, distr="gamma", method="mle", lower=c(0,0), start= list(scale=1, shape=1))
fitdist(Original, 'gamma', lower=c(0,0), start= list(scale=1, shape=1))$loglik
summary(fit.gamma)
plot(fit.gamma)
gof(fit.gamma)

##############################
### B: Specifying Expected ###
##############################

###########################################################
### C: Transforming According to the Gamma Distribution ###
###########################################################



####################################################
####################################################
########### Data Transformations ###################
####################################################
####################################################

FredomHouse$square <- (FreedomHouse$Original)^2
hist(FreedomHouse$square)

FreedomHouse$sqrt <- sqrt(FreedomHouse$Original)
### Creates a new variable named "sqrt" that is equal to the square root of the original variable "Original"
sqrt <- FreedomHouse[[2]]
mean(sqrt)
sd(sqrt)
set.seed(3000)
data_sqrt <-rnorm(1000, mean=1.735412, sd=0.6096764)
data_sqrt
hist(data_sqrt)
hist(sqrt)
qqplot(data_sqrt, sqrt)
abline(0,1)

FreedomHouse$ln <- log(FreedomHouse$Original)
### Creates a new variable named "ln" that is equal to the natural log of the original variable "Original"
hist(FreedomHouse$ln)
lognormal <- FreedomHouse[[3]]
mean(lognormal)
sd(lognormal)
set.seed(3000)
data_lognormal <-rlnorm(1000, mean=0.9696121, sd=0.7455172)
data_lognormal
hist(data_lognormal)
hist(lognormal)
qqplot(data_lognormal, lognormal)
abline(0,1)

FreedomHouse$exp <- exp(FreedomHouse$Original)
### Creates a new variable named "exp" that raises the constant e to the power of the original variable "Original"

FreedomHouse$arcsine <- asin(FreedomHouse$Original)
### Creates a new variable named "arcsine" that is equal to the arcsine of the original variable "Original"

FreedomHouse$log1 <- (log(FreedomHouse$Original))+1
log1 <- FreedomHouse[[6]]
qqplot(data_normal, log1)
abline(0,1)

FreedomHouse$box <- (((FreedomHouse$Original)^0.2064)-1)/0.2064
box <- FreedomHouse[[7]]
qqplot(data_normal, box)
abline(0,1)

FreedomHouse$ft <- (sqrt(FreedomHouse$Original)) + (sqrt(FreedomHouse$Original)+1)
ft <- FreedomHouse[[8]]
qqplot(data_normal, ft)
abline(0,1)

FreedomHouse$cube <- (FreedomHouse$Original)^(1/3)
hist(FreedomHouse$cube)



###################
#### Ln ~ Beta ####
###################

transform_original <- pnorm(FreedomHouse$ln, mean=0.9696121, sd=0.5557958)
mean(transform_original)
var(transform_original)

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
estBetaParams(0.5157402, 0.1398693)
set.seed(3000)
data_beta <-rbeta(transform_original, shape1=0.4051715, shape2=0.3804401, ncp=0)
qqplot(data_beta, transform_original)
hist(data_beta)
hist(transform_original)
fit.beta <- fitdist(transform_original, "beta", method="mle")
summary(fit.beta)
plot(fit.beta) 
gofstat(fit.beta)

transformbeta <- pbeta(transform_original, shape1=0.4051715, shape2=0.3804401, ncp=0)
expected_beta <- pbeta(data_beta, shape1=0.4051715, shape2=0.3804401, ncp=0)
par(mfrow=c(2,2))
cdfbeta <- ecdf(transformbeta) 
plot(cdfbeta) 
cdforiginal <- ecdf(transform_original) 
plot(cdforiginal) 
cdftransform <- ecdf(expected_beta)
plot(cdftransform)

par(mfrow=c(2,2))
hist(transformbeta)
hist(data_beta)
hist(transform_original)
hist(expected_beta)
ks.test(transformbeta, expected_beta)
qqplot(expected_beta, transformbeta)
abline(0,1)

##################################
#### Cube root data ~ Poisson ####
##################################

fit.poisson <- fitdist(FreedomHouse$cube, distr="pois", method="mle", lower=c(0,0))
fitdist(Original, 'pois', lower=c(0,0))$loglik
plot(fit.poisson)

expected_Poisson <- rpois(194, lambda=1.423657) 
ks.test(FreedomHouse$cube, expected_Poisson)

###############################
#### Square data ~ Poisson ####
###############################

fit.poisson <- fitdist(FreedomHouse$square, distr="pois", method="mle", lower=c(0,0))
fitdist(Original, 'pois', lower=c(0,0))$loglik
plot(fit.poisson)

mean(FreedomHouse$square)
expected_Poisson <- rpois(194, lambda=16.08247) 
ks.test(FreedomHouse$square, expected_Poisson)

###############################
###### Ln data ~ Poisson ######
###############################

fit.poisson <- fitdist(FreedomHouse$ln, distr="pois", method="mle", lower=c(0,0))
fitdist(Original, 'pois', lower=c(0,0))$loglik
plot(fit.poisson)

mean(FreedomHouse$ln)
expected_Poisson <- rpois(194, lambda=0.9696121) 
ks.test(FreedomHouse$ln, expected_Poisson)

###############################
#### Cube root data ~ Beta ####
###############################

transformed_cube <- pnorm(FreedomHouse$cube, mean=1.423657, sd=0.3418079)
mean(transformed_cube)
var(transformed_cube)
estBetaParams(0.5009815, 0.1027655)
expected_Beta <- rbeta(194, shape1=0.717763, shape2=0.7149506, ncp=0)
ks.test(transformed_cube, expected_Beta)

fit.beta <- fitdist(transformed_cube, "beta", method="mle")
fitdist(transformed_cube, 'beta')$loglik
summary(fit.beta)
plot(fit.beta) 
gofstat(fit.beta)

hist(transformed_cube)
hist(expected_Beta)


################################################
############### Residuals ######################
################################################
data_normal <-rnorm(194, mean=3.381443, sd=2.161573)
Original.lm = lm(Original ~ data_normal)
Original.res = resid(Original.lm)
plot(Original, Original.res)

##########################################

