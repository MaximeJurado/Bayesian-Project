
# Packages ----------------------------------------------------------------

#install.packages("arm")
#install.packages("BLR")
#install.packages("bayesm")
#install.packages("BayesFactor")

library("arm")
library("BLR")
library("bayesm")
library("BayesFactor")
library("lme4")

# Data --------------------------------------------------------------------

data <- read.csv2("Data/data.csv",header=TRUE)
data <- subset(data,time!="TP")

data1 <- data[rep(1:nrow(data),each=5),]
data2 <- data.frame(data$X1,data$X2,data$X3,data$X4,data$X5)
data3 <- data.frame(data1$id,data1$time,data1$sexe)

vec <- NULL
for(i in 1:nrow(data2)){
  vec1 <- data2[i,]
  vec <- c(vec,t(vec1))
}

vec <- as.data.frame(vec)
data4 <- data.frame(data3,vec)
cat <- c(rep(c(1,2,3,4,5),132))

data <- data.frame(data4,cat)
colnames(data) <- c("id","time","sexe","salary","spc")
attach(data)
salary<-as.numeric(salary)
detach(data)

# Bayesian methods --------------------------------------------------------


## bayesm seems to be appropriate

?BLR

summary(lm(salary~ time*sexe*spc
           -time:sexe:spc
           -time:sexe
           -time:spc
           ,data=data))

summary(glmer(salary~ time*sexe*spc
              -time:sexe:spc
              -time:sexe
              -time:spc
              + (1|id),data=data))


mu <- mean(data$salary,na.rm=TRUE)
mu
sigma <- sd(data$salary,na.rm=TRUE)
sigma
coef <- sigma/mu

# Coef is <1 so we select probability distributions from the gamma family.

# We want alpha/beta=mu and alpha/beta^2=sigma^2.

plot(function(x)dgamma(x,46,2.5), xlim=c(0,35),ylim=c(0,0.2),col='red',
     ylab="",xlab="",lwd=2,main="Possible probability distributions.")

# Same thing with the density estimate of the data.
plot(density(data$salary,na.rm=TRUE),ylim=c(0,0.15),lwd=2,add=TRUE)
plot(function(x)(dgamma(x,9,0.6)+dgamma(x,4,0.125)),xlim=c(0,40),col='red',ylab="",xlab="",
     lwd=2,add=TRUE)
legend(35,0.08,col=c('red','black'),lwd=c(2,2),lty=1,
       legend=c("Gamma(46,2.5)","Data"))

# Test of the goodness of the fit -----------------------------------------

# QQ-plot
n <- length(data$salary)
probabilities <- (1:(n-44))/(n+1)
gamma.quantiles <- qgamma(probabilities,15,1)

plot(sort(gamma.quantiles), sort(data$salary), xlab = 'Theoretical quantiles from 
     a Gamma(6,0.6)',ylab = 'Sample quantiles of the data', 
     main = 'Gamma quantile-quantile plot of the data',col='blue')
abline(0,1)

# KS test
kstest <- ks.test(data$salary,"pgamma",shape=46,rate=2.5)
kstest
# The warning means that we have some duplicate in the data which is usually 
# not possible for a continuous distribution.

duplicates <- duplicated(data$salary)
datau <- data$salary[!duplicated(data$salary)]
length(datau)

ks.test(datau,"pgamma",shape=46,rate=2.5)




dataCC<-subset(data,!is.na(salary))

lmBF(salary ~ sexe*time*spc, data=dataCC)
summary(aov(salary~sexe*time*spc
            -time:sexe:spc
            -time:sexe
            -time:spc
            ,data=data))

bayesglm(salary~time*spc*sexe
         -time:sexe:spc
         -time:sexe
         -time:spc
         ,data=dataCC)

regressionBF(salary~time*sexe*spc,data=dataCC)

dataCC<-subset(data,!is.na(data$salary))
dataCC<-data.frame(dataCC$id,dataCC$sexe,dataCC$time,dataCC$spc,dataCC$salary)
colnames(dataCC)<-c("id","sexe","time","spc","salary")

attach(dataCC)
time<-as.factor(time)
sexe<-as.factor(sexe)
spc<-as.factor(spc)
detach(dataCC)

BLR(dataCC$salary)


#install.packages("MCMCpack")
library("MCMCpack")
breg <- MCMCregress(salary ~ sexe+spc+time, data)
summary(breg); plot(breg)
