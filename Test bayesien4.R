
# Packages ----------------------------------------------------------------

#install.packages("arm")
#install.packages("BLR")
#install.packages("bayesm")
#install.packages("BayesFactor")
#install.packages("R2WinBUGS")
#install.packages("Hmisc")

library("Hmisc")
library("R2WinBUGS")
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

blr.s <- BLR(dataCC$salary)
summary(blr.s)
blr.s$prior

#install.packages("MCMCpack")
library("MCMCpack")
breg <- MCMCregress(salary ~ sexe+spc+time, data)
summary(breg)
windows()
plot(breg)

?dgamma


path.bug<-"C:/Users/Maxime/Desktop/Cours ERASMUS/Bayesian Analysis/Project/Bayesian-Project/"
path.WBS<-"C:/Users/Maxime/Desktop/Cours ERASMUS/Bayesian Analysis/Project/Bayesian-Project/winbugs14/WinBUGS14/"

Thin<-1
Burn<-500
Chain<-2
Iter<-1000
n <- nrow(data)

dataB <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
param <- c("alpha", "beta1", "beta2", "beta3", "tau","mu")
inits <- list(list(tau=1), list(tau=5))
mod1.4 <- bugs(dataB, inits=inits, parameters.to.save=param,
               model=paste(path.bug,"modelWin.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.4, digits=4)


dataB3 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
para3 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "mu")
inits3 <- list(list(tau=1), list(tau=5))
mod1.42 <- bugs(dataB3, inits=inits3, parameters.to.save=para3,
                model=paste(path.bug,"modelWin22.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.42, digits=4)


dataB2 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
para2 <- c("alpha", "beta1", "beta2", "beta3", "tau", "tau2", "mu")
inits2 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
mod1.5 <- bugs(dataB2, inits=inits2, parameters.to.save=para2,
               model=paste(path.bug,"modelWin21.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.5, digits=4)


dataB4 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
para4 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "tau2", "mu")
inits4 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
mod1.52 <- bugs(dataB4, inits=inits4, parameters.to.save=para4,
                model=paste(path.bug,"modelWin212.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.52, digits=4) # random effect spc

dataB5 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
para5 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "tau2", "mu")
inits5 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
mod1.53 <- bugs(dataB5, inits=inits5, parameters.to.save=para5,
                model=paste(path.bug,"modelWin213.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.53, digits=4) ## random effects sexe

dataB6 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
para6 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "tau2", "mu")
inits6 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
mod1.54 <- bugs(dataB6, inits=inits6, parameters.to.save=para6,
                model=paste(path.bug,"modelWin214.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.54, digits=4) ## random effects time

dataB7 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
para7 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "tau2","tau3", "mu")
inits7 <- list(list(tau=1,tau2=1,tau3=1), list(tau=2,tau2=2,tau3=2))
mod1.55 <- bugs(dataB7, inits=inits7, parameters.to.save=para7,
                model=paste(path.bug,"modelWin215.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.55, digits=4) ## random effects time and sexe

dataB8 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
para8 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "tau2","tau3","tau4", "mu")
inits8 <- list(list(tau=1,tau2=1,tau3=1,tau4=1), list(tau=2,tau2=2,tau3=2,tau4=2))
mod1.56 <- bugs(dataB7, inits=inits8, parameters.to.save=para8,
                model=paste(path.bug,"modelWin216.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.56, digits=4) ## random effects time, sexe and spc

dataB9 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
para9 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "tau2","tau3", "mu")
inits9 <- list(list(tau=1,tau2=1,tau3=1), list(tau=2,tau2=2,tau3=2))
mod1.57 <- bugs(dataB9, inits=inits9, parameters.to.save=para9,
                model=paste(path.bug,"modelWin217.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.57, digits=4) ## random effects spc and sexe

dataB10 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, n = nrow(data))
para10 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "tau2","tau3", "mu")
inits10 <- list(list(tau=1,tau2=1,tau3=1), list(tau=2,tau2=2,tau3=2))
mod1.58 <- bugs(dataB10, inits=inits10, parameters.to.save=para10,
                model=paste(path.bug,"modelWin218.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod1.58, digits=4) ## random effects spc and time


dataB11 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, spc2=(data$spc)^2, n = nrow(data))
para11 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau", "mu")
inits11 <- list(list(tau=1), list(tau=5))
mod2 <- bugs(dataB11, inits=inits11, parameters.to.save=para11,
             model=paste(path.bug,"modelWin219.bug",sep=""),
             bugs.directory=path.WBS,               
             n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod2, digits=4) #spc² + spc:sexe

dataB111 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, spc2=(data$spc)^2, n = nrow(data))
para111 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "mu")
inits111 <- list(list(tau=1), list(tau=5))
mod2111 <- bugs(dataB111, inits=inits111, parameters.to.save=para111,
                model=paste(path.bug,"modelWin2191.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod2111, digits=4) #spc²


dataB12 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, spc2=(data$spc)^2, n = nrow(data))
para12 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2", "mu")
inits12 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
mod2.1 <- bugs(dataB12, inits=inits12, parameters.to.save=para12,
               model=paste(path.bug,"modelWin3.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod2.1, digits=4) #spc² random effect time

dataB12 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, spc2=(data$spc)^2, n = nrow(data))
para12 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2", "mu")
inits12 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
mod2.2 <- bugs(dataB12, inits=inits12, parameters.to.save=para12,
               model=paste(path.bug,"modelWin3.1.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod2.2, digits=4) #spc² random effect sexe

dataB12 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, spc2=(data$spc)^2, n = nrow(data))
para12 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2", "mu")
inits12 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
mod2.3 <- bugs(dataB12, inits=inits12, parameters.to.save=para12,
               model=paste(path.bug,"modelWin3.2.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod2.3, digits=4) #spc² random effect spc


dataB12 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, spc2=(data$spc)^2, n = nrow(data))
para12 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2", "mu")
inits12 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
mod2.4 <- bugs(dataB12, inits=inits12, parameters.to.save=para12,
               model=paste(path.bug,"modelWin3.3.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod2.4, digits=4) #spc² random effect spc2


dataB13 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, spc2=(data$spc)^2, n = nrow(data))
para13 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2","tau3", "mu")
inits13 <- list(list(tau=1,tau2=1,tau3=1), list(tau=2,tau2=2,tau3=2))
mod2.5 <- bugs(dataB13, inits=inits13, parameters.to.save=para13,
               model=paste(path.bug,"modelWin3.4.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod2.5, digits=4) #spc² random effect spc² et intercept


dataB14 <- list(salary = data$salary,time=data$time, sexe=data$sexe, spc=data$spc, spc2=(data$spc)^2, n = nrow(data))
para14 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2","tau3", "mu")
inits14 <- list(list(tau=1,tau2=1,tau3=1), list(tau=2,tau2=2,tau3=2))
mod2.6 <- bugs(dataB14, inits=inits14, parameters.to.save=para14,
               model=paste(path.bug,"modelWin3.5.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(mod2.6, digits=4) #spc² random effect sexe and intercept



# Model selection ---------------------------------------------------------

print(mod1.4, digits=4) ## 3455.6 time+sexe+spc
print(mod2111, digits=4) ## 2988.0 time+sexe+spc+spc2
print(mod1.42, digits=4) ## 3444.8 time+sexe+spc+spc:sexe
print(mod2, digits=4) ## 2962.9 time+sexe+spc+spc2+spc:sexe

DIC<-c(3455.6,2988.0,3444.8,2962.9)

pred1.4 <- mod1.4$mean$mu
res1.4 <- pred1.4 - salary
s1<-sum(res1.4^2,na.rm=TRUE)


pred2111 <- mod2111$mean$mu
res2111 <- pred2111 - salary
s2<-sum(res2111^2,na.rm=TRUE)

pred1.42 <- mod1.42$mean$mu
res1.42 <- pred1.42 - salary
s3<-sum(res1.42^2,na.rm=TRUE)

pred2 <- mod2$mean$mu
res2 <- pred2 - salary
s4<-sum(res2^2,na.rm=TRUE)

res<-c(s1,s2,s3,s4)

tab1<-data.frame(DIC,res)

## with RE

print(mod2.2, digits=4) ## 2881.2 RE on sexe
print(mod2.1, digits=4) ## 2964.1 RE on time
print(mod2.3, digits=4) ## 2970.0 RE on spc
print(mod2.4, digits=4) ## 2986.1 RE on spc2
print(mod2.5, digits=4) ## 2869.3 RE on spc2 and intercept
print(mod2.6, digits=4) ## 2682.4 RE on sexe and intercept

DIC2<-c(2881.2,2964.1,2970.0,2986.1,2869.3,2682.4)

pred2.2 <- mod2.2$mean$mu
res2.2 <- pred2.2 - salary
s12<-sum(res2.2^2,na.rm=TRUE)


pred2.1 <- mod2.1$mean$mu
res2.1 <- pred2.1 - salary
s22<-sum(res2.1^2,na.rm=TRUE)

pred2.3 <- mod2.3$mean$mu
res2.3 <- pred2.3 - salary
s32<-sum(res2.3^2,na.rm=TRUE)

pred2.4 <- mod2.4$mean$mu
res2.4 <- pred2.4 - salary
s42<-sum(res2.4^2,na.rm=TRUE)


pred2.5 <- mod2.5$mean$mu
res2.5 <- pred2.5 - salary
s52<-sum(res2.5^2,na.rm=TRUE)


pred2.6 <- mod2.6$mean$mu
res2.6 <- pred2.6 - salary
s62<-sum(res2.6^2,na.rm=TRUE)

res2<-c(s12,s22,s32,s42,s52,s62)

tab2<-data.frame(DIC2,res2)




## mod2.6 better
print(mod2.6, digits=4) #spc² random effect sexe and intercept 





# prediction --------------------------------------------------------------

pred2.6 <- mod2.6$mean$mu
res2.6 <- pred2.6 - salary
sum(res2.6^2,na.rm=TRUE)
res2.6<-subset(res2.6,!is.na(res2.6))

hist(res2.6, freq=FALSE)
lines(density(res2.6, adjust = 1), col=2)





pred2.6.cuts <- cut2(pred2.6, cuts=c(quantile(pred2.6, c(0,0.2,0.4,0.6,0.8,1), na.rm=TRUE)))
levels(pred2.6.cuts) <- c("1","2","3","4","5")
sum(ifelse(pred2.6.cuts, 1, 0), na.rm=TRUE)
par(mfrow=c(1,1),mar=c(3,3,3,3))
plot(1:73, pred2.6, pch=16, xlab="", ylab="", main="Mean Posterior Predictive", ylim=c(600,2000))
abline(v=c(4, 10, 18, 21, 27, 32, 43, 56, 63), col="blue", lty=2)
points(1:73, rent, col="gold", pch=16)
legend("topright", c("Predicted Rent", "Actual Rent"), pch=c(16,16), col=c(1, "gold"), cex=0.9)
