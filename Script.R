# Packages ----------------------------------------------------------------

library("ggplot2")
library("xtable")


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
sexe <- as.factor(sexe)
time <- as.factor(time)
spc <- as.factor(spc)


# Descriptive analysis ----------------------------------------------------

theme <- theme(plot.title = element_text(size=16, face="bold"), axis.title.x = element_text(
              size=15), axis.title.y = element_text(size=15), axis.text.x = element_text(
              size=15, face="bold"),axis.text.y = element_text(size=15, face="bold"))

# SPC
ggplot(data, aes(factor(spc), salary)) + geom_boxplot(aes(fill = factor(spc))) + 
  xlab("Socio-Professional Category") + ylab("Salary") + guides(fill=guide_legend(title=NULL)) + 
  ggtitle("Boxplot of the salary for each category") + theme

datamean <- aggregate(data.frame(salaryMean = data$salary), by = list(id = data$id, spc = data$spc), 
                      mean)

ggplot(datamean, aes(spc, salaryMean)) + facet_wrap(facets=~ id, ncol=10) + 
  geom_line(colour="blue") + geom_point(colour="red") + theme

# Time
ggplot(data, aes(factor(time), salary)) + geom_boxplot(aes(fill = factor(time))) +
  labs(x="Time", y="Salary", title="Boxplot of the salary for each time") + theme +
  scale_fill_manual(name="Time", values=c("orange", "mediumpurple"), labels=c("0"="Part-time", 
  "1"="Full-time"))
  
ggplot(data, aes(factor(spc), salary)) + geom_boxplot(aes(fill = factor(time))) +
  labs(x="SPC", y="Salary", title="Boxplot of the salary for each SPC and each time") + 
  theme + scale_fill_manual(name="Time", values=c("orange", "mediumpurple"), labels=c("0"="Part-time", 
  "1"="Full-time"))

## Test on the differences between time1 and time0.
data.sort <- data[with(data, order(id, spc)), ]
row.names(data.sort) <- c(1:nrow(data.sort))
data.sort.time0 <- NULL
data.sort.time1 <- NULL
for(i in 1:nrow(data.sort)){
  newline <- data.sort[i,]
  if(i%%2==0){
    data.sort.time0 <- rbind(data.sort.time0, newline)
  }
  else{
    data.sort.time1 <- rbind(data.sort.time1, newline)
  }
}
data.time.paired <- as.data.frame(cbind(data.sort.time0$spc,data.sort.time0[,4],data.sort.time1[,4]))
colnames(data.time.paired) <- c("spc", "salary0", "salary1")

test.time.all <- t.test(data.time.paired[,2], data.time.paired[,3], paired=TRUE)

data.time.paired.spc1 <- subset(data.time.paired, spc==1)
data.time.paired.spc2 <- subset(data.time.paired, spc==2)
data.time.paired.spc3 <- subset(data.time.paired, spc==3)
data.time.paired.spc4 <- subset(data.time.paired, spc==4)
data.time.paired.spc5 <- subset(data.time.paired, spc==5)

test.time.spc1 <- t.test(data.time.paired.spc1[,2], data.time.paired.spc1[,3], paired=TRUE)
test.time.spc2 <- t.test(data.time.paired.spc2[,2], data.time.paired.spc2[,3], paired=TRUE)
test.time.spc3 <- t.test(data.time.paired.spc3[,2], data.time.paired.spc3[,3], paired=TRUE)
test.time.spc4 <- t.test(data.time.paired.spc4[,2], data.time.paired.spc4[,3], paired=TRUE)
test.time.spc5 <- t.test(data.time.paired.spc5[,2], data.time.paired.spc5[,3], paired=TRUE)

table.test.time <- data.frame(c("dataset","category 1","category 2","category 3","category 4",
                   "category 5"),c(test.time.all$p.value,test.time.spc1$p.value,
                   test.time.spc2$p.value,test.time.spc3$p.value,test.time.spc4$p.value,
                   test.time.spc5$p.value), c(test.time.all$estimate,test.time.spc1$estimate,
                   test.time.spc2$estimate,test.time.spc3$estimate,test.time.spc4$estimate,
                   test.time.spc5$estimate))
colnames(table.test.time) <- c("","p-value","mean of the differences")
print(xtable(table.test.time, align=c("c","c","c","c"), caption="P-values and mean of the differences 
             of the tests. \\label{tabletesttime}", digits=3))

salary.mean.spc <- aggregate(data.frame(salaryMean=data$salary),by=list(time=data$time,
                             spc=data$spc),mean,na.rm=TRUE)
(table.test.time[2,3]/salary.mean.spc[2,3])*100

# Sexe
data.m <- aggregate(data.frame(salary.m = data$salary), by = list(sexe = data$sexe, 
                    spc = data$spc,time = data$time), mean, na.rm=TRUE)

ggplot(subset(data.m,time==0),aes(x=factor(sexe),y=salary.m,fill=factor(sexe))) + 
  geom_bar(stat = "identity") + facet_wrap(~ spc) + 
  labs(title="Mean salary by gender and spc at time=0 (Part-Time)", x="Sexe", y="Mean salary") +
  scale_fill_discrete(name="Gender",  labels=c("Female", "Male"),guide = guide_legend(reverse=TRUE)) + 
  theme(plot.title = element_text(size=16, face="bold"), legend.title = element_text(colour="black",
  size=18, face="bold"),legend.position=c(.85,.15),legend.background = element_rect(size=25),
  legend.text = element_text(size = 16))
 
ggplot(subset(data.m,time==0),aes(x=factor(sexe),y=salary.m,fill=factor(sexe))) + 
  geom_bar(stat = "identity") + facet_wrap(~ spc) + 
  labs(title="Mean salary by gender and spc at time=1 (Full Time)", x="Sexe", y="Mean salary") +
  scale_fill_discrete(name="Gender",  labels=c("Female", "Male"),guide = guide_legend(reverse=TRUE)) + 
  theme(plot.title = element_text(size=16, face="bold"),legend.title = element_text(colour="black",
  size=18, face="bold"),legend.position=c(.85,.15),legend.background = element_rect(size=25),
  legend.text = element_text(size = 16))

ggplot(data.m,aes(x=factor(sexe),y=salary.m,fill=factor(sexe))) + geom_bar(stat = "identity") + 
  facet_wrap(~ spc) + labs(title="Mean salary by gender and spc", x="Sexe", y="Mean salary") + 
  scale_fill_discrete(name="Gender",labels=c("Female","Male"),guide = guide_legend(reverse=TRUE)) +
  theme(plot.title = element_text(size=16, face="bold"),legend.title = element_text(colour="black",
  size=18, face="bold"),legend.position=c(.85,.15),legend.background = element_rect(size=25),
  legend.text = element_text(size = 16))

## Test on the differences between time1 and time0.
data.sort2 <- data[with(data, order(id, spc, time)), ]
row.names(data.sort2) <- c(1:nrow(data.sort2))
data.sort.sexe0 <- NULL
data.sort.sexe1 <- NULL
for(i in 1:nrow(data.sort2)){
  newline <- data.sort2[i,]
  if(i%%2==0){
    data.sort.sexe0 <- rbind(data.sort.sexe0, newline)
  }
  else{
    data.sort.sexe1 <- rbind(data.sort.sexe1, newline)
  }
}
data.sexe.paired <- as.data.frame(cbind(data.sort.sexe0$spc,data.sort.sexe0[,4],data.sort.sexe1[,4]))
colnames(data.sexe.paired) <- c("spc", "salary0", "salary1")

test.sexe.all <- t.test(data.sexe.paired[,2], data.sexe.paired[,3], paired=TRUE)

# Linear regression
data.spc2 <- cbind(data,data$spc^2)
colnames(data.spc2) <- c("id","time","sexe","salary","spc","spc2")

reg <- lm(salary~time+sexe*spc, data=data)
summary(reg)

reg.spc2 <- lm(salary~time+sexe*spc+spc2, data=data.spc2)
summary(reg.spc2)

anova(reg,reg.spc2)

table.reg <- data.frame(coefficients(reg.spc2),summary(reg.spc2)[[4]][,4])
colnames(table.reg) <- c("estimates","p-value")


# Missings ----------------------------------------------------------------

dataMissing <- data[which(is.na(data$salary)),]

length(dataMissing)

id1 <- data[which(data$id==1),]
# Missing in the data
id8 <- data[which(data$id==8),]
# Secret data
id22 <- data[which(data$id==22),]
# Missing in the data


# Bayesian model ----------------------------------------------------------

library(R2WinBUGS)

path.bug <- "C:/Users/Mathieu/Documents/Cours/2A/Erasmus/Cours/Bayesian analysis/Bayesian-Project/modelBug/"
path.WBS <- "C:/Users/Mathieu/Documents/Logiciels/WinBuggs/WinBUGS14/"

Iter <- 1000
Burn <- 500
Chain <- 2
Thin <- 1
n <- nrow(data)

datalist <- list(salary=data$salary, time=data$time, sexe=data$sexe, spc=data$spc, n=n)
datalist2 <- list(salary=data$salary,time=data$time, sexe=data$sexe, spc=data$spc, 
                  spc2=(data$spc)^2, n=n)

# sexe,time and spc
parameters10 <- c("alpha","beta1","beta2","beta3","tau","mu")
inits10 <- list(list(tau=1),list(tau=5))
model10 <- bugs(datalist,inits=inits10,parameters.to.save=parameters10,
               model=paste(path.bug,"modelWin10.bug",sep=""),bugs.directory=path.WBS,
               n.iter=(Iter*Thin+Burn),n.burnin=Burn,n.thin=Thin,n.chains=Chain, DIC=F,debug=T)
print(model10, digits=4)
# DIC=3455.6

# Sexe, time, spc and spc²
parameters11 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "mu")
inits11 <- list(list(tau=1), list(tau=5))
model11 <- bugs(datalist2, inits=inits11, parameters.to.save=parameters11,
                model=paste(path.bug,"modelWin11.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(model11, digits=4)
# DIC=2987.5

# sexe, time, spc and spc*sexe
parameters12 <- c("alpha", "beta1", "beta2", "beta3","beta4", "tau", "mu")
inits12 <- list(list(tau=1), list(tau=5))
model12 <- bugs(datalist, inits=inits12, parameters.to.save=parameters12,
                model=paste(path.bug,"modelWin12.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(model12, digits=4)
# DIC=3444.8

# sexe, time, spc, spc*sexe and spc²
parameters13 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau", "mu")
inits13 <- list(list(tau=1), list(tau=5))
model13 <- bugs(datalist2, inits=inits13, parameters.to.save=parameters13,
             model=paste(path.bug,"modelWin13.bug",sep=""),
             bugs.directory=path.WBS,               
             n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(model13, digits=4) 
# DIC=2962.9

results <- t(as.data.frame(c(model13$mean[1],model13$mean[2],model13$mean[3],model13$mean[4],
             model13$mean[5],model13$mean[6])))
colnames(results) <- c("mean")
rownames(results) <- c("intercept", "time", "sexe", "spc", "spc*sexe", "spc²")

# Random effects on time
parameters21 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2", "mu")
inits21 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
model21 <- bugs(datalist2, inits=inits21, parameters.to.save=parameters21,
               model=paste(path.bug,"modelWin21.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(model21, digits=4) 
# DIC=2964.1

# Random effects on sexe
parameters22 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2", "mu")
inits22 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
model22 <- bugs(datalist2, inits=inits22, parameters.to.save=parameters22,
               model=paste(path.bug,"modelWin22.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(model22, digits=4) 
# DIC=2881.2

# Random effects on spc
parameters23 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2", "mu")
inits23 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
model23 <- bugs(datalist2, inits=inits23, parameters.to.save=parameters23,
               model=paste(path.bug,"modelWin23.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(model23, digits=4)
# DIC=2970.0

# Random effects on spc²
parameters24 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2", "mu")
inits24 <- list(list(tau=1,tau2=1), list(tau=2,tau2=2))
model24 <- bugs(datalist2, inits=inits24, parameters.to.save=parameters24,
               model=paste(path.bug,"modelWin24.bug",sep=""),
               bugs.directory=path.WBS,               
               n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(model24, digits=4)
# DIC=2986.1

# Random effects sexe and intercept
parameters221 <- c("alpha", "beta1", "beta2", "beta3","beta4","beta5", "tau","tau2", "tau3", "mu")
inits221 <- list(list(tau=1,tau2=1, tau3=1), list(tau=2,tau2=2, tau3=2))
model221 <- bugs(datalist2, inits=inits221, parameters.to.save=parameters221,
                model=paste(path.bug,"modelWin221.bug",sep=""),
                bugs.directory=path.WBS,               
                n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, DIC=T, debug=T)
print(model221, digits=4) 
# DIC=2682.4

results2 <- t(as.data.frame(c(model221$mean[1],model221$mean[2],model221$mean[3],model221$mean[4],
                             model221$mean[5],model221$mean[6])))
colnames(results2) <- c("mean")
rownames(results2) <- c("intercept", "time", "sexe", "spc", "spc*sexe", "spc²")
