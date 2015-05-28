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

reg <- lm(salary~time+sexe*spc, data=data)
summary(reg)

table.reg <- data.frame(coefficients(reg),summary(reg)[[4]][,4])
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
