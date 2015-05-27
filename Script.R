# Packages ----------------------------------------------------------------

library("ggplot2")


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

ggplot(data, aes(factor(spc), salary)) + geom_boxplot(aes(fill = factor(spc))) + 
  xlab("Socio-Professional Category") + ylab("Salary") + guides(fill=guide_legend(title=NULL)) + 
  ggtitle("Boxplot of the salary for each category") + theme

ggplot(data, aes(factor(spc), salary)) + geom_boxplot(aes(fill = factor(time))) +
  labs(x="SPC", y="Salary", title="Boxplot of the salary for each SPC and each time") + 
  theme + scale_fill_manual(name="Time", values=c("orange", "mediumpurple"), labels=c("0"="Part-time", 
  "1"="Full-time"))


##
dataH<-subset(data,sexe==1)
dataF<-subset(data,sexe==0)
dataTC<-subset(data,time==1)
dataTNC<-subset(data,time==0)
data.spc1<-subset(data,spc==1)
data.spc2<-subset(data,spc==2)
data.spc3<-subset(data,spc==3)
data.spc4<-subset(data,spc==4)
data.spc5<-subset(data,spc==5)


s.mean<-mean(data$salary,na.rm=TRUE)
s.mean.H<-mean(dataH$salary,na.rm=TRUE)
s.mean.F<-mean(dataF$salary,na.rm = TRUE)
s.mean.TC<-mean(dataTC$salary,na.rm = TRUE)
s.mean.TNC<-mean(dataTNC$salary,na.rm = TRUE)
s.mean.spc.1<-mean(data.spc1$salary,na.rm = TRUE)
s.mean.spc.2<-mean(data.spc2$salary,na.rm = TRUE)
s.mean.spc.3<-mean(data.spc3$salary,na.rm = TRUE)
s.mean.spc.4<-mean(data.spc4$salary,na.rm = TRUE)
s.mean.spc.5<-mean(data.spc5$salary,na.rm = TRUE)

vec.mean<-data.frame(s.mean,s.mean.H,s.mean.F,s.mean.TC,s.mean.TNC,s.mean.spc.1,
                     s.mean.spc.2,s.mean.spc.3,s.mean.spc.4,s.mean.spc.5)
colnames(vec.mean)<-c("Total","Male","Female","TC","TNC","SPC1","SPC2",
                      "SPC3","SPC4","SPC5")

ggplot(data, aes(factor(spc), salary)) + geom_bar()


barplot(c(s.mean,s.mean.H,s.mean.F),col=c(1,4,2),main = "Mean salary by gender", ylim=c(0,20) , ylab="Mean Salary",xlab="Total                                Male                                Female")

barplot(c(s.mean,s.mean.TC,s.mean.TNC),col=c(1,4,2),main = "Mean salary by time", ylim=c(0,20) , ylab="Mean Salary",xlab="Total                                TC                                TNC")

barplot(c(s.mean,s.mean.spc.1,s.mean.spc.2,s.mean.spc.3,s.mean.spc.4,s.mean.spc.5),col=c(1,2,3,4,5,6),main = "Mean salary by SPC", ylim=c(0,35) , ylab="Mean Salary",xlab="Total          SPC1           SPC2            SPC3            SPC4             SPC5")


data.t0.H.spc1<-subset(data,time==0 & sexe==1 & spc==1)
data.t0.H.spc2<-subset(data,time==0 & sexe==1 & spc==2)
data.t0.H.spc3<-subset(data,time==0 & sexe==1 & spc==3)
data.t0.H.spc4<-subset(data,time==0 & sexe==1 & spc==4)
data.t0.H.spc5<-subset(data,time==0 & sexe==1 & spc==5)
data.t0.F.spc1<-subset(data,time==0 & sexe==0 & spc==1)
data.t0.F.spc2<-subset(data,time==0 & sexe==0 & spc==2)
data.t0.F.spc3<-subset(data,time==0 & sexe==0 & spc==3)
data.t0.F.spc4<-subset(data,time==0 & sexe==0 & spc==4)
data.t0.F.spc5<-subset(data,time==0 & sexe==0 & spc==5)
data.t1.H.spc1<-subset(data,time==1 & sexe==1 & spc==1)
data.t1.H.spc2<-subset(data,time==1 & sexe==1 & spc==2)
data.t1.H.spc3<-subset(data,time==1 & sexe==1 & spc==3)
data.t1.H.spc4<-subset(data,time==1 & sexe==1 & spc==4)
data.t1.H.spc5<-subset(data,time==1 & sexe==1 & spc==5)
data.t1.F.spc1<-subset(data,time==1 & sexe==0 & spc==1)
data.t1.F.spc2<-subset(data,time==1 & sexe==0 & spc==2)
data.t1.F.spc3<-subset(data,time==1 & sexe==0 & spc==3)
data.t1.F.spc4<-subset(data,time==1 & sexe==0 & spc==4)
data.t1.F.spc5<-subset(data,time==1 & sexe==0 & spc==5)

time.m<-c(rep(0,10),rep(1,10))
sexe.m<-c(rep(1,5),rep(0,5),rep(1,5),rep(0,5))
spc.m<-c(rep(1:5,4))

s.mean.t0.H.spc1<-mean(data.t0.H.spc1$salary,na.rm=TRUE)
s.mean.t0.H.spc2<-mean(data.t0.H.spc2$salary,na.rm=TRUE)
s.mean.t0.H.spc3<-mean(data.t0.H.spc3$salary,na.rm=TRUE)
s.mean.t0.H.spc4<-mean(data.t0.H.spc4$salary,na.rm=TRUE)
s.mean.t0.H.spc5<-mean(data.t0.H.spc5$salary,na.rm=TRUE)
s.mean.t0.F.spc1<-mean(data.t0.F.spc1$salary,na.rm=TRUE)
s.mean.t0.F.spc2<-mean(data.t0.F.spc2$salary,na.rm=TRUE)
s.mean.t0.F.spc3<-mean(data.t0.F.spc3$salary,na.rm=TRUE)
s.mean.t0.F.spc4<-mean(data.t0.F.spc4$salary,na.rm=TRUE)
s.mean.t0.F.spc5<-mean(data.t0.F.spc5$salary,na.rm=TRUE)
s.mean.t1.H.spc1<-mean(data.t1.H.spc1$salary,na.rm=TRUE)
s.mean.t1.H.spc2<-mean(data.t1.H.spc2$salary,na.rm=TRUE)
s.mean.t1.H.spc3<-mean(data.t1.H.spc3$salary,na.rm=TRUE)
s.mean.t1.H.spc4<-mean(data.t1.H.spc4$salary,na.rm=TRUE)
s.mean.t1.H.spc5<-mean(data.t1.H.spc5$salary,na.rm=TRUE)
s.mean.t1.F.spc1<-mean(data.t1.F.spc1$salary,na.rm=TRUE)
s.mean.t1.F.spc2<-mean(data.t1.F.spc2$salary,na.rm=TRUE)
s.mean.t1.F.spc3<-mean(data.t1.F.spc3$salary,na.rm=TRUE)
s.mean.t1.F.spc4<-mean(data.t1.F.spc4$salary,na.rm=TRUE)
s.mean.t1.F.spc5<-mean(data.t1.F.spc5$salary,na.rm=TRUE)

salary.m<-c(s.mean.t0.H.spc1,s.mean.t0.H.spc2,s.mean.t0.H.spc3,s.mean.t0.H.spc4,s.mean.t0.H.spc5,
            s.mean.t0.F.spc1,s.mean.t0.F.spc2,s.mean.t0.F.spc3,s.mean.t0.F.spc4,s.mean.t0.F.spc5,
            s.mean.t1.H.spc1,s.mean.t1.H.spc2,s.mean.t1.H.spc3,s.mean.t1.H.spc4,s.mean.t1.H.spc5,
            s.mean.t1.F.spc1,s.mean.t1.F.spc2,s.mean.t1.F.spc3,s.mean.t1.F.spc4,s.mean.t1.F.spc5)

data.m<-data.frame(time.m,sexe.m,spc.m,salary.m)


ggplot(data.m, aes(factor(spc.m), salary.m,)) + geom_bar(stat = "identity")


# Missings ----------------------------------------------------------------

length(which(is.na(data)))

