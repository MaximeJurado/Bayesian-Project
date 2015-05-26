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


boxplot(salary, data=data)
boxplot(salary~time,data=data)
boxplot(salary~sexe,data=data)
boxplot(salary~spc,data=data)

boxplot(salary~time+sexe+spc,data=data)

?barplot

dataH<-subset(data,sexe=="H")
dataF<-subset(data,sexe=="F")

s.mean<-mean(data$salary,na.rm=TRUE)
s.mean.H<-mean(dataH$salary,na.rm=TRUE)
s.mean.F<-mean(dataF$salary,na.rm = TRUE)

?barplot
barplot(c(s.mean,s.mean.H,s.mean.F),col=c(1,4,2),main = "Mean salary by gender", ylim=c(0,20) , ylab="Mean Salary",xlab="Gender")
legend("bottom", c("Total","Male","Female")
       , lty=c(1,1,1) , lwd=c(2,2,2), 
       col=c(1,4,2))
