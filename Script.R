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
