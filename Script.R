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

# TODO: test whether time1>time0 or not.

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

# TODO: test whether sexe1>sexe0 or not.

# Missings ----------------------------------------------------------------

dataMissing <- data[which(is.na(data$salary)),]

length(dataMissing)

id1 <- data[which(data$id==1),]
# Missing in the data
id8 <- data[which(data$id==8),]
# Secret data
id22 <- data[which(data$id==22),]
# Missing in the data


