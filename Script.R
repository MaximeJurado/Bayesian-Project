# Packages ----------------------------------------------------------------

#install.packages("xlsx")
library(xlsx)

# Data --------------------------------------------------------------------

data <- read.xlsx("Data/data.xslx",sheetIndex=1,header=TRUE)
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
data$time <- factor(data$CE, levels = c("TNC", "TC"))
colnames(data) <- c("id","time","sexe","salary","spc")

attach(data)

salary<-as.numeric(salary)

# Descriptive analysis ----------------------------------------------------

boxplot(salary,data=data)
lapply(data,class)
