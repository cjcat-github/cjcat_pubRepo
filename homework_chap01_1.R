#第一个作业
setwd("E://上课//数据可视化//data")
getwd()

install.packages("boot")
library("boot")
data("calcium")

#--计算总体均值---------------
cal_m = mean(calcium$cal)


#--仿真N=10000次(10000个样本)，计算x_bar[10000]
#--simulation----------
N = 10000
x_bar = matrix(0,nrow=N,ncol=1) #x_bar[10000]
m_diff = matrix(0,nrow=N,ncol=1) #m_diff[10000]
for(i in 1:N){  #R的for循环
  x <- sample(calcium$cal,size=100,replace=TRUE);
  x_bar[i] = mean(x)
  m_diff[i] = cal_m-x_bar[i]
}

windows()
par(mfrow=c(2,1))
plot(m_diff,xlab="index",ylab="差异")
hist(m_diff,breaks = 20,col="grey80",main="正态曲线")

#--第2次作业--------------
data(amis)
amis$pair<-as.factor(amis$pair)
amis$warning<-as.factor(amis$warning)
table(amis$pair)
table(amis$pair,amis$warning)