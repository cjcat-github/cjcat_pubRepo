#### 类别数据的频数分布的可视化###########################

###3.1 一维列联表的可视化############################

#---简单条形图----------------
setwd("E://上课//数据可视化//data//mydata")
data1.df<-read.csv(".//chap01//data1_1.csv")

attach(data1.df)
count_sex<-table(性别)
count_shop<-table(网购次数)
count_saf<-table(满意度)

#--Exampl1----------
windows()
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
par(mai=c(0.6,0.6,0.4,0.1),cex=0.7)
barplot(count_sex,xlab="性别",ylab="人数",
        density=30,angle=0,col=c("grey80","grey50"),
        border="blue",main="(a)垂直条形图")
barplot(count_sex,xlab="人数",ylab="性别",,horiz=TRUE,
        density=20,angle=90,col=c("yellow","green"),
        main="(b)水平条形图")
barplot(count_shop,xlab="网购次数",ylab="人数",
        col=2:4,main ="(c)网购的统计图")
barplot(count_saf,xlab="满意度",ylab="人数",
        col=2:4,main="(d)满意度的统计图")
detach()

#--Example2----------
windows()
par(mfrow=c(1,2),mai=c(0.7,0.7,0.2,0.1),cex=0.7)
bar<-barplot(count_shop,col=grey.colors(3),
             ylim=c(0,1.5*max(count_shop)),
             xlab="网购次数",ylab="人数",
             cex.names=0.8,main="(a)添加频数标签")
text(bar,count_shop,labels = count_shop,pos=3)
bar<-barplot(count_saf,ylim=c(0,1.5*max(count_saf)),
             xlab="满意度",ylab="人数",
             cex.names=0.8, col=c("grey50","grey70","grey90"),
             main="(b)添加频数折线")
text(bar,count_saf,labels=count_saf,pos=3)
lines(bar,count_saf,type="o",col="red",pch=19,lwd=3)
points(bar,count_saf,type="h",col="red",lwd=3,lty=6)

#--Example3: sjPlot包---
install.packages("sjPlot")
library(sjPlot)
windows()
set_theme(axis.textsize = 0.8,    #坐标轴刻度字体大小
          axis.title.size = 0.8,  #坐标轴标题字体大小
          geom.label.size = 2.5)  #图形标签字体大小
plot_frq(data1.df$满意度,type="bar",show.n=TRUE,show.prc=TRUE,
         axis.labels=c("not sat","sat","very sat"),
         axis.title="satisfied degree")

#--Pareto图------------
windows()
par(mai=c(1,1,0.5,1.5))
x<-sort(table(data1.df$满意度),decreasing=TRUE)#一维列联表按频数降序排列
bar<barplot(x,xlab="满意度",ylab="人数",
            col=rainbow(3),ylim=c(0,1000))
text(bar,x,labels=x,pos=3,col=1)
y<-cumsum(x)/sum(x)
par(new=T)
plot(y,type="b",pch=15,axes=FALSE,ann=FALSE,
     ylim=c(0,1.0))
axis(side=4)
mtext("累积频率",side=4,line=3,cex=0.8)
text(labels="累积分布曲线",x=2.0,y=0.95,cex=1)

### 3.2 二维列联表的可视化############################
#---图3.5的绘制代码-----------
mytable1<-table(data1.df$网购次数,data1.df$满意度)
addmargins(mytable1)

windows()
par(mfrow=c(2,1),mai=c(0.6,0.6,0.3,0.1),cex=0.6)
barplot(mytable1,xlab="满意度",ylab="人数",col=rainbow(3),
        ylim=c(0,1.1*max(mytable1)),legend=rownames(mytable1),
        args.legend=list(x=9.5,y=330,ncol=3),beside=TRUE,
        main="(a)并列条形图")
barplot(mytable1,xlab="满意度",ylab="人数",col=rainbow(3),
        legend=rownames(mytable1),args.legend=list(x=2.2,y=800),
        main="(b)堆叠条形图")

#---图3.6的代码（sjPlot）----
install.packages("sjPlot")
library(sjPlot)
set_theme(title.size = 0.8,axis.title.size = 0.8,
          axis.textsize=0.8, geom.label.size = 2.5,
          legend.size=0.8, legend.title.size=0.8)
plot_xtab(data1.df$满意度,data1.df$性别,
              bar.pos = "dodge",
              show.n = TRUE, show.prc=TRUE, show.total=FALSE,
              show.summary = TRUE,
              vjust="center", title="(a)并列条形图")
dev.new()
plot_xtab(data1.df$满意度,data1.df$性别,
          bar.pos = "stack",
          show.n = TRUE, show.prc=TRUE, show.total=TRUE,
          vjust="middle", title="(b)堆叠条形图")

#---epade包中的bar3d：绘制3D图---------------
#---图3.7------------------------------------
install.packages("epade")
library(epade)
windows()
par(mai=c(0.7,0.4,0.1,0.1),cex=0.7)
bar3d.ade(data1.df$满意度,data1.df$性别,alpha=0.3,wall=3,
          xw=0.5,zw=1.2,
          xlab="满意度",ylab="人数",zlab="性别")

#---图3.8------------------------------------
windows()
bar.plot.ade(data1.df$性别,data1.df$满意度,
             form="z",wall=4,prozent=TRUE,btext=TRUE,
             xlab="满意度",ylab="人数",main="性别与满意度的条形图")
#---图3.9----------------------------------
dev.new()
windows()
bar.plot.ade(data1.df$满意度,data1.df$网购次数,
             form="c",wall=4,prozent=TRUE,btext=TRUE,
             lhoriz=TRUE, #是否绘制水平图例
             xlab="网购次数",ylab="人数",
             main="满意度与网购次数的3D并列条形图")

#---图3.10--------------------------------------
windows()
bar.plot.ade(data1.df$满意度,data1.df$网购次数,
             beside = FALSE,
             form="c",wall=5,prozent=TRUE,btext=TRUE,
             lhoriz=TRUE, #是否绘制水平图例
             xlab="网购次数",ylab="人数",
             main="满意度与网购次数的3D堆叠条形图")

#---脊形图，图3.11(x和y都是factor)--------------
mytable2<-table(data1.df$性别,data1.df$满意度)
addmargins(mytable2)

windows()
#par(mfrow=c(1,2),mai=c(0.7,0.7,0.4,0.4),cex=0.8)
spineplot(性别~满意度,data=data1.df, col=terrain.colors(2),
            xlab="满意度",ylab="性别",main="(a)性别与满意度")
legend("topleft",inset=.05,
       title = "性别",c("男","女"), #title对应Y轴（类别）
       fill=terrain.colors(2), horiz=TRUE)

spineplot(data1.df$满意度,data1.df$性别,col=terrain.colors(2),
            xlab="满意度",ylab="性别",main="(b)网购次数与满意度")
legend("topleft",inset=.05,
       title = "性别", c("男","女"), #title对应Y轴（类别）
       fill=terrain.colors(2), horiz=FALSE)

#--脊形图,x是数值，y是因子--------------------
data("mtcars")
mtcars
CYL<-factor(mtcars$cyl)
windows()
spineplot(mtcars$mpg,CYL,breaks=4,xlab="MPG",ylab="CYL",
          col=terrain.colors(3)) #注意：breaks
legend("topleft", inset=.05, title="Number of Cylinders",
       c("4","6","8"), fill=terrain.colors(3), horiz=TRUE)



#--百分比条形图-------------------------------
mytable3<-table(data1.df$网购次数,data1.df$满意度)
p1<-(mytable3[,1]/sum(mytable3[,1])*100) #满意
p2<-(mytable3[,2]/sum(mytable3[,2])*100) #不满意
p3<-(mytable3[,3]/sum(mytable3[,3])*100) #中立
M<-as.matrix(data.frame(p1,p2,p3)) ### 这一句很重要
windows()
barplot(M,names=c("不满意","满意","中立"),
        xlab="满意度",ylab="百分比(%)",ylim=c(0,115))
legend("top",rownames(M),cex=.8,box.col = "grey80",
       fill=grey.colors(3),ncol=3)

#?barplot() #看看barplot的帮助


####### 3.3 高维表的可视化 #############################
#install.packages("vcd")
#library(vcd)
#高维列联表，用ftable()函数
mytable.mul<-ftable(data1.df,
                    row.vars = c("网购次数","满意度"),
                    col.vars="性别")
mytable.mul
#----标准马赛克图--------
windows()
mosaicplot(~性别+网购次数+满意度,data=data1.df,
           las=3,color=rainbow(3), cex.axis=0.8,off=8,
           main="")
#----扩展马赛克图--------
dev.new()
mosaicplot(~性别+网购次数+满意度,data=data1.df,
           shade=TRUE,las=3,
           color=rainbow(3), 
           cex.axis=0.8,off=8,main="")

### 3.3.2 马赛克图的变种####################
# 使用vcd包中的各个函数来做马赛克图的变种
install.packages("vcd")
library(vcd)
#--图3.18 vs. 图3.19 ----------------------
d1<-structable(data1.df)
d1
windows()
strucplot(d1,shade=TRUE,labeling=labeling_values,
          main="实际频数")

dev.new()
strucplot(d1,shade=TRUE,labeling=labeling_values,
          value_type="expected",main="理论频数")

#d2<-ftable(data1.df,
#       row.vars = c("网购次数","满意度"),
#       col.vars="性别")
#d2
#windows()
#strucplot(d2,shade=TRUE,labeling=labeling_values)

#--图3.22（双层图）？？？颜色？？？--------
windows()
doubledecker(d1)

#--图3.24(pair函数)------------------------
windows()
pairs(d1)

#--图3.25(条件-马赛克图)------------------
dd<-structable(性别~网购次数+满意度,data=data1.df)
windows()
cotabplot(dd,labeling=labeling_values)


####3.4 其他###############################

#--图3.27，饼图-----------------------------
par(pin=c(3,3),mai=c(0.1,0.4,0.1,0.4))
count1<-table(data1.df$满意度)
name1<-names(count1)
percent<-prop.table(count1)*100
lable1<-paste(name1,percent,"%",sep="")
windows()
pie(count1,label=label1,init.angle = 180,
    radius=1, col = rainbow(3))

#--图3.28,3D的饼图--------------------
install.packages("plotrix")
library(plotrix)
windows()
pie3D(count1,labels=lable1,explode = 0.3,
      labelcex = 0.7,col=rainbow(3))

#--图3.31，扇形图--------------------
data3_2.df<-read.csv(".//chap03//data3_2.csv")
d<-t(data3_2.df[,2:5])

percent<-round(prop.table(d[,1]*100,1))
name<-c("劳动者报酬\n","生产税\n净额","固定资产折旧","营业盈余")
labs<-paste(name,"",percent,"%",sep="")
windows()
fan.plot(d[,1],labels=labs, max.span=0.9*pi,
         shrink=0.06,radius=1.2, label.radius = 1.4,
         ticks=200,
         col = c("deepskyblue","lightgreen","lightblue","pink"))


#--图3.29，环形图-------------------
attach(data1.df)
count1<-table(性别)
count2<-table(网购次数)
count3<-table(满意度)
name1<-names(count1)
name2<-names(count2)
name3<-names(count3)
percent1<-prop.table(count1)*100
percent2<-prop.table(count2)*100
percent3<-prop.table(count3)*100
label1<-paste(name1,"",percent1,"%",sep="")
label2<-paste(name2,"",percent2,"%",sep="")
label3<-paste(name3,"",percent3,"%",sep="")
par(mai=c(0.2,0.2,0.2,0.2),cex=0.7)
pie(count1,labels=label1,clockwise=TRUE,radius=0.8,col=gray.colors(2))
par(new=T)
pie(count2,labels=label2,clockwise=TRUE,radius=0.6,col=gray.colors(3))
par(new=T)
pie(count3,labels=label3,clockwise=TRUE,radius=0.4,col=gray.colors(3))
par(new=T)
pie(1,labels = "",clockwise=TRUE,radius=0.2,border = "white",col="white")
par(new=T)
box(col=3)
detach()

#--图3.30，环形图-------------
d
windows()
percent1<-round(prop.table(d[,1])*100)
percent2<-round(prop.table(d[,2])*100)
percent3<-round(prop.table(d[,3])*100)
name1<-c("劳动者报酬\n北京","生产税净额\n北京",
         "固定资产折旧\n北京","营业额盈余\n北京")
name2<-c("天津","天津","天津","天津")
name3<-c("上海","上海","上海","上海")
label1<-paste(name1,"",percent1,"%",sep="")
label2<-paste(name2,"",percent2,"%",sep="")
label3<-paste(name3,"",percent3,"%",sep="")
par(mai=c(0.2,0.2,0.2,0.2),cex=0.7)
pie(d[,1],labels=label1,init.angle = 90,radius=0.8,main="",col=c(6,7,5,3))
par(new=T)
pie(d[,2],labels=label2,init.angle = 90,radius=0.6,col=c(6,7,5,3))
par(new=T)
pie(d[,3],labels=label3,init.angle = 90,radius=0.4,main="",col=c(6,7,5,3))
par(new=T)
pie(1,labels="",init.angle = 90,radius=0.2,border="white",col="white")
par(new=T)
box(col="black")