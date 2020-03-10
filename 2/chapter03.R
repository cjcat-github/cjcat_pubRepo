#### ������ݵ�Ƶ���ֲ��Ŀ��ӻ�###########################

###3.1 һά�������Ŀ��ӻ�############################

#---������ͼ----------------
setwd("E://�Ͽ�//���ݿ��ӻ�//data//mydata")
data1.df<-read.csv(".//chap01//data1_1.csv")

attach(data1.df)
count_sex<-table(�Ա�)
count_shop<-table(��������)
count_saf<-table(�����)

#--Exampl1----------
windows()
layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
par(mai=c(0.6,0.6,0.4,0.1),cex=0.7)
barplot(count_sex,xlab="�Ա�",ylab="����",
        density=30,angle=0,col=c("grey80","grey50"),
        border="blue",main="(a)��ֱ����ͼ")
barplot(count_sex,xlab="����",ylab="�Ա�",,horiz=TRUE,
        density=20,angle=90,col=c("yellow","green"),
        main="(b)ˮƽ����ͼ")
barplot(count_shop,xlab="��������",ylab="����",
        col=2:4,main ="(c)������ͳ��ͼ")
barplot(count_saf,xlab="�����",ylab="����",
        col=2:4,main="(d)����ȵ�ͳ��ͼ")
detach()

#--Example2----------
windows()
par(mfrow=c(1,2),mai=c(0.7,0.7,0.2,0.1),cex=0.7)
bar<-barplot(count_shop,col=grey.colors(3),
             ylim=c(0,1.5*max(count_shop)),
             xlab="��������",ylab="����",
             cex.names=0.8,main="(a)����Ƶ����ǩ")
text(bar,count_shop,labels = count_shop,pos=3)
bar<-barplot(count_saf,ylim=c(0,1.5*max(count_saf)),
             xlab="�����",ylab="����",
             cex.names=0.8, col=c("grey50","grey70","grey90"),
             main="(b)����Ƶ������")
text(bar,count_saf,labels=count_saf,pos=3)
lines(bar,count_saf,type="o",col="red",pch=19,lwd=3)
points(bar,count_saf,type="h",col="red",lwd=3,lty=6)

#--Example3: sjPlot��---
install.packages("sjPlot")
library(sjPlot)
windows()
set_theme(axis.textsize = 0.8,    #������̶������С
          axis.title.size = 0.8,  #��������������С
          geom.label.size = 2.5)  #ͼ�α�ǩ�����С
plot_frq(data1.df$�����,type="bar",show.n=TRUE,show.prc=TRUE,
         axis.labels=c("not sat","sat","very sat"),
         axis.title="satisfied degree")

#--Paretoͼ------------
windows()
par(mai=c(1,1,0.5,1.5))
x<-sort(table(data1.df$�����),decreasing=TRUE)#һά��������Ƶ����������
bar<barplot(x,xlab="�����",ylab="����",
            col=rainbow(3),ylim=c(0,1000))
text(bar,x,labels=x,pos=3,col=1)
y<-cumsum(x)/sum(x)
par(new=T)
plot(y,type="b",pch=15,axes=FALSE,ann=FALSE,
     ylim=c(0,1.0))
axis(side=4)
mtext("�ۻ�Ƶ��",side=4,line=3,cex=0.8)
text(labels="�ۻ��ֲ�����",x=2.0,y=0.95,cex=1)

### 3.2 ��ά�������Ŀ��ӻ�############################
#---ͼ3.5�Ļ��ƴ���-----------
mytable1<-table(data1.df$��������,data1.df$�����)
addmargins(mytable1)

windows()
par(mfrow=c(2,1),mai=c(0.6,0.6,0.3,0.1),cex=0.6)
barplot(mytable1,xlab="�����",ylab="����",col=rainbow(3),
        ylim=c(0,1.1*max(mytable1)),legend=rownames(mytable1),
        args.legend=list(x=9.5,y=330,ncol=3),beside=TRUE,
        main="(a)��������ͼ")
barplot(mytable1,xlab="�����",ylab="����",col=rainbow(3),
        legend=rownames(mytable1),args.legend=list(x=2.2,y=800),
        main="(b)�ѵ�����ͼ")

#---ͼ3.6�Ĵ��루sjPlot��----
install.packages("sjPlot")
library(sjPlot)
set_theme(title.size = 0.8,axis.title.size = 0.8,
          axis.textsize=0.8, geom.label.size = 2.5,
          legend.size=0.8, legend.title.size=0.8)
plot_xtab(data1.df$�����,data1.df$�Ա�,
              bar.pos = "dodge",
              show.n = TRUE, show.prc=TRUE, show.total=FALSE,
              show.summary = TRUE,
              vjust="center", title="(a)��������ͼ")
dev.new()
plot_xtab(data1.df$�����,data1.df$�Ա�,
          bar.pos = "stack",
          show.n = TRUE, show.prc=TRUE, show.total=TRUE,
          vjust="middle", title="(b)�ѵ�����ͼ")

#---epade���е�bar3d������3Dͼ---------------
#---ͼ3.7------------------------------------
install.packages("epade")
library(epade)
windows()
par(mai=c(0.7,0.4,0.1,0.1),cex=0.7)
bar3d.ade(data1.df$�����,data1.df$�Ա�,alpha=0.3,wall=3,
          xw=0.5,zw=1.2,
          xlab="�����",ylab="����",zlab="�Ա�")

#---ͼ3.8------------------------------------
windows()
bar.plot.ade(data1.df$�Ա�,data1.df$�����,
             form="z",wall=4,prozent=TRUE,btext=TRUE,
             xlab="�����",ylab="����",main="�Ա�������ȵ�����ͼ")
#---ͼ3.9----------------------------------
dev.new()
windows()
bar.plot.ade(data1.df$�����,data1.df$��������,
             form="c",wall=4,prozent=TRUE,btext=TRUE,
             lhoriz=TRUE, #�Ƿ����ˮƽͼ��
             xlab="��������",ylab="����",
             main="�����������������3D��������ͼ")

#---ͼ3.10--------------------------------------
windows()
bar.plot.ade(data1.df$�����,data1.df$��������,
             beside = FALSE,
             form="c",wall=5,prozent=TRUE,btext=TRUE,
             lhoriz=TRUE, #�Ƿ����ˮƽͼ��
             xlab="��������",ylab="����",
             main="�����������������3D�ѵ�����ͼ")

#---����ͼ��ͼ3.11(x��y����factor)--------------
mytable2<-table(data1.df$�Ա�,data1.df$�����)
addmargins(mytable2)

windows()
#par(mfrow=c(1,2),mai=c(0.7,0.7,0.4,0.4),cex=0.8)
spineplot(�Ա�~�����,data=data1.df, col=terrain.colors(2),
            xlab="�����",ylab="�Ա�",main="(a)�Ա��������")
legend("topleft",inset=.05,
       title = "�Ա�",c("��","Ů"), #title��ӦY�ᣨ���
       fill=terrain.colors(2), horiz=TRUE)

spineplot(data1.df$�����,data1.df$�Ա�,col=terrain.colors(2),
            xlab="�����",ylab="�Ա�",main="(b)���������������")
legend("topleft",inset=.05,
       title = "�Ա�", c("��","Ů"), #title��ӦY�ᣨ���
       fill=terrain.colors(2), horiz=FALSE)

#--����ͼ,x����ֵ��y������--------------------
data("mtcars")
mtcars
CYL<-factor(mtcars$cyl)
windows()
spineplot(mtcars$mpg,CYL,breaks=4,xlab="MPG",ylab="CYL",
          col=terrain.colors(3)) #ע�⣺breaks
legend("topleft", inset=.05, title="Number of Cylinders",
       c("4","6","8"), fill=terrain.colors(3), horiz=TRUE)



#--�ٷֱ�����ͼ-------------------------------
mytable3<-table(data1.df$��������,data1.df$�����)
p1<-(mytable3[,1]/sum(mytable3[,1])*100) #����
p2<-(mytable3[,2]/sum(mytable3[,2])*100) #������
p3<-(mytable3[,3]/sum(mytable3[,3])*100) #����
M<-as.matrix(data.frame(p1,p2,p3)) ### ��һ�����Ҫ
windows()
barplot(M,names=c("������","����","����"),
        xlab="�����",ylab="�ٷֱ�(%)",ylim=c(0,115))
legend("top",rownames(M),cex=.8,box.col = "grey80",
       fill=grey.colors(3),ncol=3)

#?barplot() #����barplot�İ���


####### 3.3 ��ά���Ŀ��ӻ� #############################
#install.packages("vcd")
#library(vcd)
#��ά����������ftable()����
mytable.mul<-ftable(data1.df,
                    row.vars = c("��������","�����"),
                    col.vars="�Ա�")
mytable.mul
#----��׼������ͼ--------
windows()
mosaicplot(~�Ա�+��������+�����,data=data1.df,
           las=3,color=rainbow(3), cex.axis=0.8,off=8,
           main="")
#----��չ������ͼ--------
dev.new()
mosaicplot(~�Ա�+��������+�����,data=data1.df,
           shade=TRUE,las=3,
           color=rainbow(3), 
           cex.axis=0.8,off=8,main="")

### 3.3.2 ������ͼ�ı���####################
# ʹ��vcd���еĸ�����������������ͼ�ı���
install.packages("vcd")
library(vcd)
#--ͼ3.18 vs. ͼ3.19 ----------------------
d1<-structable(data1.df)
d1
windows()
strucplot(d1,shade=TRUE,labeling=labeling_values,
          main="ʵ��Ƶ��")

dev.new()
strucplot(d1,shade=TRUE,labeling=labeling_values,
          value_type="expected",main="����Ƶ��")

#d2<-ftable(data1.df,
#       row.vars = c("��������","�����"),
#       col.vars="�Ա�")
#d2
#windows()
#strucplot(d2,shade=TRUE,labeling=labeling_values)

#--ͼ3.22��˫��ͼ����������ɫ������--------
windows()
doubledecker(d1)

#--ͼ3.24(pair����)------------------------
windows()
pairs(d1)

#--ͼ3.25(����-������ͼ)------------------
dd<-structable(�Ա�~��������+�����,data=data1.df)
windows()
cotabplot(dd,labeling=labeling_values)


####3.4 ����###############################

#--ͼ3.27����ͼ-----------------------------
par(pin=c(3,3),mai=c(0.1,0.4,0.1,0.4))
count1<-table(data1.df$�����)
name1<-names(count1)
percent<-prop.table(count1)*100
lable1<-paste(name1,percent,"%",sep="")
windows()
pie(count1,label=label1,init.angle = 180,
    radius=1, col = rainbow(3))

#--ͼ3.28,3D�ı�ͼ--------------------
install.packages("plotrix")
library(plotrix)
windows()
pie3D(count1,labels=lable1,explode = 0.3,
      labelcex = 0.7,col=rainbow(3))

#--ͼ3.31������ͼ--------------------
data3_2.df<-read.csv(".//chap03//data3_2.csv")
d<-t(data3_2.df[,2:5])

percent<-round(prop.table(d[,1]*100,1))
name<-c("�Ͷ��߱���\n","����˰\n����","�̶��ʲ��۾�","Ӫҵӯ��")
labs<-paste(name,"",percent,"%",sep="")
windows()
fan.plot(d[,1],labels=labs, max.span=0.9*pi,
         shrink=0.06,radius=1.2, label.radius = 1.4,
         ticks=200,
         col = c("deepskyblue","lightgreen","lightblue","pink"))


#--ͼ3.29������ͼ-------------------
attach(data1.df)
count1<-table(�Ա�)
count2<-table(��������)
count3<-table(�����)
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

#--ͼ3.30������ͼ-------------
d
windows()
percent1<-round(prop.table(d[,1])*100)
percent2<-round(prop.table(d[,2])*100)
percent3<-round(prop.table(d[,3])*100)
name1<-c("�Ͷ��߱���\n����","����˰����\n����",
         "�̶��ʲ��۾�\n����","Ӫҵ��ӯ��\n����")
name2<-c("���","���","���","���")
name3<-c("�Ϻ�","�Ϻ�","�Ϻ�","�Ϻ�")
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