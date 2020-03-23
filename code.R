

##code of generating table
t=c(0,0.67,0.84,1.28,1.65,2.32,2.58,3.09,3.72)
n=c(100,1000,10000)
p=matrix(0,nrow=9,ncol=3)
a=0
for (i in 1:9)
  for(j in 1:3){
    a=a+1
    num=rnorm(n[j],0,1)
    p[i,j]=mean(num<t[i])
    print(a)
  }
rownames(p)<-t
colnames(p)<-n
pcycle=array(0,dim=c(9,3,100))

#code of generating figure
#k代表次数
a=0
for (i in 1:9)
  for(j in 1:3)
    for (k in 1:100){
      a=a+1
      num=rnorm(n[j],0,1)
      pcycle[i,j,k]=mean(num<t[i])
      print(a)
    }
dimnames(pcycle)[[1]]<-t
dimnames(pcycle)[[2]]<-n
#生成dataframe-方便画图
V1<-c(1:100)
plot_data<-data.frame(V1)
k=0
for (i in 1:9)
  for (j in 1:3){
    k=k+1
    print(k)
    plot_data[ ,k]<-pcycle[i,j, ]
    plot_data[ ,k]<-plot_data[ ,k]-pnorm(t[i])
  }
label<-data.frame(label1<-c("0-100"),label2<-c("0-1000"),label3<-c("0-10000"),
                  label4<-c("0.67-100"),label5<-c("0.67-1000"),label6<-c("0.67-10000"),
                  label7<-c("0.84-100"),label8<-c("0.84-1000"),label9<-c("0.84-10000"),
                  label10<-c("1.28-100"),label11<-c("1.28-1000"),label12<-c("1.28-10000"),
                  label13<-c("1.65-100"),label14<-c("1.65-1000"),label15<-c("1.65-10000"),
                  label16<-c("2.32-100"),label17<-c("2.32-1000"),label18<-c("2.32-10000"),
                  label19<-c("2.58-100"),label20<-c("2.58-1000"),label21<-c("2.58-10000"),
                  label22<-c("3.09-100"),label23<-c("3.09-1000"),label24<-c("3.09-10000"),
                  label25<-c("3.72-100"),label26<-c("3.72-1000"),label27<-c("3.72-10000"))

library(ggplot2)
library(lattice)
library(plyr)
library(Rmisc)
p1<-ggplot(data=plot_data,aes(y=V1,x=label1))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=0, n=100",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
#+theme_bw()-背景换为白色 ,theme(hjust)=0.5
p1
p2<-ggplot(data=plot_data,aes(y=V2,x=label2))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=0, n=1000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p3<-ggplot(data=plot_data,aes(y=V3,x=label3))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=0, n=10000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
g1<-multiplot(p1,p2,p3,cols=3)

p4<-ggplot(data=plot_data,aes(y=V4,x=label4))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=0.67, n=100",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p5<-ggplot(data=plot_data,aes(y=V5,x=label5))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=0.67, n=1000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p6<-ggplot(data=plot_data,aes(y=V6,x=label6))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=0.67, n=10000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
g2<-multiplot(p4,p5,p6,cols=3)

p7<-ggplot(data=plot_data,aes(y=V7,x=label7))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=0.84, n=100",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p8<-ggplot(data=plot_data,aes(y=V8,x=label8))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=0.84, n=1000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p9<-ggplot(data=plot_data,aes(y=V9,x=label9))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=0.84, n=10000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
g3<-multiplot(p7,p8,p9,cols=3)

p10<-ggplot(data=plot_data,aes(y=V10,x=label10))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=1.28, n=100",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p11<-ggplot(data=plot_data,aes(y=V11,x=label11))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=1.28, n=1000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p12<-ggplot(data=plot_data,aes(y=V12,x=label12))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=1.28, n=10000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
g4<-multiplot(p10,p11,p12,cols=3)

p13<-ggplot(data=plot_data,aes(y=V13,x=label13))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=1.65, n=100",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p14<-ggplot(data=plot_data,aes(y=V14,x=label14))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=1.65, n=1000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p15<-ggplot(data=plot_data,aes(y=V15,x=label15))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=1.65, n=10000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
g5<-multiplot(p13,p14,p15,cols=3)

p16<-ggplot(data=plot_data,aes(y=V16,x=label16))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=2.32, n=100",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p17<-ggplot(data=plot_data,aes(y=V17,x=label17))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=2.32, n=1000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p18<-ggplot(data=plot_data,aes(y=V18,x=label18))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=2.32, n=10000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
g6<-multiplot(p16,p17,p18,cols=3)

p19<-ggplot(data=plot_data,aes(y=V19,x=label19))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=2.58, n=100",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p20<-ggplot(data=plot_data,aes(y=V20,x=label20))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=2.58, n=1000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p21<-ggplot(data=plot_data,aes(y=V21,x=label21))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=2.58, n=10000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
g7<-multiplot(p19,p20,p21,cols=3)

p22<-ggplot(data=plot_data,aes(y=V22,x=label22))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=3.09, n=100",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p23<-ggplot(data=plot_data,aes(y=V23,x=label23))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=3.09, n=1000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p24<-ggplot(data=plot_data,aes(y=V24,x=label24))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t<3.09, n=10000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
g8<-multiplot(p22,p23,p24,cols=3)

p25<-ggplot(data=plot_data,aes(y=V25,x=label25))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=3.72, n=100",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p26<-ggplot(data=plot_data,aes(y=V26,x=label26))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=3.72, n=1000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
p27<-ggplot(data=plot_data,aes(y=V27,x=label27))+geom_boxplot(
  fill="thistle",colour="gray27")+
  labs(title="Boxplot of error at t=3.72, n=10000",y="error",
       x=NULL)+theme(plot.title=element_text(size=13,hjust=0.5))
g9<-multiplot(p25,p26,p27,cols=3)