f.norm.vec<-function(x){
  nrm<-sqrt(sum(x^2))
  return(x/nrm)
}

f.norm<-function(x){
  nrm<-sqrt(sum(x^2))
  return(nrm)
}

par(mfrow=c(2,2))
f.T2.confidence<-function(x,n=100,cl=0.95){
  xm<-apply(x,2,mean)
  p<-dim(x)[2]
  xn<-dim(x)[1]
  xv<-var(x)
  ev<-eigen(xv)
  sph<-matrix(rnorm(n*p),ncol=p)
  cntr<-t(apply(sph,1,f.norm.vec))
  cntr<-cntr%*%diag(sqrt(ev$values))%*%t(ev$vectors)
  cntr<-cntr*sqrt(p*(xn-1)*qf(cl,p,xn-p)/(xn*(xn-p)))
  cntr<-cntr+matrix(rep(xm,n),nrow=n,byrow=T)
  return(cntr)
}

f.var.ellipse<-function(x,n=100){
  xm<-apply(x,2,mean)
  p<-dim(x)[2]
  xn<-dim(x)[1]
  xv<-var(x)
  ev<-eigen(xv)
  sph<-matrix(rnorm(n*p),ncol=p)
  cntr<-t(apply(sph,1,f.norm.vec))
  cntr<-cntr%*%diag(sqrt(ev$values))%*%t(ev$vectors)
  cntr<-cntr+matrix(rep(xm,n),nrow=n,byrow=T)
  return(cntr)
}

par(pty="s")
p<-ggplot(d.flea,aesthetics=c(x=tars1,y=tars2,colour=species,shape=species))
ggpoint(p)

library(RColorBrewer)
mypalette<-brewer.pal(8,"Set1")
par(mfrow=c(2,2),pty="s",mar=c(5,4,2,2))
plot(d.flea[,1:2],pch=d.flea[,7],xlim=c(110,250),ylim=c(105,150))
rect(100,90,260,170,col="grey80")
abline(h=seq(110,140,by=10),col="white")
abline(v=seq(120,240,by=20),col="white")
points(d.flea[d.flea[,7]==1,1:2],pch=16,col=mypalette[3],cex=1.5)
points(d.flea[d.flea[,7]==2,1:2],pch=1,col=mypalette[4],cex=1.5)
points(d.flea[d.flea[,7]==3,1:2],pch=4,col=mypalette[5],cex=1.5)
box(col="gray70")
x1<-f.var.ellipse(d.flea[d.flea[,7]==1,1:2],n=200)
x2<-f.var.ellipse(d.flea[d.flea[,7]==2,1:2],n=200)
x3<-f.var.ellipse(d.flea[d.flea[,7]==3,1:2],n=200)
plot(d.flea[,1:2],pch=d.flea[,7],xlim=c(110,250),ylim=c(105,150))
rect(100,90,260,170,col="grey80")
abline(h=seq(110,140,by=10),col="white")
abline(v=seq(120,240,by=20),col="white")
points(x1,pch=16,col=mypalette[3])
points(x2,pch=16,col=mypalette[4])
points(x3,pch=16,col=mypalette[5])
box(col="gray70")

plot(d.olive[,6:7],pch=as.numeric(d.olive[,1]),xlim=c(6250,8550),ylim=c(380,1500))
rect(6000,300,10000,1600,col="grey80")
abline(v=seq(6500,8500,by=500),col="white")
abline(h=seq(400,1400,by=200),col="white")
points(d.olive[d.olive[,1]=="South",6:7],pch=1,col=mypalette[3],cex=1.5)
points(d.olive[d.olive[,1]=="North",6:7],pch=16,col=mypalette[4],cex=1.5)
points(d.olive[d.olive[,1]=="Sardinia",6:7],pch=4,col=mypalette[5],cex=1.5)
box(col="gray70")
x1<-f.var.ellipse(d.olive[d.olive[,1]=="South",6:7],n=200)
x2<-f.var.ellipse(d.olive[d.olive[,1]=="North",6:7],n=200)
x3<-f.var.ellipse(d.olive[d.olive[,1]=="Sardinia",6:7],n=200)
plot(d.olive[,6:7],pch=as.numeric(d.olive[,1]),xlim=c(6250,8550),ylim=c(380,1500))
rect(6000,300,10000,1600,col="grey80")
abline(v=seq(6500,8500,by=500),col="white")
abline(h=seq(400,1400,by=200),col="white")
points(x1,pch=16,col=mypalette[3])
points(x2,pch=16,col=mypalette[4])
points(x3,pch=16,col=mypalette[5])
box(col="gray70")

# Looking at boundaries
par(pty="s")
plot(d.flea[,1:2],pch=d.flea[,7])
rect(110,100,250,160,col="grey80")
abline(h=seq(110,140,by=10),col="white")
abline(v=seq(120,240,by=20),col="white")
points(d.flea[d.flea[,7]==1,1:2],pch=16,col=mypalette[3])
points(d.flea[d.flea[,7]==2,1:2],pch=1,col=mypalette[4])
points(d.flea[d.flea[,7]==3,1:2],pch=4,col=mypalette[5])
box(col="gray70")

library(MASS)
flea.lda<-lda(d.flea[,1:2],d.flea[,7])
x<-seq(100,255,by=2)
y<-seq(100,160,by=0.5)
z<-NULL
for (i in 1:length(x))
  for (j in 1:length(y))
    z<-rbind(z,c(x[i],y[j]))
zm<-predict(flea.lda,z)$class
#par(pty="s",mfrow=c(1,2))
plot(d.flea[,1:2],pch=d.flea[,7],xlim=c(110,250),ylim=c(105,150))
rect(100,90,260,170,col="grey80")
abline(h=seq(110,140,by=10),col="white")
abline(v=seq(120,240,by=20),col="white")
points(z[,1],z[,2],col=mypalette2[as.numeric(zm)],pch=8,xlab="tars1",ylab="tars2")
points(d.flea[d.flea[,7]==1,1:2],pch=16,col=mypalette[3],cex=1.5)
points(d.flea[d.flea[,7]==2,1:2],pch=1,col=mypalette[4],cex=1.5)
points(d.flea[d.flea[,7]==3,1:2],pch=4,col=mypalette[5],cex=1.5)
box(col="gray70")
#plot(z[,1],z[,2],col=mypalette[as.numeric(zm)+2],pch=8,xlab="tars1",ylab="tars2",
#  xlim=c(110,250),ylim=c(105,150))
#title("LDA")

colnames(z)<-c("tars1","tars2")
library(rpart)
flea.rp<-rpart(species~tars1+tars2,d.flea[,c(1:2,7)],method="class")
flea.rp<-rpart(species~tars1+tars2,d.flea[,c(1:2,7)],method="class",
  parms=list(split='information'),
  control=rpart.control(cp=0.0005,minsplit=1,minbucket=1,maxdepth=20))
#plot(flea.rp);text(flea.rp)
zm2<-predict(flea.rp,data.frame(z),type="class")
#par(pty="s")
plot(d.flea[,1:2],pch=d.flea[,7],xlim=c(110,250),ylim=c(105,150))
rect(100,90,260,170,col="grey80")
abline(h=seq(110,140,by=10),col="white")
abline(v=seq(120,240,by=20),col="white")
points(z[,1],z[,2],col=mypalette2[as.numeric(zm2)],pch=8,
  xlab="tars1",ylab="tars2")
points(d.flea[d.flea[,7]==1,1:2],pch=16,col=mypalette[3],cex=1.5)
points(d.flea[d.flea[,7]==2,1:2],pch=1,col=mypalette[4],cex=1.5)
points(d.flea[d.flea[,7]==3,1:2],pch=4,col=mypalette[5],cex=1.5)
box(col="gray70")
#plot(z[,1],z[,2],col=mypalette[as.numeric(zm2)+2],pch=8,
#  xlab="tars1",ylab="tars2",xlim=c(110,250),ylim=c(105,150))
#title("tree")

table(predict(flea.lda,d.flea[,1:2])$class,d.flea[,7])
table(predict(flea.rp,d.flea[,1:2],type="class"),d.flea[,7])

mypalette2<-brewer.pal(8,"Pastel2")
mypalette2<-mypalette2[c(1,4,2,3,5,6,7,8)]
par(mfrow=c(1,2),pty="s",mar=c(5,4,2,2))
xm<-d.flea[predict(flea.lda,d.flea[,1:2])$class!=d.flea[,7],]
plot(d.flea[,1:2],pch=d.flea[,7],xlim=c(110,250),ylim=c(105,150))
rect(100,90,260,170,col="grey80")
abline(h=seq(110,140,by=10),col="white")
abline(v=seq(120,240,by=20),col="white")
points(z[,1],z[,2],col=mypalette2[as.numeric(zm)],pch=8,xlab="tars1",ylab="tars2")
points(d.flea[d.flea[,7]==1,1:2],pch=16,col=mypalette[3],cex=0.75)
points(d.flea[d.flea[,7]==2,1:2],pch=1,col=mypalette[4],cex=0.75)
points(d.flea[d.flea[,7]==3,1:2],pch=4,col=mypalette[5],cex=0.75)
points(xm[xm[,7]==1,1:2],pch=16,col=mypalette[3],cex=2.5)
points(xm[xm[,7]==2,1:2],pch=1,col=mypalette[4],cex=2.5)
points(xm[xm[,7]==3,1:2],pch=4,col=mypalette[5],cex=2.5,lwd=3)
box(col="gray70")
xm<-d.flea[predict(flea.rp,d.flea[,1:2],type="class")!=d.flea[,7],]
plot(d.flea[,1:2],pch=d.flea[,7],xlim=c(110,250),ylim=c(105,150))
rect(100,90,260,170,col="grey80")
abline(h=seq(110,140,by=10),col="white")
abline(v=seq(120,240,by=20),col="white")
points(z[,1],z[,2],col=mypalette2[as.numeric(zm2)],pch=8,
  xlab="tars1",ylab="tars2")
points(d.flea[d.flea[,7]==1,1:2],pch=16,col=mypalette[3],cex=0.75)
points(d.flea[d.flea[,7]==2,1:2],pch=1,col=mypalette[4],cex=0.75)
points(d.flea[d.flea[,7]==3,1:2],pch=4,col=mypalette[5],cex=0.75)
points(xm[xm[,7]==1,1:2],pch=16,col=mypalette[3],cex=2.5)
points(xm[xm[,7]==2,1:2],pch=1,col=mypalette[4],cex=2.5)
points(xm[xm[,7]==3,1:2],pch=4,col=mypalette[5],cex=2.5,lwd=3)
box(col="gray70")

