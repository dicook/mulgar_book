# Clusters or segmenting
d.separated.clusters<-matrix(rnorm(99*2),ncol=2)
d.separated.clusters[1:33,1]<-d.separated.clusters[1:33,1]+8
d.separated.clusters[34:66,2]<-d.separated.clusters[34:66,2]+8
d.separated.clusters[34:66,1]<-d.separated.clusters[34:66,1]+4
d.unseparated.clusters<-matrix(rnorm(99*2),ncol=2)
vc<-matrix(c(1,0.6,0.6,1),ncol=2,byrow=T)
d.unseparated.clusters<-d.unseparated.clusters%*%vc
d.nuisance<-matrix(rnorm(99*2),ncol=2)
d.nuisance[1:49,1]<-d.nuisance[1:49,1]+8
d.odd.shapes<-matrix(rnorm(99*2),ncol=2)
d.odd.shapes[1:66,2]<-(d.odd.shapes[1:66,1])^2-5+rnorm(66)*0.6
d.odd.shapes[1:66,1]<-d.odd.shapes[1:66,1]*3

# Revised version
par(mfrow=c(2,2),pty="s",mar=c(2,2,2,2))
plot(d.separated.clusters[,1],d.separated.clusters[,2],xlab="",ylab="",axes=F,
  xlim=c(-3,10),ylim=c(-2.5,10.5))
rect(-5,-5,15,17,col="grey80")
abline(h=seq(-5,15,by=2),col="white")
abline(v=seq(-5,15,by=2),col="white")
points(d.separated.clusters[,1],d.separated.clusters[,2],pch=16)
box(col="gray70")
plot(d.unseparated.clusters[,1],d.unseparated.clusters[,2],xlab="",ylab="",axes=F,
  xlim=c(-3,3.3),ylim=c(-3.1,2.8))
rect(-5,-5,15,17,col="grey80")
abline(h=seq(-5,5,by=1),col="white")
abline(v=seq(-5,5,by=1),col="white")
points(d.unseparated.clusters[,1],d.unseparated.clusters[,2],pch=16)
box(col="gray70")
plot(d.nuisance[,1],d.nuisance[,2],xlab="",ylab="",axes=F,xlim=c(-2.5,10.6),
  ylim=c(-3.4,2.8))
rect(-5,-5,15,17,col="grey80")
abline(h=seq(-5,15,by=1),col="white")
abline(v=seq(-5,15,by=2),col="white")
points(d.nuisance[,1],d.nuisance[,2],pch=16)
box(col="gray70")
plot(d.odd.shapes[,1],d.odd.shapes[,2],xlab="",ylab="",axes=F,xlim=c(-5.6,6),
  ylim=c(-6.5,2.2))
rect(-10,-10,15,17,col="grey80")
abline(h=seq(-8,15,by=1),col="white")
abline(v=seq(-8,15,by=2),col="white")
points(d.odd.shapes[,1],d.odd.shapes[,2],pch=16)
box(col="gray70")

library(ggplot)
qplot(d.separated.clusters[,1],d.separated.clusters[,2],xlab=expression(X[1]),
  ylab=expression(X[2]))
qplot(d.unseparated.clusters[,1],d.unseparated.clusters[,2],xlab=expression(X[1]),
  ylab=expression(X[2]))
qplot(d.nuisance[,1],d.unseparated.clusters[,2],xlab=expression(X[1]),
  ylab=expression(X[2]))
qplot(d.odd.shapes[,1],d.odd.shapes[,2],xlab=expression(X[1]),
  ylab=expression(X[2]))
# end new version

postscript("ideal.ps",width=8.0,height=8.0,horizontal=FALSE,
  paper="special",family="URWHelvetica")
par(mfrow=c(2,2),pty="s",mar=c(1,1,1,1))
plot(d.separated.clusters[,1],d.separated.clusters[,2],pch=16,
  xlab="",ylab="",axes=F,cex=1.5)
box()
plot(d.unseparated.clusters[,1],d.unseparated.clusters[,2],pch=16,
  xlab="",ylab="",axes=F,cex=1.5)
box()
plot(d.nuisance[,1],d.nuisance[,2],pch=16,xlab="",ylab="",axes=F,cex=1.5)
box()
plot(d.odd.shapes[,1],d.odd.shapes[,2],pch=16,xlab="",ylab="",axes=F,cex=1.5)
box()
dev.off()

cluster.data<-c(7.3,7.4,6.7,4.1,5.9,8.8,6.1,6.3,8.3,
       7.6,7.2,7.2,4.6,6.7,8.2,6.5,6.1,8.3,
       7.7,7.3,7.1,4.6,6.7,8.1,6.4,6.7,8.6,
       8.0,7.2,7.4,4.8,6.6,8.1,6.6,6.6,8.5)
cluster.data<-matrix(cluster.data,byrow=F,ncol=4)
min<-min(cluster.data)
max<-max(cluster.data)
cluster.data<-cluster.data[c(1,2,4),]

panel.bg<-function(x, y)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(2, 10, 2, 10))
    rect(2,2,10,10,col="grey90")
    abline(v=3:9,col="white")
    abline(h=3:9,col="white")
    points(x,y,pch=c("1","2","3"),col="black",cex=1.5)
    box(col="white")
}

panel.diag<-function(x,y)
{
  box(col="white")
}

par(pty="s",mar=c(4,4,4,4))
labels<-c("Var 1","Var 2","Var 3","Var 4")
pairs(cluster.data,labels,lower.panel=panel.bg,
  upper.panel=panel.bg) #,diag.panel=panel.diag)
) #cex=1.9,xlim=c(3.5,9),ylim=c(3,9),pch=c("1","2","3"))

par(mfrow=c(4,4),mar=c(0,0,0,0)+0.3,oma=c(2,2,2,2))
plot(c(0,1),c(0,1),type="n",xlab="n",ylab="n",axes=F)
text(0.5,0.5,"Var 1",cex=2,col="grey60")
box(col="grey80")
plot(cluster.data[,2],cluster.data[,1],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,2],cluster.data[,1],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
mtext("4     5     6     7     8",3,cex=0.8,line=0.5,col="grey60")
plot(cluster.data[,3],cluster.data[,1],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,3],cluster.data[,1],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
#mtext("4     5     6     7     8",3,cex=0.8,line=0.5,col="grey60")
plot(cluster.data[,4],cluster.data[,1],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,4],cluster.data[,1],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
mtext("4     5     6     7     8",3,cex=0.8,line=0.5,col="grey60")
mtext("4     5     6     7     8",4,cex=0.8,line=0.5,col="grey60")
plot(cluster.data[,1],cluster.data[,2],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,1],cluster.data[,2],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
mtext("4     5     6     7     8",2,cex=0.8,line=0.5,col="grey60")
plot(c(0,1),c(0,1),type="n",xlab="n",ylab="n",axes=F)
text(0.5,0.5,"Var 2",cex=2,col="grey60")
box(col="grey80")
plot(cluster.data[,3],cluster.data[,2],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,3],cluster.data[,2],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
plot(cluster.data[,4],cluster.data[,2],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,4],cluster.data[,2],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
plot(cluster.data[,1],cluster.data[,3],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,1],cluster.data[,3],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
plot(cluster.data[,2],cluster.data[,3],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,2],cluster.data[,3],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
plot(c(0,1),c(0,1),type="n",xlab="n",ylab="n",axes=F)
text(0.5,0.5,"Var 3",cex=2,col="grey60")
box(col="grey80")
plot(cluster.data[,4],cluster.data[,3],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,4],cluster.data[,3],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
mtext("4     5     6     7     8",4,cex=0.8,line=0.5,col="grey60")
plot(cluster.data[,1],cluster.data[,4],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,1],cluster.data[,4],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
mtext("4     5     6     7     8",1,cex=0.8,line=0.5,col="grey60")
mtext("4     5     6     7     8",2,cex=0.8,line=0.5,col="grey60")
plot(cluster.data[,2],cluster.data[,4],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,2],cluster.data[,4],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
plot(cluster.data[,3],cluster.data[,4],xlim=c(3.5,8.5),ylim=c(3.5,8.5),
  type="n",xlab="n",ylab="n",axes=F)
rect(2,2,10,10,col="grey90")
abline(v=4:8,col="white")
abline(h=4:8,col="white")
points(cluster.data[,3],cluster.data[,4],pch=c("1","2","3"),cex=1.5)
box(col="grey80")
mtext("4     5     6     7     8",1,cex=0.8,line=0.5,col="grey60")
plot(c(0,1),c(0,1),type="n",xlab="n",ylab="n",axes=F)
text(0.5,0.5,"Var 4",cex=2,col="grey60")
box(col="grey80")


dist1<-function(x1,x2) {
  dst<-1-abs(cor(x1,x2))
  return(dst)
}

dist2<-function(x1,x2) {
  dst<-(1-abs(sum(x1*x2)/(sqrt(sum(x1*x1))*sqrt(sum(x2*x2)))))
  return(dst)
}

dist3<-function(x1,x2) {
  dst<-(sum((x1-x2)^2))/(length(x1)-1)
  return(dst)
}

dist4<-function(x1,x2) {
  dst<-2*(1-(cor(x1,x2)))
  return(dst)
}
2*(1-cor(cluster.data.std[1,],cluster.data.std[2,]))
2*(1-cor(cluster.data[1,],cluster.data[2,]))
sqrt(sum((cluster.data.std[1,]-cluster.data.std[2,])^2))

dist5<-function(x1,x2) {
  dst<-(1-(sum(x1*x2)/(sqrt(sum(x1*x1))*sqrt(sum(x2*x2)))))
  return(dst)
}

par(mfrow=c(1,1),cex=1,pty="m",mar=c(2,2,0,0))
plot(c(1:4),as.numeric(cluster.data[1,]),type="l",ylim=c(3.8,8.5),xlim=c(0.8,4.2),
  ylab="",xlab="",xpd=T,axes=F)
rect(0,0,5,10,col="grey90")
abline(h=seq(4,9,by=0.5),col="white")
abline(v=1:4,col="white")
for(i in 1:3) {
  lines(c(1:4),as.numeric(cluster.data[i,]),lwd=2)
  text(4,cluster.data[i,4],i,pos=4,cex=1.2)
  text(1,cluster.data[i,1],i,pos=2,cex=1.2)
}
axis(side=1,at=c(1:4),lab=c("1","2","3","4"),col="grey60",
 col.axis="grey60")
axis(side=2,at=c(3,4,5,6,7,8,9,10),lab=c(3,4,5,6,7,8,9,10),col="grey60",
 col.axis="grey60")
box(col="grey80",lwd=2)
mtext("Variable",1,line=2.5,cex=1.3,col="grey60")
#mtext("Value",2,line=2.5,cex=1.3,col="grey60")

plot(c(1:4),as.numeric(cluster.data[1,]),type="n",ylim=c(3.5,9.5),xlim=c(0.5,4.5),
  ylab="Gene Expression",xlab=" ",xpd=T,axes=F)
for(i in c(1,3,4,7)) {
  lines(c(1:4),as.numeric(cluster.data[i,]))
  text(4,cluster.data[i,4],i,pos=4)
  text(1,cluster.data[i,1],i,pos=2)
}
axis(side=1,at=c(1:4),lab=c("Var 1","Var 2","Var 3","Var 4"))
axis(side=2,at=c(3,4,5,6,7,8,9,10),lab=c(3,4,5,6,7,8,9,10))
title("One Cluster?")
plot(c(1:4),as.numeric(data[1,]),type="n",ylim=c(3.5,9.5),xlim=c(0.5,4.5),
  ylab="Gene Expression",xlab=" ",xpd=T,axes=F)
for(i in c(2,6)) {
  lines(c(1:4),as.numeric(data[i,]))
  text(4,cluster.data[i,4],i,pos=4)
  text(1,cluster.data[i,1],i,pos=2)
}
axis(side=1,at=c(1:4),lab=c("Var 1","Var 2","Var 3","Var 4"))
axis(side=2,at=c(3,4,5,6,7,8,9,10),lab=c(3,4,5,6,7,8,9,10))
title("Another Cluster?")

# Calculate interpoint distance matrices
options(digits=2)
for (i in 1:9) {
  for (j in 1:9)
    if (j<=i) cat(dist1(cluster.data[i,],cluster.data[j,])," ")
  cat("\n")
}

options(digits=2)
for (i in 1:9) {
  for (j in 1:9)
    if (j<=i) cat(dist2(cluster.data[i,],cluster.data[j,])," ")
  cat("\n")
}

options(digits=2)
for (i in 1:9) {
  for (j in 1:9)
    if (j<=i) cat(dist3(cluster.data.std[i,],cluster.data.std[j,])," ")
  cat("\n")
}

options(digits=2)
for (i in 1:9) {
  for (j in 1:9)
    if (j<=i) cat(dist4(cluster.data[i,],cluster.data[j,])," ")
  cat("\n")
}

cor.dist<-NULL
for (i in 1:9) 
  for (j in 1:9)
    cor.dist<-c(cor.dist,dist4(cluster.data[i,],cluster.data[j,]))
cor.dist<-matrix(cor.dist,ncol=9,byrow=T)
cor.dist2<-as.dist(cor.dist)
hc.cor<-hclust(cor.dist2,method="ward")
hc.cor<-hclust(cor.dist2,method="single")
plot(hc.cor)


cluster.data.std<-t(apply(cluster.data,1,std.row))

x<-matrix((runif(500)-0.5)*2,ncol=2)
x<-x[(x[,1]^2+x[,2]^2)<1,]
x2<-cbind(x[,1]+1,x[,2]+1)

sub.mean<-function(x) {
  return(x-mean(x))
}
std.row<-function(x) {
  return((x-mean(x))/sd(x))
}
x4<-t(apply(x,1,sub.mean))
x4<-t(apply(x,1,std.row))

x3<-matrix((runif(3000)-0.5)*2,ncol=3)
x3<-x3[(x3[,1]^2+x3[,2]^2+x3[,3]^2)<1,]
x5<-t(apply(x3,1,sub.mean))
x5<-t(apply(x3,1,std.row))
pairs(x5)

x6<-matrix((runif(4000)-0.5)*2,ncol=4)
x6<-x6[(x6[,1]^2+x6[,2]^2+x6[,3]^2+x6[,4])<1,]
x7<-t(apply(x6,1,std.row))
pairs(x7)


dst2<-NULL
for (i in 1:204)
  dst2<-c(dst2,dist2(c(1,0),x[i,]))
summary(dst2)
dst3<-NULL
for (i in 1:204)
  dst3<-c(dst3,dist2(c(2,1),x2[i,]))
summary(dst3)
dst6<-NULL
for (i in 1:204)
  dst6<-c(dst6,dist3(sub.mean(c(2,1)),x4[i,]))
summary(dst6)
plot(x4)
points(0.5,-0.5,pch="x",cex=2)
dst7<-NULL
for (i in 1:204)
  dst7<-c(dst7,dist5(c(1,0),x[i,]))
summary(dst7)

par(mfrow=c(1,2),pty="s")
par(mfrow=c(1,3),pty="s")
plot(x,pch=16,xlab="x1",ylab="x2",xlim=c(-1,1),ylim=c(-1,1),col="grey")
points(x[dst2<summary(dst2)[2],1],x[dst2<summary(dst2)[2],2],
  col="black",pch=16)
points(x[dst2>=summary(dst2)[2]&dst2<summary(dst2)[3],1],
  x[dst2>=summary(dst2)[2]&dst2<summary(dst2)[3],2],
  col="grey45",pch=16)
points(1,0,col="black",pch="x",cex=2)
title("Corr: Data centered at (0,0)")
plot(x2,pch=16,xlab="x1",ylab="x2",xlim=c(0,2),ylim=c(0,2),col="grey")
points(x2[dst3<summary(dst3)[2],1],x2[dst3<summary(dst3)[2],2],
  col="black",pch=16)
points(x2[dst3>=summary(dst3)[2]&dst3<summary(dst3)[3],1],
  x2[dst3>=summary(dst3)[2]&dst3<summary(dst3)[3],2],
  col="grey45",pch=16)
points(2,1,col="black",pch="x",cex=2)
title("Corr: Data centered at (1,1)")
plot(x,pch=16,xlab="x1",ylab="x2",xlim=c(-1,1),ylim=c(-1,1),col="grey")
points(x[dst7<summary(dst7)[2],1],x[dst7<summary(dst7)[2],2],
  col="black",pch=16)
points(x[dst7>=summary(dst7)[2]&dst7<summary(dst7)[3],1],
  x[dst7>=summary(dst7)[2]&dst7<summary(dst7)[3],2],
  col="grey45",pch=16)
points(1,0,col="black",pch="x",cex=2)
title("Corr: centered at (0,0)")
plot(x,pch=16,xlab="x1",ylab="x2",xlim=c(-1,1),ylim=c(-1,1),col="grey")
points(x[dst6<summary(dst6)[2],1],x[dst6<summary(dst6)[2],2],
  col="black",pch=16)
points(x[dst6>=summary(dst6)[2]&dst6<summary(dst6)[3],1],
  x[dst6>=summary(dst6)[2]&dst6<summary(dst6)[3],2],
  col="grey45",pch=16)
points(1,0,col="black",pch="x",cex=2)
title("Euclidean")

dst1<-NULL
for (i in 1:dim(x3)[1])
  dst1<-c(dst1,dist1(c(1,0,0),x3[i,]))
summary(dst1)
dst4<-NULL
for (i in 1:dim(x3)[1])
  dst4<-c(dst4,dist2(c(1,0,0),x3[i,]))
summary(dst4)
dst5<-NULL
for (i in 1:dim(x3)[1])
  dst5<-c(dst5,dist3(c(1,0,0),x3[i,]))
summary(dst5)

library(Rggobi)
ggobi(x3)
# Reset colors
setColors.ggobi(rep(8,dim(x3)[1]),c(1:dim(x3)[1]))
setGlyphs.ggobi(rep(5,dim(x3)[1]),rep(2,dim(x3)[1]),c(1:dim(x3)[1]))

# Color according to corr dist 1
indc<-c(1:dim(x3)[1])[dst1<summary(dst1)[2]]
indc2<-c(1:dim(x3)[1])[dst1>=summary(dst1)[2]&dst1<summary(dst1)[3]]

setColors.ggobi(rep(1,length(indc)),indc)
setGlyphs.ggobi(rep(5,length(indc)),rep(3,length(indc)),indc)
setColors.ggobi(rep(2,length(indc2)),indc2)
setGlyphs.ggobi(rep(5,length(indc2)),rep(3,length(indc2)),indc2)

# Color according to corr dist 2
indc<-c(1:dim(x3)[1])[dst4<summary(dst4)[2]]
indc2<-c(1:dim(x3)[1])[dst4>=summary(dst4)[2]&dst4<summary(dst4)[3]]

setColors.ggobi(rep(1,length(indc)),indc)
setGlyphs.ggobi(rep(5,length(indc)),rep(3,length(indc)),indc)
setColors.ggobi(rep(2,length(indc2)),indc2)
setGlyphs.ggobi(rep(5,length(indc2)),rep(3,length(indc2)),indc2)

# Color according to euclidean dist
indc<-c(1:dim(x3)[1])[dst5<summary(dst5)[2]]
indc2<-c(1:dim(x3)[1])[dst5>=summary(dst5)[2]&dst5<summary(dst5)[3]]

setColors.ggobi(rep(1,length(indc)),indc)
setGlyphs.ggobi(rep(5,length(indc)),rep(3,length(indc)),indc)
setColors.ggobi(rep(2,length(indc2)),indc2)
setGlyphs.ggobi(rep(5,length(indc2)),rep(3,length(indc2)),indc2)

library(Rggobi)
ggobi() # start up the flea data in ggobi
d.flea<-getData.ggobi()
d.flea.dist<-dist(d.flea[,-7])
d.flea.dend<-hclust(d.flea.dist,method="average")
plot(d.flea.dend)
clust3<-cutree(d.flea.dend,k=3)
addVariable.ggobi(clust3,"c3")
clust4<-cutree(d.flea.dend,k=4)
addVariable.ggobi(clust4,"c4")

d.flea<-read.csv("/Users/dicook/ggobi-svn/papers/book/data/flea.csv")

library(mclust)
bicvals<-EMclust(d.flea[,-1])
smry<-summary(bicvals,d.flea[,-1])
plot(bicvals)
legend(7,-3200,legend=c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV"),pch=c("A","B","C","D","E","F","G","H","I","J"))
cl<-unclass(smry)$classification
addVariable.ggobi(cl,"mc")

# model-based clustering
d.crabs<-read.csv("/Users/dicook/ggobi-svn/papers/book/data/australian-crabs.csv")
d.crabs[d.crabs[,1]==1,1]<-"Blue"
d.crabs[d.crabs[,1]==2,1]<-"Orange"
d.crabs[d.crabs[,2]==1,2]<-"Male"
d.crabs[d.crabs[,2]==2,2]<-"Female"
plot(d.crabs[,4],d.crabs[,5],pch=".", xlab="FL",ylab="RW")
points(d.crabs[d.crabs[,1]=="Blue"&d.crabs[,2]=="Male",4],
  d.crabs[d.crabs[,1]=="Blue"&d.crabs[,2]=="Male",5],col="blue4")
points(d.crabs[d.crabs[,1]=="Blue"&d.crabs[,2]=="Female",4],
  d.crabs[d.crabs[,1]=="Blue"&d.crabs[,2]=="Female",5],col="blue2")
write.csv(d.crabs,"/Users/dicook/ggobi-svn/papers/book/data/australian-crabs2.csv",
  row.names=F,quote=F)

d.crabs<-read.csv("/Users/dicook/ggobi-svn/papers/book/data/australian-crabs.csv")
d.blue.crabs<-d.crabs[d.crabs[,1]=="Blue",c(2,4:8)]

library(help='mclust')
library(mclust)
#postscript("mclust.ps",width=6.0,height=8.0,horizontal=FALSE,paper="special",family="URWHelvetica")

library(RColorBrewer)
mypalette<-brewer.pal(8,"Set1")

bicvals<-EMclust(d.blue.crabs[,2:3])
smry<-summary(bicvals,d.blue.crabs[,2:3])
cl<-unclass(smry)$classification
table(d.blue.crabs[,1],3-cl)

   cl
    1  2 
  1 13 37
  2 50  0

# Utility functions
f.gen.sphere<-function(n=100,p=5) {
 x<-matrix(rnorm(n*p),ncol=p)
 xnew<-t(apply(x,1,norm.vec))
 xnew
 }

norm.vec<-function(x) {
 x<-x/norm(x)
 x
 }

norm<-function(x) { sqrt(sum(x^2))}

# Run model-based clustering on the 4 groups and 5 variables
library(Rggobi)
ggobi() # start up the xml file

library(mclust)
mclst4<-mclustBIC(d.crabs[,4:8],G=1:8,modelNames=c("EEE","EEV","VVV"))
mclst4<-mclustBIC(d.crabs[,4:8],G=4,modelNames="EEV")
smry4<-mclustModel(d.crabs[,4:8],mclst4,G=4,modelNames="EEV")
cl4<-summary(smry4)$classification
table(c(rep(1:4,rep(50,4))),cl4)

par(mfrow=c(1,1))
plot(mclst4)
legend(7,-4250,legend=c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV"),pch=c("A","B","C","D","E","F","G","H","I","J"))
cl4<-smry4$classification
sp.sex<-rep(1,200)
sp.sex[d.crabs[,1]=="Blue"&d.crabs[,2]=="Female"]<-2 # Blue females
sp.sex[d.crabs[,1]=="Orange"&d.crabs[,2]=="Male"]<-3 # Orange males
sp.sex[d.crabs[,1]=="Orange"&d.crabs[,2]=="Female"]<-4 # Orange females
table(sp.sex,cl4)

mclst5<-EMclust(d.crabs[,4:8],G=4,emModelNames="EEV")
smry5<-summary(mclst5,d.crabs[,4:8])
cl5<-smry5$classification
table(sp.sex,cl5)
vc<-smry5$sigma[,,1]
evc<-eigen(vc)
vc2<-(evc$vectors)%*%diag(sqrt(evc$values))%*%t(evc$vectors)
y1<-f.gen.sphere(500,5)
y1<-y1%*%vc2
y1[,1]<-y1[,1]+smry5$mu[1,1]
y1[,2]<-y1[,2]+smry5$mu[2,1]
y1[,3]<-y1[,3]+smry5$mu[3,1]
y1[,4]<-y1[,4]+smry5$mu[4,1]
y1[,5]<-y1[,5]+smry5$mu[5,1]
vc<-smry5$sigma[,,2]
evc<-eigen(vc)
vc2<-(evc$vectors)%*%diag(sqrt(evc$values))%*%t(evc$vectors)
y2<-f.gen.sphere(500,5)
y2<-y2%*%vc2
y2[,1]<-y2[,1]+smry5$mu[1,2]
y2[,2]<-y2[,2]+smry5$mu[2,2]
y2[,3]<-y2[,3]+smry5$mu[3,2]
y2[,4]<-y2[,4]+smry5$mu[4,2]
y2[,5]<-y2[,5]+smry5$mu[5,2]
vc<-smry5$sigma[,,3]
evc<-eigen(vc)
vc2<-(evc$vectors)%*%diag(sqrt(evc$values))%*%t(evc$vectors)
y3<-f.gen.sphere(500,5)
y3<-y3%*%vc2
y3[,1]<-y3[,1]+smry5$mu[1,3]
y3[,2]<-y3[,2]+smry5$mu[2,3]
y3[,3]<-y3[,3]+smry5$mu[3,3]
y3[,4]<-y3[,4]+smry5$mu[4,3]
y3[,5]<-y3[,5]+smry5$mu[5,3]
vc<-smry5$sigma[,,4]
evc<-eigen(vc)
vc2<-(evc$vectors)%*%diag(sqrt(evc$values))%*%t(evc$vectors)
y4<-f.gen.sphere(500,5)
y4<-y4%*%vc2
y4[,1]<-y4[,1]+smry5$mu[1,4]
y4[,2]<-y4[,2]+smry5$mu[2,4]
y4[,3]<-y4[,3]+smry5$mu[3,4]
y4[,4]<-y4[,4]+smry5$mu[4,4]
y4[,5]<-y4[,5]+smry5$mu[5,4]
pairs(y1)
pairs(y2)
pairs(y3)
pairs(y4)
cl<-c(rep(5,500),rep(6,500),rep(7,500),rep(8,500))
x<-cbind(cl,rbind(y1,y2,y3,y4))
dimnames(x)[[2]]<-c("sp.sex","FL","RW","CL","CW","BD")
d.crabs.model<-rbind(cbind(sp.sex,d.crabs[,4:8]),x)
library(rggobi)
ggobi(d.crabs.model)

# Draw nice pictures
library(DescribeDisplay)
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/crabs7.R")
d.tmp$title="Data"
plot(d.tmp,axislocation=c(0.85,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/crabs8.R")
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#377EB8"]<-"grey10"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey30"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#FFFF33"]<-"grey60"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#FF7F00"]<-"grey80"
d.tmp$plots[[1]]$points$cex<-rep(0.5,2000)
d.tmp$plots[[1]]$points$pch<-rep(16,2000)
d.tmp$title="Model"
plot(d.tmp,axislocation=c(0.85,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/crabs9.R")
d.tmp$title="Data"
plot(d.tmp,axislocation=c(0.85,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/crabs10.R")
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#377EB8"]<-"grey10"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey30"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#FFFF33"]<-"grey60"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#FF7F00"]<-"grey80"
d.tmp$plots[[1]]$points$cex<-rep(0.5,2000)
d.tmp$plots[[1]]$points$pch<-rep(16,2000)
d.tmp$title="Model"
plot(d.tmp,axislocation=c(0.85,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/crabs11.R")
d.tmp$title="Data"
plot(d.tmp,axislocation=c(0.15,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/crabs12.R")
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#377EB8"]<-"grey10"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey30"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#FFFF33"]<-"grey60"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#FF7F00"]<-"grey80"
d.tmp$plots[[1]]$points$cex<-rep(0.5,2000)
d.tmp$plots[[1]]$points$cex<-rep(0.5,2000)
d.tmp$plots[[1]]$points$pch<-rep(16,2000)
d.tmp$title="Model"
plot(d.tmp,axislocation=c(0.15,0.15))

# hierarchical clustering and prim7
library(Rggobi)
ggobi() # open up PRIM7

d.prim7<-getData.ggobi()
d.prim7.dist<-dist(d.prim7[,-8])
d.prim7.dend<-hclust(d.prim7.dist,method="average")
plot(d.prim7.dend)
clust9<-cutree(d.prim7.dend,k=9)
addVariable.ggobi(clust9,"c9")
scatterplot.ggobi("X1","c9")
d.prim7.clust<-cbind(d.prim7,clust9)
write.table(d.prim7.clust,"prim7-clust.csv",sep=",",quote=F,
  append=T,col.names=T,row.names=F)

  # New code
#d.prim7<-read.csv("/Users/dicook/ggobi-svn/papers/book/data/prim7.csv",row.names=1)
#d.prim7<-d.prim7[,-8]
#write.csv(d.prim7,"/Users/dicook/ggobi-svn/papers/book/data/prim7.csv",row.names=F)
d.prim7<-read.csv("/Users/dicook/ggobi-svn/papers/book/data/prim7.csv")
# Spin-and-brush
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp1.R")
d.tmp$plots[[1]]$points$cex<-0.8
d.tmp$title<-"Initial projection - PC2 vs PC1"
plot(d.tmp,axislocation=c(0.5,0.85))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp2.R")
d.tmp$plots[[1]]$points$cex<-0.8
d.tmp$title<-"...Spin, Stop, Brush, ..."
plot(d.tmp,axislocation=c(0.85,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp3.R")
d.tmp$plots[[1]]$points$cex<-0.8
plot(d.tmp,axislocation=c(0.85,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp4.R")
d.tmp$plots[[1]]$points$cex<-0.8
plot(d.tmp,axislocation=c(0.85,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp5.R")
d.tmp$plots[[1]]$points$cex<-0.8
d.tmp$title<-"...Brush, Brush, Brush, Spin..."
plot(d.tmp,axislocation=c(0.85,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp6.R")
d.tmp$plots[[1]]$points$cex<-0.8
plot(d.tmp,axislocation=c(0.85,0.85))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp7.R")
d.tmp$plots[[1]]$points$cex<-0.8
d.tmp$title<-"...Brush..."
plot(d.tmp,axislocation=c(0.85,0.85))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp8.R")
d.tmp$plots[[1]]$points$cex<-0.8
d.tmp$title<-"...Hide, Brush, Spin..."
plot(d.tmp,axislocation=c(0.85,0.85))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp9.R")
d.tmp$plots[[1]]$points$cex<-0.8
d.tmp$title<-"...Brush..."
plot(d.tmp,axislocation=c(0.85,0.85))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp10.R")
d.tmp$plots[[1]]$points$cex<-0.5
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="grey50"]<-"#FFFFFF"
d.tmp$plots[[1]]$edges$lwd<-c(4,4,4)
d.tmp$title<-"...Hide, Spin, Connect the Dots ..."
plot(d.tmp,axislocation=c(0.15,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp11.R")
d.tmp$plots[[1]]$points$cex<-0.5
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="grey50"]<-"#FFFFFF"
d.tmp$plots[[1]]$edges$lwd<-c(4,4,4,4,4,2)
d.tmp$title<-"...Show more, Connect the Dots, Spin..."
plot(d.tmp,axislocation=c(0.85,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-pp13.R")
d.tmp$plots[[1]]$points$cex<-0.8
d.tmp$title<-"...Finished!"
plot(d.tmp,axislocation=c(0.15,0.85))
d.tmp$plots[[1]]$drawpoints<-FALSE
plot(d.tmp,axislocation=c(0.15,0.85))

library(rggobi)
g<-ggobi(d.prim7)
d<-g[1]
d.prim7.dist<-dist(d)
d.prim7.dend<-hclust(d.prim7.dist,method="average")
plot(d.prim7.dend)
clust9<-cutree(d.prim7.dend,k=9)
abline(h=13,col=2)
glyph_type(d)<-6
glyph_size(d)<-3
glyph_color(d)[clust9==1]<-9 # triangle
glyph_color(d)[clust9==1]<-1
glyph_color(d)[clust9==2]<-9 # one arm
glyph_color(d)[clust9==2]<-1
glyph_color(d)[clust9==3]<-9 # triangle
glyph_color(d)[clust9==3]<-1
glyph_color(d)[clust9==4]<-9 # half-arm
glyph_color(d)[clust9==4]<-1
glyph_color(d)[clust9==5]<-9 # triangle
glyph_color(d)[clust9==5]<-1
glyph_color(d)[clust9==6]<-9 # half-arm
glyph_color(d)[clust9==6]<-1
glyph_color(d)[clust9==7]<-9 # outlier
glyph_color(d)[clust9==7]<-1
glyph_color(d)[clust9==8]<-9 # 2 pts at end of an arm
glyph_color(d)[clust9==8]<-1
glyph_color(d)[clust9==9]<-9 # outlier
glyph_color(d)[clust9==9]<-1

library(DescribeDisplay)
# hierarchical clustering
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-clust1.R")
d.tmp$plots[[1]]$points$cex[d.tmp$plots[[1]]$points$col=="#984EA3"]<-0.8
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey"
d.tmp$title<-"Cluster 1"
plot(d.tmp,axislocation=c(0.85,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-clust2.R")
d.tmp$plots[[1]]$points$cex[d.tmp$plots[[1]]$points$col=="#984EA3"]<-0.8
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey"
d.tmp$title<-"Cluster 2"
plot(d.tmp,axislocation=c(0.85,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-clust3.R")
d.tmp$plots[[1]]$points$cex[d.tmp$plots[[1]]$points$col=="#984EA3"]<-0.8
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey"
d.tmp$title<-"Cluster 3"
plot(d.tmp,axislocation=c(0.85,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-clust4.R")
d.tmp$plots[[1]]$points$cex[d.tmp$plots[[1]]$points$col=="#984EA3"]<-0.8
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey"
d.tmp$title<-"Cluster 4"
plot(d.tmp,axislocation=c(0.85,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-clust5.R")
d.tmp$plots[[1]]$points$cex[d.tmp$plots[[1]]$points$col=="#984EA3"]<-0.8
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey"
d.tmp$title<-"Cluster 5"
plot(d.tmp,axislocation=c(0.15,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-clust6.R")
d.tmp$plots[[1]]$points$cex[d.tmp$plots[[1]]$points$col=="#984EA3"]<-0.8
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey"
d.tmp$title<-"Cluster 6"
plot(d.tmp,axislocation=c(0.15,0.15))
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/prim7-clust7.R")
d.tmp$plots[[1]]$points$cex[d.tmp$plots[[1]]$points$col=="#984EA3"]<-0.8
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#984EA3"]<-"grey"
d.tmp$title<-"Cluster 7"
plot(d.tmp,axislocation=c(0.15,0.15))


# challenge data
x<-matrix(rnorm(100*5),ncol=5)
vc1<-matrix(0.3,5,5)
diag(vc1)<-c(1,1,1,1,1)
x<-x%*%vc1
y<-matrix(rnorm(50*5),ncol=5)
vc2<-matrix(0.5,5,5)
diag(vc2)<-c(1,1,1,1,1)
y<-y%*%vc2
y<-y+matrix(rep(c(2,-2,2,-2,2),50),ncol=5,byrow=T)
x<-rbind(x,y)
y<-matrix(rnorm(75*5),ncol=5)
y<-y+matrix(rep(c(-2,2,-2,2,-2),75),ncol=5,byrow=T)
x<-rbind(x,y)
y<-matrix(rnorm(100*5),ncol=5)
vc2<-matrix(-0.3,5,5)
diag(vc2)<-c(1,1,1,1,1)
vc2[,1]<-c(1,0,0,0,0)
vc2[,2]<-c(0,1,-0.3,0.3,-0.3)
vc2[,3]<-c(0,-0.3,1,-0.3,0.3)
vc2[,4]<-c(0,0.3,-0.3,1,-0.3)
vc2[,5]<-c(0,-0.3,0.3,-0.3,0)
y<-y%*%vc2
y<-y+matrix(rep(c(-3,3,3,-3,-3),100),ncol=5,byrow=T)
x<-rbind(x,y)
f.writeXML(dat1=x,filename="clust1.xml",data.name="cluster challenge")


pairs(rbind(x,y))
ggobi(rbind(x,y))


# The effect of standardzing rows.
x<-matrix(rnorm(500*2),ncol=2)

std.row<-function(x) {
  return((x-mean(x))/sd(x))
}
y<-t(apply(x,1,std.row))
#postscript(file="Std-effect.ps",horizontal=F, width=8,height=4)
par(mfrow=c(1,2),pty="s")
plot(x,pch=16,xlab="Var 1",ylab="Var 2")
title("Bivariate normal sample")
plot(y,pch=16,xlab="Std 1",ylab="Std 2")
title("Row standardized")
#dev.off()

x<-matrix(rnorm(500*3),ncol=3)

std.row<-function(x) {
  return((x-mean(x))/sd(x))
}
y<-t(apply(x,1,std.row))
#postscript(file="Std-effect2.ps",horizontal=F, width=8,height=8)
par(mfrow=c(1,1),pty="s",c(2,2,3,2))
pairs(x,pch=16,labels=c("Var 1","Var 2","Var 3"),
  main="Trivariate normal sample")
#dev.off()

library(rggobi)
ggobi(y)

# New version
library(DescribeDisplay)
par(pty="s") 
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/Std-effect2b.R")
plot(d.tmp)

std.plot2<-source("Std-effect2b.R")
F1<-matrix(std.plot2$value$plots[[1]]$tour2dparams$F,ncol=2,byrow=F)
postscript(file="Std-effect2b.ps",horizontal=F, width=4,height=4)
par(pty="s",mfrow=c(1,1),mar=c(0.5,0.5,1.5,0.5))
plot(std.plot2$value$plots[[1]]$points$x,std.plot2$value$plots[[1]]$points$y,
  pch=16,
  xaxt="n",yaxt="n",xlab="",ylab="",main="Row standardized")
rnx1<-range(std.plot2$value$plots[[1]]$points$x)
rny1<-range(std.plot2$value$plots[[1]]$points$y)
rn1<-max(rnx1,rny1)
for (i in 1:3) {
  lines(c(0,F1[i,1]*rn1/2),c(0,0+F1[i,2]*rn1/2),
    col="gray50")
  text(F1[i,1]*rn1/2+rn1*0.05,F1[i,2]*rn1/2+rn1*0.05,paste("Std ",i),col="black")
}
dev.off()

x<-matrix(rnorm(500*4),ncol=4)

std.row<-function(x) {
  return((x-mean(x))/sd(x))
}
y<-t(apply(x,1,std.row))
postscript(file="Std-effect3.ps",horizontal=F, width=8,height=8)
par(mfrow=c(1,1),pty="s")
pairs(x,pch=16,labels=c("Var 1","Var 2","Var 3","Var 4"),
  main="Four variate normal sample")
dev.off()

library(Rggobi)
ggobi(y)

std.plot3<-source("Std-effect3b.R")
std.plot3$value$plots[[1]]$points$color[std.plot3$value$plots[[1]]$points$color==8]<-16
std.plot3$value$plots[[1]]$points$color[std.plot3$value$plots[[1]]$points$color==0]<-1
std.plot3c<-source("Std-effect3c.R")
std.plot3c$value$plots[[1]]$points$color[std.plot3c$value$plots[[1]]$points$color==8]<-16
std.plot3c$value$plots[[1]]$points$color[std.plot3c$value$plots[[1]]$points$color==0]<-1
F1<-matrix(std.plot3c$value$plots[[1]]$tour2dparams$F,ncol=2,byrow=F)

postscript(file="Std-effect3b.ps",horizontal=F, width=4,height=4)
par(pty="s",mfrow=c(1,1),mar=c(0.5,0.5,1.5,0.5))
#plot(std.plot3$value$plots[[1]]$points$x,std.plot3$value$plots[[1]]$points$y,
#  pch=std.plot3$value$plots[[1]]$points$color,
#  col="grey50",
#  xaxt="n",yaxt="n",xlab=" ",ylab=" ", main="First 2 std vars")
#points(std.plot3$value$plots[[1]]$points$x[
#  std.plot3$value$plots[[1]]$points$color==16],
#  std.plot3$value$plots[[1]]$points$y[
#  std.plot3$value$plots[[1]]$points$color==16],cex=2,pch=16)
#mtext("Chip 1",side=1)
#mtext("Chip 2",side=2)
plot(std.plot3c$value$plots[[1]]$points$x,
  std.plot3c$value$plots[[1]]$points$y,
  pch=std.plot3c$value$plots[[1]]$points$color,
  col="grey50",
  xaxt="n",yaxt="n",xlab=" ",ylab=" ",main="Three PCs of 4 std vars")
points(std.plot3c$value$plots[[1]]$points$x[
  std.plot3c$value$plots[[1]]$points$color==16],
  std.plot3c$value$plots[[1]]$points$y[
  std.plot3c$value$plots[[1]]$points$color==16],cex=2,pch=16)
rnx1<-range(std.plot3c$value$plots[[1]]$points$x)
rny1<-range(std.plot3c$value$plots[[1]]$points$y)
rn1<-max(rnx1,rny1)
for (i in 1:3) {
  lines(c(rnx1[1]+2000,rnx1[1]+2000+F1[i,1]*rn1/5),
    c(rny1[1]+2000,rny1[1]+2000+F1[i,2]*rn1/5),
    col="gray50")
  text(rnx1[1]+2000+F1[i,1]*rn1/5,rny1[1]+2000+F1[i,2]*rn1/5,i,col="black")
}
dev.off()

norm<-function(x) {sqrt(sum(x^2))}
z<-apply(y,1,norm)
hist(z,col=2)

# New material
library(DescribeDisplay)
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim1.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim2.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim3.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim4.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim5.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim6.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim7.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim8.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim10.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim11.R")
d.tmp$title<-""
plot(d.tmp,axislocation=c(0.15,0.15))
summary(d.tmp$plots[[1]]$points$col)

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim12.R")
d.tmp$title<-""
d.tmp$colormap$hiddenColor=c(0.2,0.2,0.2)
plot(d.tmp,axislocation=c(0.15,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim7-2-model.R")
d.tmp$title<-""
d.tmp$plots[[1]]$drawpoints<-FALSE
plot(d.tmp,axislocation=c(0.15,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/R-plots/prim7-2-par.R")
d.tmp$title<-""
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="grey50"]<-"grey90"
d.tmp$plots[[1]]$points$col[d.tmp$plots[[1]]$points$col=="#999999"]<-"#FFFFFF"
ggplot(d.tmp)

# standardize before doing par plot.

x<-cbind(d.tmp$plots[[1]]$points$x,d.tmp$plots[[2]]$points$x,
  d.tmp$plots[[3]]$points$x,d.tmp$plots[[4]]$points$x,
  d.tmp$plots[[5]]$points$x,d.tmp$plots[[6]]$points$x,
  d.tmp$plots[[7]]$points$x)
range(x)
f.std.data<-function(x) {
  (x-mean(x,na.rm=T))/sd(x,na.rm=T)
}
f.min.max<-function(x) {
  (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
}
x<-apply(x,2,f.std.data)
x<-apply(x,2,f.min.max)
y<-x[d.tmp$plots[[1]]$points$col=="grey50",]
plot(c(1:7),y[1,],type="l",ylim=c(0,1),axes=F,xlab="Variable",ylab="")
axis(side=1,at=1:7,labels=c("X1","X2","X3","X4","X5","X6","X7"))
rect(0,-1,10,1.5,col="grey80")
abline(h=seq(0,1,by=0.2),col="white")
abline(v=1:7,col="white")
for (i in 1:nrow(y))
  lines(1:7,y[i,],col="grey50")
y<-x[d.tmp$plots[[1]]$points$col=="#999999",]
for (i in 1:nrow(y))
  lines(1:7,y[i,],col="yellow",lwd=2)
box(col="gray70")
 

#SOM
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-SOM1a.R")
d.tmp$title<-"Tour Projection"
plot(d.tmp,axislocation=c(0.8,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-SOM1b.R")
d.tmp$title<-"Map"
plot(d.tmp)

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-SOM2b.R")
d.tmp$title<-"Tour Projection"
plot(d.tmp,axislocation=c(0.15,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-SOM2c.R")
d.tmp$title<-"Tour Projection"
plot(d.tmp,axislocation=c(0.80,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-SOM2d.R")
d.tmp$title<-"Tour Projection"
plot(d.tmp,axislocation=c(0.15,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-SOM2e.R")
d.tmp$title<-"Tour Projection"
plot(d.tmp,axislocation=c(0.80,0.15))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-SOM2f.R")
d.tmp$title<-"Map"
plot(d.tmp)


d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-hc1a.R")
d.tmp$plots[[1]]$points$y<-jitter(d.tmp$plots[[1]]$points$y,1.3)
d.tmp$plots[[1]]$points$x<-jitter(d.tmp$plots[[1]]$points$x,1.3)
d.tmp$plots[[1]]$yscale <- expand_range(d.tmp$plots[[1]]$yscale, 0.1)
d.tmp$plots[[1]]$xscale <- expand_range(d.tmp$plots[[1]]$xscale, 0.1)
d.tmp$title<-"Confusion Table"
plot(d.tmp)

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-hc1b.R")
d.tmp$title<-"Tour Projection"
plot(d.tmp,axislocation=c(0.85,0.85))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-hc2a.R")
d.tmp$plots[[1]]$points$y<-jitter(d.tmp$plots[[1]]$points$y,1.3)
d.tmp$plots[[1]]$points$x<-jitter(d.tmp$plots[[1]]$points$x,1.3)
d.tmp$plots[[1]]$yscale <- expand_range(d.tmp$plots[[1]]$yscale, 0.1)
d.tmp$plots[[1]]$xscale <- expand_range(d.tmp$plots[[1]]$xscale, 0.1)
d.tmp$title<-"Confusion Table"
plot(d.tmp)

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music/music-hc2b.R")
d.tmp$title<-"Tour Projection"
plot(d.tmp,axislocation=c(0.85,0.85))

# Music data - PCA
d.music<-read.csv("/Users/dicook/ggobi-svn/papers/book/data/music-plusnew-sub.csv",row.names=1)
d.music.pca<-princomp(d.music[,-c(1,2)],cor=T,scores=T)
par(pty="s")
plot(d.music.pca$scores[,1],d.music.pca$scores[,2],type="n",
  xlab="PC 1",ylab="PC 2",xlim=c(-7,4),ylim=c(-2.5,4.5))
rect(-8,-3,5,5,col="gray90")
abline(v=seq(-6,4,2),col="white")
abline(h=-2:4,col="white")
text(d.music.pca$scores[,1],d.music.pca$scores[,2],rownames(d.music.pca$scores))
title("Principal Components")
box(col="gray80")

# Music data - SOM
d.music.std<-cbind(d.music[,c(1,2)],apply(d.music[,-c(1,2)],2,f.std.data))
library(som)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=100)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=200)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=300)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=400)
music.som<-som(d.music.std[,-c(1:2)],6,6,rlen=1000)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=100)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=200)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=300)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=400)
music.som<-som(d.music.std[,-c(1:2)],6,6,neigh="bubble",rlen=1000)
music.som<-som(d.music.std[,-c(1:2)],6,6,init="random",neigh="bubble",rlen=100)
music.som<-som(d.music.std[,-c(1:2)],6,6,init="random",neigh="bubble",
  rlen=1000)

xmx<-jitter(music.som$visual$x,factor=2)
xmy<-jitter(music.som$visual$y,factor=2)
par(mfrow=c(1,1),pty="s")
plot(xmx,xmy,type="n",pch=16,xlab="x",ylab="y",main="SOM Map",
  xlim=c(-0.8,6.5),ylim=c(-0.5,6))
rect(-1.5,-1,8,8,col="gray90")
abline(v=c(0:6),col="white")
abline(h=c(0:6),col="white")
points(matrix(c(rep(0:6,7),rep(0:6,rep(7,7))),ncol=2),pch=16,col="white",cex=2)
text(xmx[d.music[,2]=="New wave"],xmy[d.music[,2]=="New wave"],
  dimnames(d.music)[[1]][d.music[,2]=="New wave"],col="black")
text(xmx[d.music[,2]=="Rock"],xmy[d.music[,2]=="Rock"],
  dimnames(d.music)[[1]][d.music[,2]=="Rock"],col=Purple)
text(xmx[d.music[,2]=="Classical"],xmy[d.music[,2]=="Classical"],
  dimnames(d.music)[[1]][d.music[,2]=="Classical"],col=Green)
box(col="grey80")

dimnames(music.som$code)<-list(NULL,names(d.music[,-c(1,2)]))
d.music.clust<-cbind(d.music.std,xmx,xmy)
dimnames(d.music.clust)[[2]][8]<-"Map 1"
dimnames(d.music.clust)[[2]][9]<-"Map 2"
d.music.grid<-cbind(rep("0",36),rep("0",36),music.som$code,
  music.som$code.sum[,1:2])
dimnames(d.music.grid)[[2]][1]<-"Artist"
dimnames(d.music.grid)[[2]][2]<-"Type"
dimnames(d.music.grid)[[2]][8]<-"Map 1"
dimnames(d.music.grid)[[2]][9]<-"Map 2"
d.music.clust<-rbind(d.music.grid,d.music.clust)

# Setting up the net lines
n.nodes<-6
x33.l<-NULL
for (i in 1:n.nodes) {
  for (j in 1:n.nodes) {
    if (j<n.nodes) x33.l<-rbind(x33.l,c((i-1)*n.nodes+j,(i-1)*n.nodes+j+1))
    if (i<n.nodes) x33.l<-rbind(x33.l,c((i-1)*n.nodes+j,i*n.nodes+j))
}}

source("/Users/dicook/EDA/lectures/writeXML.R")
f.writeXML(d.music.clust,
  "/Users/dicook/ggobi-svn/papers/book/data/music-SOM1.xml",
  data.num=2,dat1.id<-c(1:dim(d.music.clust)[1]),
  dat2=cbind(c(1:60),c(1:60)),
  dat2.source=x33.l[,1],
  dat2.destination=x33.l[,2],
  dat2.name="SOM",dat2.id=paste(rep("l",60),c(1:60)))

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music-SOM1a-bad.R")
d.tmp$title<-"Map"
d.tmp$plots[[1]]$points$cex[d.tmp$plots[[1]]$points$col!="#999999"]<-1.3
plot(d.tmp)

d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music-SOM1b-bad.R")
d.tmp$title<-"Tour Projection"
d.tmp$plots[[1]]$points$cex[d.tmp$plots[[1]]$points$col!="#999999"]<-1.3
plot(d.tmp,axislocation=c(0.15,0.15))

# Hierarchical clustering
d.tmp<-dd_load("/Users/dicook/ggobi-svn/papers/book/chap-clust/music-clust1.R")
plot(d.tmp)
