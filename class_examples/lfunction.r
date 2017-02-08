
#############################################
#############################################
#
#  Some plots for examing the Ripley's K
#
#############################################
#############################################

library(spatstat)
library(splancs)
source("http://reuningscherer.net/fes781/rscripts/SpatialPPFuncs.R.txt ")

par(mfrow=c(3,2),mar=c(5,4,4,2))

#do some comparison K plots

#first do regular grid
x10<-rep(c(1:10),10)
y10<-sort(x10)
par(pty="s")
plot(x10,y10,pch=19,col='red',xlab="",ylab="",main="Regular Grid")
grid.ppp<-ppp(x10,y10,xrange=range(x10),yrange=range(y10))
grid.env<-envelope(grid.ppp)
grid.K<-Kest(grid.ppp)
Kplot(grid.env,grid.K,0,20,"Regular Grid")

#100 random points on square (CSR)
x<-runif(100,1,100)
y<-runif(100,1,100)
par(pty="s")
plot(x,y,pch=19,col='red',xlab="",ylab="",main="CSR")
CSR.ppp<-ppp(x,y,xrange=range(x),yrange=range(y))
CSR.env<-envelope(CSR.ppp)
CSR.K<-Kest(CSR.ppp)
Kplot(CSR.env,CSR.K,0,2000,"Randomly Generated CSR")

#10 clusters of 10 points
xa<-runif(100)
ya<-runif(100)
movex<-runif(10,1,10)
movex<-sort(rep(movex,10))+xa
movey<-runif(10,1,10)
movey<-rep(movey,each=10)+ya
par(pty="s")
plot(movex,movey,pch=19,col='red',xlab="",ylab="",main="Clustered Process")

Clust.ppp<-ppp(movex, movey,xrange=range(movex),yrange=range(movey))
Clust.env<-envelope(Clust.ppp)
Clust.K<-Kest(Clust.ppp)
Kplot(Clust.env,Clust.K,0,15,"Clustered Process")


#do some comparison L plots

#first do regular grid
x10<-rep(c(1:10),10)
y10<-sort(x10)
par(pty="s")
plot(x10,y10,pch=19,col='red',xlab="",ylab="",main="Regular Grid")
grid.ppp<-ppp(x10,y10,xrange=range(x10),yrange=range(y10))
grid.env<-envelope(grid.ppp,r=seq(0,5,.01))
grid.K<-Kest(grid.ppp,r=seq(0,5,0.01))
L(grid.env,grid.K,2,-.55,"Regular Grid")

#100 random points on square (CSR)
x<-runif(100,1,100)
y<-runif(100,1,100)
par(pty="s")
plot(x,y,pch=19,col='red',xlab="",ylab="",main="CSR")
CSR.ppp<-ppp(x,y,xrange=range(x),yrange=range(y))
CSR.env<-envelope(CSR.ppp,r=seq(0,50,.1))
CSR.K<-Kest(CSR.ppp,r=seq(0,50,.1))
L(CSR.env,CSR.K,2,-1.8,"Randomly Generated CSR")

#10 clusters of 10 points
xa<-runif(100)
ya<-runif(100)
movex<-runif(10,1,10)
movex<-sort(rep(movex,10))+xa
movey<-runif(10,1,10)
movey<-rep(movey,each=10)+ya
par(pty="s")
plot(movex,movey,pch=19,col='red',xlab="",ylab="",main="Clustered Process")

Clust.ppp<-ppp(movex, movey,xrange=range(movex),yrange=range(movey))
Clust.env<-envelope(Clust.ppp,r=seq(0,5.5,.01))
Clust.K<-Kest(Clust.ppp,r=seq(0,5.5,.01))
L(Clust.env,Clust.K,0,-0.25,"Clustered Process")





##################################################################
####  NOW SOME REAL DATA
##################################################################

###################################
#UGANDA VOLCANO DATA
###################################


 ugpoly=read.csv("http://reuningscherer.net/fes781/data/ugpoly.csv")
 ugpts=read.csv("http://reuningscherer.net/fes781/data/ugdata.csv")
ug.pts=as.points(ugpts[,2],ugpts[,3])
ug.poly=as.points(ugpoly[,2],ugpoly[,3])
 ugppp=ppp(ug.pts[,1],ug.pts[,2],xrange=range(ug.poly[,1]),yrange=range(ug.poly[,2]))

par(mfrow=c(2,2))

plot(ug.pts[,1],ug.pts[,2],pch=20,cex=1.2,col="red",,main="Uganda Volcanos",xlab="", ylab=""
, xlim=c(0,3500))
polymap(ug.poly,add=T)

ug.env<-envelope(ugppp,r=seq(0,500,1))
ug.K<-Kest(ugppp,r=seq(0,500,1))
L(ug.env,ug.K,0,300,"Uganda Volcanos")


###################################
#CARDIFF JUVENLE DELINQUENCY DATA
###################################

cardpoly=read.csv("http://reuningscherer.net/fes781/data/cardpoly.csv")
cardpts=read.csv("http://reuningscherer.net/fes781/data/cardpts.csv")
card.pts=as.points(cardpts[,2],cardpts[,3])
card.poly=as.points(cardpoly[,2],cardpoly[,3])
cardppp=ppp(card.pts[,1],card.pts[,2],xrange=range(card.poly[,1]),yrange=range(card.poly[,2]))

plot(card.pts[,1],card.pts[,2],pch=20,cex=1.2,col="red",,main="Cardiff Delinquents",xlab="", ylab=""
,)
polymap(card.poly,add=T)

card.env<-envelope(cardppp,r=seq(0,40,.1))
card.K<-Kest(cardppp,r=seq(0,40,.1))
L(card.env,card.K,0,7,"Cardiff Delinquents")




######################################################################
#This is an example of the problem of parsing out what is mean vs variance
######################################################################

#Example from last time where mean increases from one corner to another in plot

par(mfrow=c(1,2), pty="s")
x <- 1:20
y <- 1:20
z <- matrix(0,20,20)
for (i in 1:20) {
  for (j in 1:20) {
    z[i,j] <- (i^2+j^2)/800
  }
}
lamb <- function(x,y) { (x^2+y^2)/800 }
tempppp <- rpoispp(lamb, lmax=100,win=owin(c(1,20),c(1,20)))
plot(tempppp$x,tempppp$y,xlim=c(0,20),ylim=c(0,20),xlab="u",ylab="v",cex.lab=1.5,pch=19,col='red')
contour(x,y,z,lwd=1,lty=3,add=T,drawlabels=F)
test.env<-envelope(tempppp,r=seq(0,30,.1))
test.K<-Kest(tempppp,r=seq(0,30,.1))
L(test.env,test.K,9,.8,"Example of Int. Mean as Variance")






