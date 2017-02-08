######################################################################
#  kernelsmooth.r.txt
#
#
#  Some examples of kernel smoothing
#  Requires spalncs, spatstat, animation packages
#
######################################################################

library(splancs)
library(spatstat)
library(animation)

######################################################################
#  Three Heterogeneous Spatial PPP as discussed in W&G Chapter 5, p. 129
######################################################################


x <- 1:20
y <- 1:20

z <- matrix(0,20,20)
mu1 <- c(3,3)
mu2 <- c(16,14)
sigmasq1 <- 6
sigmasq2 <- 12
Siginv <- matrix(0,2,2)
Siginv[1,1] <- 1/sigmasq1
Siginv[2,2] <- 1/sigmasq2

Siginv2 <- matrix(0,2,2)
Siginv2[1,1] <- 1/60
Siginv2[2,2] <- 1/35

for (i in 1:20) {
  for (j in 1:20) {
    z[i,j] <- (1/(2*pi))*exp((-1/2)*
              ( (c(x[i],y[j]) - mu1)%*%Siginv%*%(c(x[i],y[j]) - mu1) ) )+
              (1/(2*pi))*exp((-1/2)*
              ( (c(x[i],y[j]) - mu2)%*%Siginv2%*%(c(x[i],y[j]) - mu2) ) )
  }
  print(i)
}

par(mfcol=c(4,3),pty='s',mar=c(2,2,2,2)) #s means square plot

xrand <- runif(300,min=0,max=20)
yrand <- runif(300,min=0,max=20)
test <- runif(300,min=,max=max(z))
zval <- 1:300
for (i in 1:300) {
   zval[i] <- z[trunc(xrand[i])+1,trunc(yrand[i])+1]
  }
plot1x <- xrand[test<zval][1:100]
plot1y <- yrand[test<zval][1:100]

plot(plot1x,plot1y,xlim=c(0,20),ylim=c(0,20),xlab="u",ylab="v",cex.lab=1.5,pch=19,col='red')
contour(x,y,z,lwd=1,lty=3,add=T,drawlabels=F)


temp<-ppp(plot1x,plot1y,xrange=c(0,20),yrange=c(0,20))
dens1<-density.ppp(temp,sigma=1,dimyx=c(20,20))
dens2<-density.ppp(temp,sigma=2,dimyx=c(20,20))
dens3<-density.ppp(temp,sigma=3,dimyx=c(20,20))
persp(dens1,theta=45,phi=45,xlab="u",ylab="v",zlab="lambda",cex.lab=1.5,main="Sigma=1") #these are wire plots
persp(dens2,theta=45,phi=45,xlab="u",ylab="v",zlab="lambda",cex.lab=1.5,main="Sigma=2")
persp(dens3,theta=45,phi=45,xlab="u",ylab="v",zlab="lambda",cex.lab=1.5,main="Sigma=3")



for (i in 1:20) {
  for (j in 1:20) {
    z[i,j] <- (i^2+j^2)/800
  }
}


lamb <- function(x,y) { (x^2+y^2)/800 }
temp <- rpoispp(lamb, lmax=100,win=owin(c(1,20),c(1,20)))
plot(temp$x,temp$y,xlim=c(0,20),ylim=c(0,20),xlab="u",ylab="v",cex.lab=1.5,pch=19,col='red')
contour(x,y,z,lwd=1,lty=3,add=T,drawlabels=F)

dens1<-density.ppp(temp,sigma=1,dimyx=c(20,20))
dens2<-density.ppp(temp,sigma=2,dimyx=c(20,20))
dens3<-density.ppp(temp,sigma=3,dimyx=c(20,20))
persp(dens1,theta=45,phi=45,xlab="u",ylab="v",zlab="lambda",cex.lab=1.5,main="Sigma=1") #these are contour plots
persp(dens2,theta=45,phi=45,xlab="u",ylab="v",zlab="lambda",cex.lab=1.5,main="Sigma=2")
persp(dens3,theta=45,phi=45,xlab="u",ylab="v",zlab="lambda",cex.lab=1.5,main="Sigma=3")



for (i in 1:20) {
  for (j in 1:20) {
    z[i,j] <- ((i-10)^2+(j-10)^2)/200
  }
}


lamb <- function(x,y) { ((x-10)^2+(y-10)^2)/200 }
temp <- rpoispp(lamb, lmax=100,win=owin(c(1,20),c(1,20)))
plot(temp$x,temp$y,xlim=c(0,20),ylim=c(0,20),xlab="u",ylab="v",cex.lab=1.5,pch=19,col='red')
contour(x,y,z,lwd=1,lty=3,add=T,drawlabels=F)

dens1<-density.ppp(temp,sigma=1,dimyx=c(20,20))
dens2<-density.ppp(temp,sigma=2,dimyx=c(20,20))
dens3<-density.ppp(temp,sigma=3,dimyx=c(20,20))
persp(dens1,theta=45,phi=45,xlab="u",ylab="v",zlab="lambda",cex.lab=1.5,main="Sigma=1")
persp(dens2,theta=45,phi=45,xlab="u",ylab="v",zlab="lambda",cex.lab=1.5,main="Sigma=2")
persp(dens3,theta=45,phi=45,xlab="u",ylab="v",zlab="lambda",cex.lab=1.5,main="Sigma=3")


########################################
#Same thing again but with contour plots
#########################################



for (i in 1:20) {
  for (j in 1:20) {
    z[i,j] <- (1/(2*pi))*exp((-1/2)*
              ( (c(x[i],y[j]) - mu1)%*%Siginv%*%(c(x[i],y[j]) - mu1) ) )+
              (1/(2*pi))*exp((-1/2)*
              ( (c(x[i],y[j]) - mu2)%*%Siginv2%*%(c(x[i],y[j]) - mu2) ) )
  }
  print(i)
}

par(mfcol=c(4,3),pty='s',mar=c(2,2,2,2))

xrand <- runif(300,min=0,max=20)
yrand <- runif(300,min=0,max=20)
test <- runif(300,min=,max=max(z))
zval <- 1:300
for (i in 1:300) {
   zval[i] <- z[trunc(xrand[i])+1,trunc(yrand[i])+1]
  }
plot1x <- xrand[test<zval][1:100]
plot1y <- yrand[test<zval][1:100]

plot(plot1x,plot1y,xlim=c(0,20),ylim=c(0,20),xlab="u",ylab="v",cex.lab=1.5,pch=19,col='red')
contour(x,y,z,lwd=1,lty=3,add=T,drawlabels=F)


temp<-ppp(plot1x,plot1y,xrange=c(0,20),yrange=c(0,20))
dens1<-density.ppp(temp,sigma=1,dimyx=c(20,20))
dens2<-density.ppp(temp,sigma=2,dimyx=c(20,20))
dens3<-density.ppp(temp,sigma=3,dimyx=c(20,20))
contour(dens1,xlab="u",ylab="v",main="Sigma=1")
contour(dens2,xlab="u",ylab="v",main="Sigma=2")
contour(dens3,xlab="u",ylab="v",main="Sigma=3")



for (i in 1:20) {
  for (j in 1:20) {
    z[i,j] <- (i^2+j^2)/800
  }
}


lamb <- function(x,y) { (x^2+y^2)/800 }
temp <- rpoispp(lamb, lmax=100,win=owin(c(1,20),c(1,20)))
plot(temp$x,temp$y,xlim=c(0,20),ylim=c(0,20),xlab="u",ylab="v",cex.lab=1.5,pch=19,col='red')
contour(x,y,z,lwd=1,lty=3,add=T,drawlabels=F)

dens1<-density.ppp(temp,sigma=1,dimyx=c(20,20))
dens2<-density.ppp(temp,sigma=2,dimyx=c(20,20))
dens3<-density.ppp(temp,sigma=3,dimyx=c(20,20))
contour(dens1,xlab="u",ylab="v",main="Sigma=1")
contour(dens2,xlab="u",ylab="v",main="Sigma=2")
contour(dens3,xlab="u",ylab="v",main="Sigma=3")



for (i in 1:20) {
  for (j in 1:20) {
    z[i,j] <- ((i-10)^2+(j-10)^2)/200
  }
}


lamb <- function(x,y) { ((x-10)^2+(y-10)^2)/200 }
temp <- rpoispp(lamb, lmax=100,win=owin(c(1,20),c(1,20)))
plot(temp$x,temp$y,xlim=c(0,20),ylim=c(0,20),xlab="u",ylab="v",cex.lab=1.5,pch=19,col='red')
contour(x,y,z,lwd=1,lty=3,add=T,drawlabels=F)

dens1<-density.ppp(temp,sigma=1,dimyx=c(20,20))
dens2<-density.ppp(temp,sigma=2,dimyx=c(20,20))
dens3<-density.ppp(temp,sigma=3,dimyx=c(20,20))
contour(dens1,xlab="u",ylab="v",main="Sigma=1")
contour(dens2,xlab="u",ylab="v",main="Sigma=2")
contour(dens3,xlab="u",ylab="v",main="Sigma=3")



##################################################################
####  NOW SOME REAL DATA
##################################################################

###################################
#UGANDA VOLCANO DATA
###################################

#kernel density estimates for Volcano Data

ugpoly=read.csv("http://reuningscherer.net/fes781/data/ugpoly.csv")
ugpts=read.csv("http://reuningscherer.net/fes781/data/ugdata.csv")
ug.pts=as.points(ugpts[,2],ugpts[,3])
ug.poly=as.points(ugpoly[,2],ugpoly[,3])
#a plot of the data with a polygon around it.
polymap(ug.poly,add=TRUE)
pointmap(ug.pts,pch=20,cex=2,add=TRUE)

par(mfrow=c(2,2))
par(mar=c(2.1,4,4,2.1))
bandvec=c(100,200,300,1000)
for (i in 1:4)
{
 bandwidth=bandvec[i]
 uglamnx=80
 uglamny=80
 uglamest=kernel2d(ug.pts,ug.poly,bandwidth,uglamnx,uglamny)
 image(uglamest$x,uglamest$y,uglamest$z,asp=1,
col=topo.colors(100),main=paste("Bandwidth = ",bandwidth,", Grid Size = ",uglamnx)
 , xlab="", ylab="")
 polymap(ug.poly,add=TRUE)
 pointmap(ug.pts,pch=20,cex=1.2,add=TRUE)
}


for (i in 1:4)
{
 bandwidth=bandvec[i]
 uglamnx=80
 uglamny=80
 uglamest=kernel2d(ug.pts,ug.poly,bandwidth,uglamnx,uglamny)
 contour(uglamest$x,uglamest$y,uglamest$z,xlab="",ylab="",cex.lab=1.2,
 main=paste("Bandwidth = ",bandwidth,", Grid Size = ",uglamnx),
 xlim=c(0,3000))
 polymap(ug.poly,add=TRUE)
 pointmap(ug.pts,pch=20,cex=1,add=TRUE,col="red")
}




###################################
#CARDIFF JUVENLE DELINQUENCY DATA
###################################

#kernel density estimates for Cardiff

cardpoly=read.csv("http://reuningscherer.net/fes781/data/cardpoly.csv")
cardpts=read.csv("http://reuningscherer.net/fes781/data/cardpts.csv")
card.pts=as.points(cardpts[,2],cardpts[,3])
card.poly=as.points(cardpoly[,2],cardpoly[,3])


par(mfrow=c(2,2))
bandvec=c(5,10,15,20)
for (i in 1:4)
{
 bandwidth=bandvec[i]
 cardlamnx=80
 cardlamny=80
 cardlamest=kernel2d(card.pts,card.poly,bandwidth,cardlamnx,cardlamny)
 image(cardlamest$x,cardlamest$y,cardlamest$z,asp=1, xlab="",ylab="",
col=topo.colors(100),main=paste("Bandwidth = ",bandwidth,", Grid Size = ",cardlamnx))
 polymap(card.poly,add=TRUE)
 pointmap(card.pts,pch=20,cex=1.2,add=TRUE)
}


#plot similar to one suggested in help file for kernel2d

par(mfrow=c(1,1))

plot(card.poly, asp=1, type="n")
image(kernel2d(card.pts, card.poly, h0=2, nx=100, ny=100), 
add=TRUE, col=terrain.colors(20))
pointmap(card.pts, add=TRUE)
polymap(card.poly, add=TRUE)
card.xy <- card.pts
apply(card.poly, 2, range)
grd1 <- GridTopology(cellcentre.offset=c(0.8, 4.8), cellsize=c(1, 1), cells.dim=c(100,100))
k5 <- spkernel2d(card.pts, card.poly, h0=5, grd1)
k10 <- spkernel2d(card.pts, card.poly, h0=10, grd1)
k15 <- spkernel2d(card.pts, card.poly, h0=15, grd1)
k20 <- spkernel2d(card.pts, card.poly, h0=20, grd1)
if (.sp_lt_0.9()) {
  df <- AttributeList(list(k5=k5, k10=k10, k15=k15, k20=k20))
} else {
  df <- data.frame(k5=k5, k10=k10, k15=k15, k20=k20)
}
kernels <- SpatialGridDataFrame(grd1, data=df)
spplot(kernels, checkEmptyRC=FALSE, col.regions=terrain.colors(16), cuts=15)





par(mfrow=c(2,2))

for (i in 1:4)
{
 bandwidth=bandvec[i]
 cardlamnx=80
 cardlamny=80
 cardlamest=kernel2d(card.pts,card.poly,bandwidth,cardlamnx,cardlamny)
 contour(cardlamest$x,cardlamest$y,cardlamest$z,xlab="",ylab="",cex.lab=1.2,main=paste("Bandwidth = ",bandwidth,", Grid Size = ",cardlamnx))
 polymap(card.poly,add=TRUE)
 pointmap(card.pts,pch=20,cex=1,add=TRUE,col="red")
}



#simulation for class

par(mfrow=c(1,1))

oopt = ani.options(interval = 0.7, nmax = 50)


bandvec=c(c(1:20),seq(20,100,10)) #just to 1-10, then skip to 20-100 by 10
for (i in 1:29)
{

 bandwidth=bandvec[i]
 cardlamnx=80
 cardlamny=80
 cardlamest=kernel2d(card.pts,card.poly,bandwidth,cardlamnx,cardlamny)
 image(cardlamest$x,cardlamest$y,cardlamest$z,asp=1,
col=topo.colors(100),main=paste("Bandwidth = ",bandwidth,", Grid Size = ",cardlamnx))
 polymap(card.poly,add=TRUE)
 pointmap(card.pts,pch=20,cex=1.2,add=TRUE)
 ani.pause()  #pause animation
}


