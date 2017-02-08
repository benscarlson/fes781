##########################################################
##                                                      ##
##  Examples of Spatial PP                              ##
##  reuningscherer.net/fes781/                          ##
##########################################################

#get spatstat package loaded
library(spatstat)

# 1. import population and store population size and total stem weight

maine <- read.fwf("http://reuningscherer.net/fes781/data/maine.txt", skip=44,
                  col.names=c("treecode","spp","crown", "dbh", "ht", "stemwt", "x", "y"),
                  widths=c(7,3,3,8,8,8,8,8), strip.white=T)
maine$ba <- pi*(maine$dbh/2/12)^2

maine$dbh.cm <- maine$dbh * 2.54;  maine$height.m <- maine$ht * .3048
maine$ba.m2 <- maine$ba * .3048**2

maine$tree.label <- 1:nrow(maine); maine$tree.count <- rep(1, nrow(maine))
maine$stemwt.kg <- maine$stemwt / 2.2046
attach(maine)


N <- nrow(maine)       # store population size
N                      # display population size

spruce <- maine[spp == 1,]  ; spruce$Nsp <- nrow(spruce)
hemlock <- maine[spp == 2,]  ; hemlock$Nhe <- nrow(hemlock)
redmaple <- maine[spp == 3,]  ; redmaple$Nrm <- nrow(redmaple)
cedar <- maine[spp == 4,]  ; cedar$Nce <- nrow(cedar)
balsamfir <- maine[spp == 5,]  ; balsamfir$Nbf <- nrow(balsamfir)
paperbirch <- maine[spp == 6,]  ; paperbirch$Npb <- nrow(paperbirch)
whitepine <- maine[spp == 7,]  ; whitepine$Nwp <- nrow(whitepine)


#Example of a marked process - categorical marking

paperbirch.ppp<-ppp(paperbirch$x,paperbirch$y,xrange=range(paperbirch$x),yrange=range(paperbirch$y))
whitepine.ppp<-ppp(whitepine$x,whitepine$y,xrange=range(whitepine$x),yrange=range(whitepine$y))

plot(paperbirch.ppp$x,paperbirch.ppp$y,pch=19,col='red',xlab="X",ylab="Y",main="Maine Tree Data - Marked Point Process")
points(whitepine.ppp$x,whitepine.ppp$y,pch=5,col='blue',xlab="X",ylab="Y",main="White Pine")
legend(280,625,legend=c("P. Birch","W. Pine"), col=c('red','blue'),pch=c(19,5))


#Example of a marked process - continuous marking
paperbirch.ppp<-ppp(paperbirch$x,paperbirch$y,xrange=range(paperbirch$x),yrange=range(paperbirch$y), marks=paperbirch$dbh)
plot(paperbirch.ppp, legend=FALSE, main="Paper Birch - Diameter=DBH",pch=19,cols='red')

#Examples of a covariate - tropical rain forest data with elevation
#load data
data(bei)
#make elevation plot
plot(bei.extra$elev, main="Relationship of Covariate to Points")
points(bei, pch=3, cex=.5)
