
#########################################
#########################################
#
#   Lfunction.R.txt
#
#
#  The function L makes the corrected L plot ala Waller and Gotway
#  that is, sqrt(K/pi)-r vs r, using several corrections for the calculation
#  of K.  Calculation may take some time!
#  Notice that there are two required inputs
#    1)  the output of the envelope() function and
#    2)  the output of the Kest() function
#  Calculation of these may take some time.
#  X and Y are the upper left corner of the legend - you'll probably
#  have to run twice to get the location correct!
#
###############################################
###############################################


#######################################
#  L Function
#######################################

L <- function (envdata, Kdata, x=1, y=1, datatitle="Data"){
  dataenv<-envdata
  dataK<-Kdata
  dataenv$obs<-sqrt(dataenv$obs/pi)-dataenv$r
  dataenv$theo<-sqrt(dataenv$theo/pi)-dataenv$r
  dataenv$lo<-sqrt(dataenv$lo/pi)-dataenv$r
  dataenv$hi<-sqrt(dataenv$hi/pi)-dataenv$r

  dataK$theo<-sqrt(dataK$theo/pi)-dataK$r
  dataK$border<-sqrt(dataK$border/pi)-dataK$r
  dataK$trans<-sqrt(dataK$trans/pi)-dataK$r
  dataK$iso<-sqrt(dataK$iso/pi)-dataK$r


  plrange<-range(c(dataK$border[is.finite(dataK$border)], 
                 dataK$trans[is.finite(dataK$trans)],
                 dataK$iso[is.finite(dataK$iso)],
		     dataenv$lo[is.finite(dataenv$lo)],
                 dataenv$hi[is.finite(dataenv$hi)]))
  pllength<-max(length(dataK$border[is.finite(dataK$border)]),
              length(dataK$trans[is.finite(dataK$trans)]),
              length(dataK$iso[is.finite(dataK$iso)]))
par(pty="m")
plot(dataenv$r[1:pllength],dataenv$obs[1:pllength],pch="",
   main=paste("L-plot for",datatitle), ylim=plrange,xlab="Distance (h)",
   ylab="sqrt(K/pi)-h")
lines(dataenv$r,dataenv$hi,lty=2,col="Red",lwd=2)
lines(dataenv$r,dataenv$lo,,lty=2,col="Red",lwd=2)
lines(dataK$r,dataK$theo,,lty=1,col="Black",lwd=2)
lines(dataK$r,dataK$border,,lty=1,col="Yellow",lwd=2)
lines(dataK$r,dataK$trans,,lty=1,col="Green",lwd=2)
lines(dataK$r,dataK$iso,,lty=1,col="Blue",lwd=2)
legend(x,y,c("CSR", "Boundaries", "Border Corrected","Trans Corrected","Iso Corrected"),
       lty=c(1,2,1,1,1),col=c("Black","Red","Yellow","Green","Blue"),
       lwd=c(2,2,2,2,2),cex=0.5)
}



#########################################
#
#  Kplot
#  
#  The function Kplot basically does the same thing as the L function above
#  but doesn't make the transformation to make the plot linear
###############################################


Kplot <- function (envdata, Kdata, x=1, y=1, datatitle="Data"){
  dataenv<-envdata
  dataK<-Kdata
  plrange<-range(c(dataK$border[is.finite(dataK$border)], 
                 dataK$trans[is.finite(dataK$trans)],
                 dataK$iso[is.finite(dataK$iso)],
		     dataenv$lo[is.finite(dataenv$lo)],
                 dataenv$hi[is.finite(dataenv$hi)]))
  pllength<-max(length(dataK$border[is.finite(dataK$border)]),
              length(dataK$trans[is.finite(dataK$trans)]),
              length(dataK$iso[is.finite(dataK$iso)]))
par(pty="m")
plot(dataenv$r[1:pllength],dataenv$obs[1:pllength],pch="",
   main=paste("Ripleys K-plot for",datatitle), ylim=plrange,xlab="Distance (h)",
   ylab="Ripley's K")
lines(dataenv$r,dataenv$hi,lty=2,col="Red",lwd=2)
lines(dataenv$r,dataenv$lo,,lty=2,col="Red",lwd=2)
lines(dataK$r,dataK$theo,,lty=1,col="Black",lwd=2)
lines(dataK$r,dataK$border,,lty=1,col="Yellow",lwd=2)
lines(dataK$r,dataK$trans,,lty=1,col="Green",lwd=2)
lines(dataK$r,dataK$iso,,lty=1,col="Blue",lwd=2)
legend(x,y,c("CSR", "Boundaries", "Border Corrected","Trans Corrected","Iso Corrected"),
       lty=c(1,2,1,1,1),col=c("Black","Red","Yellow","Green","Blue"),
       lwd=c(2,2,2,2,2),cex=0.5)
}



#########################################################
########################################################
##  G-function
##########################################################
#########################################################

par(mfrow=c(1,1))
G <- function (envdata, Kdata, datatitle){
  dataenv<-envdata
  dataK<-Kdata
  pllength<-max(length(dataK$rs[is.finite(dataK$rs)]),
              length(dataK$km[is.finite(dataK$km)]),
              length(dataK$theo[is.finite(dataK$theo)]))
  xlim1<-range(dataK$r[dataK$km<1])*1.25
  xlim2<-range(dataK$r[dataK$r<1])*1.25
  xlims<-range(xlim1,xlim2)
par(pty="m")
plot(dataenv$r[1:pllength],dataenv$obs[1:pllength],pch="",
   main=paste("G-plot for",datatitle), xlim=xlims, ylim=c(0,1),xlab="Distance (r)",
   ylab="G(r)")
lines(dataenv$r,dataenv$hi,lty=2,col="Red",lwd=2)
lines(dataenv$r,dataenv$lo,,lty=2,col="Red",lwd=2)
lines(dataK$r,dataK$theo,,lty=1,col="Black",lwd=2)
lines(dataK$r,dataK$rs,,lty=1,col="Yellow",lwd=2)
lines(dataK$r,dataK$km,,lty=1,col="Green",lwd=2)
lines(dataK$r,dataK$han,,lty=1,col="Blue",lwd=2)
legend(0,1,c("CSR", "Boundaries", "Border Corrected","KM Corrected","Hanish"),
       lty=c(1,2,1,1,1),col=c("Black","Red","Yellow","Green","Blue"),
       lwd=c(2,2,2,2,2),cex=0.5)
par(pty="s")
}





#########################################################
########################################################
##  F-function
##########################################################
#########################################################

Ffunc <- function (envdata, Kdata, datatitle){
  dataenv<-envdata
  dataK<-Kdata
  pllength<-max(length(dataK$rs[is.finite(dataK$rs)]),
              length(dataK$km[is.finite(dataK$km)]),
              length(dataK$theo[is.finite(dataK$theo)]))
  xlim1<-range(dataK$r[dataK$km<1])*1.25
  xlim2<-range(dataK$r[dataK$r<1])*1.25
  xlims<-range(xlim1,xlim2)
par(pty="m")
plot(dataenv$r[1:pllength],dataenv$obs[1:pllength],pch="",
   main=paste("F-plot for",datatitle), xlim=xlims, ylim=c(0,1),xlab="Distance (r)",
   ylab="F(r)")
lines(dataenv$r,dataenv$hi,lty=2,col="Red",lwd=2)
lines(dataenv$r,dataenv$lo,,lty=2,col="Red",lwd=2)
lines(dataK$r,dataK$theo,,lty=1,col="Black",lwd=2)
lines(dataK$r,dataK$rs,,lty=1,col="Yellow",lwd=2)
lines(dataK$r,dataK$km,,lty=1,col="Green",lwd=2)
lines(dataK$r,dataK$cs,,lty=1,col="Blue",lwd=2)
legend(0,1,c("CSR", "Boundaries", "Border Corrected","KM Corrected","Chiu-Stoyan"),
       lty=c(1,2,1,1,1),col=c("Black","Red","Yellow","Green","Blue"),
       lwd=c(2,2,2,2,2),cex=0.5)
par(pty="s")
}



