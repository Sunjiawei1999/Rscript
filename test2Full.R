library("deSolve")
library("ggplot2")
library("scatterplot3d")

#旨在描述模型延伸二整个系统的动态变化，以各个内生变量关于时间的变动规律作图；
FullSystem <- function(Time,State,Pars) 
{
  with( as.list(c(State,Pars)), {
    dY <- c1 + (c2 - 1)*Y + c3*P + I 
    dP <- (f*(1-x)/2 - c*(1+x)/2)*(d*Y - P)
    if((v*dY<k1star)&(v*dY>k2star)){dI <- v*c1 + v*(c2 - 1)*Y + v*c3*P + (v - 1)*I}
    if((v*dY) > k1star){dI <- k1star - I}
    if((v*dY) < k2star){dI <- k2star - I}
    dx <- k*(1-x)*exp(mu*wx*x+mu*wy*dY)-k*(1+x)*exp(-mu*wx*x-mu*wy*dY)
    return(list(c(dY, dP, dI, dx)))
 })
}
Equilibrium <- function(Time,State,Pars) 
{
  with( as.list(c(State,Pars)), {
    dY <- 0 
    dP <- 0
    dI <- 0
    dx <- 0
    return(list(c(dY, dP, dI, dx)))
 })
}

c1 <- 10
c2 <- 0.85
c3 <- 0.02
f <- 4
c <- 2
d <- 1
v <- 2.5
k <- 0.1
mu <- 0.5
wx <- 0.2
wy <- 1-wx
k1star <- 1.5
k2star <- -0.2

epsilon<-c(5, 5, 0, 0.3)

Length <- 5000
Interval <- 0.005
Ybar <- c1/(1-c2-c3*d)
Pbar <- c1*d/(1-c2-c3*d)
Ibar <- 0
xbar <- 0
E <- c(Y=Ybar,P=Pbar,I=Ibar,x=xbar)
start1 <- E+epsilon
start2 <- E-epsilon
time <- seq(0, Length, by=Interval)
pars1 <- c(c1=c1,c2=c2,c3=c3,f=f,c=c,d=d,v=v,k=k,mu=mu,wx=wx,wy=wy,k1star=k1star,k2star=k2star)
out1 <- ode(y = start1, times = time, func = FullSystem, parms = pars1,method="rk4")
out2 <- ode(y = start2, times = time, func = FullSystem, parms = pars1,method="rk4")
outE <- ode(y = E, times = time, func = Equilibrium, parms = pars1,method="rk4")

split.screen(c(2,1))
screen(1)
plot(out1[980000:1000000,c("time","Y")],col="red",type="l",lwd=2,ylab="Production",xlab="Time")
lines(out2[980000:1000000,c("time","Y")],col="blue",lty=1,lwd=2)
lines(outE[980000:1000000,c("time","Y")],col="black",lty=2,lwd=2)
screen(2)
plot(out1[980000:1000000,c("time","P")],col="red",type="l",lwd=2,ylab="Stock Price",xlab="Time")
lines(out2[980000:1000000,c("time","P")],col="blue",lty=1,lwd=2)
lines(outE[980000:1000000,c("time","P")],col="black",lty=2,lwd=2)

#screen(1)
#plot(out1[980000:1000000,c("time","I")],col="red",type="l",lwd=2,ylab="Induced Investment",xlab="Time")
#lines(out2[980000:1000000,c("time","I")],col="blue",lty=1,lwd=2)
#lines(outE[980000:1000000,c("time","I")],col="black",lty=2,lwd=2)
#screen(2)
#plot(out1[980000:1000000,c("time","x")],col="red",type="l",lwd=2,ylab="Sentiment",xlab="Time")
#lines(out2[980000:1000000,c("time","x")],col="blue",lty=1,lwd=2)
#lines(outE[980000:1000000,c("time","x")],col="black",lty=2,lwd=2)
