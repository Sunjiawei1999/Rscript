library("deSolve")
library("ggplot2")
library("scatterplot3d")

#旨在描述模型延伸二中考虑了投资支出和投资延迟后的整个系统的动态变化，以各个内生变量关于时间的变动规律作图；
TimeLagSystem <- function(Time,State,Pars) 
{
  with( as.list(c(State,Pars)), {
    dY <- V 
    dP <- (f*(1-x)/2 - c*(1+x)/2)*(d*Y - P)
    if((v*V)>k1star)
    {
      phi <- k1star
    }else if((v*V)<k2star)
    {
      phi <- k2star
    }else if(((v*V)<=k1star) & ((v*V)>=k2star))
    {
      phi <- v*V
    }
    dV <- (-epsilon*V-(1-c2)*theta*V+phi+c3*P+c3*theta*dP-(1-c2)*Y+(c1+Ia))/(epsilon*theta)
    dx <- k*(1-x)*exp(mu*wx*x+mu*wy*V)-k*(1+x)*exp(-mu*wx*x-mu*wy*V)
    return(list(c(dY, dV, dP, dx)))
  })
}
Equi <- function(Time,State,Pars) 
{
  with( as.list(c(State,Pars)), {
    dY <- 0 
    dV <- 0
    dP <- 0
    dx <- 0
    return(list(c(dY, dV, dP, dx)))
 })
}

#注意参数选取只考虑mu*wx<1的情形；
c1 <- 10
c2 <- 0.55
c3 <- 0.03
f <- 4
c <- 2
d <- 1
v <- 2
wx <- 0.2
wy <- 1-wx
mu <- 0.4
k <- 0.1
k1star <- 1.5
k2star <- -0.2

Ia <- 4
theta <- 0.3
epsilon <- 0.05
pars1 <- c(c1=c1,c2=c2,c3=c3,f=f,c=c,d=d,v=v,wx=wx,wy=wy,mu=mu,k=k,k1star=k1star,k2star=k2star,Ia=Ia,theta=theta,epsilon=epsilon)

#此处设置一个信号变量，值仅可选择为212或234或3；如果值设置为212则是反映Y,V分别关于时间的变动；
#如果设置为234则是反映P,x分别关于时间的变动；如果设置为3则是反映Y,P,x的内生性联动；
Flag <- 3
if(Flag!=212 & Flag!=234 & Flag!=3){message("The signal variable is set wrong!")}

Ybar <- (c1+Ia)/(1-c2-c3*d)
Vbar <- 0
Pbar <- (c1+Ia)*d/(1-c2-c3*d)
xbar <- 0
equi <- c(Y=Ybar,V=Vbar,P=Pbar,x=xbar)
deviation <- c(5,0.5,5,0.2)

start1 <- equi+deviation
start2 <- equi-deviation

Length <- 5000
Interval <- 0.01
time <- seq(0,Length,by=Interval)

out1 <- ode(y = start1, times = time, func = TimeLagSystem, parms = pars1,method="rk4")
out2 <- ode(y = start2, times = time, func = TimeLagSystem, parms = pars1,method="rk4")
outequi <- ode(y = equi, times = time, func = Equi, parms = pars1,method="rk4")

if(Flag==212){
  split.screen(c(2,1))
  screen(1)
  plot(out1[498000:500000,c("time","Y")],col="red",type="l",lwd=2,ylab="Production",xlab="Time")
  lines(out2[498000:500000,c("time","Y")],col="blue",lty=1,lwd=2)
  lines(outequi[498000:500000,c("time","Y")],col="black",lty=2,lwd=2)
  screen(2)
  plot(out1[498000:500000,"time"],Ia+v*out1[498000:500000,"V"],col="red",type="l",lwd=2,ylab="Net Investment",xlab="Time")
  lines(out2[498000:500000,"time"],Ia+v*out2[498000:500000,"V"],col="blue",lty=1,lwd=2)
  lines(outequi[498000:500000,"time"],Ia+v*outequi[498000:500000,"V"],col="black",lty=2,lwd=2)
  }
if(Flag==234){
  split.screen(c(2,1))
  screen(1)
  plot(out1[498000:500000,c("time","P")],col="red",type="l",lwd=2,ylab="Stock Price",xlab="Time")
  lines(out2[498000:500000,c("time","P")],col="blue",lty=1,lwd=2)
  lines(outequi[498000:500000,c("time","P")],col="black",lty=2,lwd=2)
  screen(2)
  plot(out1[498000:500000,c("time","x")],col="red",type="l",lwd=2,ylab="Sentiment",xlab="Time")
  lines(out2[498000:500000,c("time","x")],col="blue",lty=1,lwd=2)
  lines(outequi[498000:500000,c("time","x")],col="black",lty=2,lwd=2)
  }
if(Flag==3){
  sd <- scatterplot3d(out1[,c("Y","P","x")],color="red",type="l",main="存在投资延迟的系统",xlab="国民收入",ylab="股票价格",zlab="市场情绪",lwd=1,col.axis="darkgrey",col.grid="lightgrey")
  sd$points3d(out2[,c("Y","P","x")],col="blue",type="l",lwd=1)
  #标记轨道出发点；
  sd$points3d(Ybar+deviation[1],Pbar+deviation[3],xbar+deviation[4],type="p",col="brown",pch=20)
  sd$points3d(Ybar-deviation[1],Pbar-deviation[3],xbar-deviation[4],type="p",col="brown",pch=20)

  #标记系统的平衡点;
  sd$points3d(Ybar,Pbar,xbar,type="p",col="black",pch=20)
  
  #下面的代码旨在将Y-P平面、Y-x平面和P-x平面的投影绘制出来，解释实体经济与股票市场之间的相互作用；
  message("You can input 'Pic1','Pic2' or 'Pic3' to get projection!")
  ##out1 Y-P平面
  Pic1 <- ggplot()+geom_path(data= data.frame(Production=out1[,"Y"],Stock_Price=out1[,"P"]),aes(x=Production, y=Stock_Price),color = "red")+geom_point(aes(x=Ybar+deviation[1],y=Pbar+deviation[3]),color="brown",size=2)+geom_point(aes(x=Ybar,y=Pbar),color="black",size=2)+geom_path(data=data.frame(Production=out2[,"Y"],Stock_Price=out2[,"P"]),aes(x=Production, y=Stock_Price),color="blue")+geom_point(aes(x=Ybar-deviation[1],y=Pbar-deviation[3]),color="brown",size=2)
  ##out1 Y-x平面
  Pic2 <- ggplot()+geom_path(data= data.frame(Production=out1[,"Y"],Sentiment=out1[,"x"]),aes(x=Production, y=Sentiment),color="red")+geom_point(aes(x=Ybar+deviation[1],y=xbar+deviation[4]),color="brown",size=2)+geom_point(aes(x=Ybar,y=xbar),color="black",size=2)+geom_path(data=data.frame(Production=out2[,"Y"],Sentiment=out2[,"x"]),aes(x=Production, y=Sentiment),color="blue")+geom_point(aes(x=Ybar-deviation[1],y=xbar-deviation[4]),color="brown",size=2)
  ##out1 P-x平面
  Pic3 <- ggplot()+geom_path(data= data.frame(Stock_Price=out1[,"P"],Sentiment=out1[,"x"]),aes(x=Stock_Price, y=Sentiment),color="red")+geom_point(aes(x=Pbar+deviation[3],y=xbar+deviation[4]),color="brown",size=2)+geom_point(aes(x=Pbar,y=xbar),color="black",size=2)+geom_path(data=data.frame(Stock_Price=out2[,"P"],Sentiment=out2[,"x"]),aes(x=Stock_Price, y=Sentiment),color="blue")+geom_point(aes(x=Pbar-deviation[3],y=xbar-deviation[4]),color="brown",size=2)
  }



