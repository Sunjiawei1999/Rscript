library("deSolve")
library("ggplot2")
library("scatterplot3d")

SI <- function(Time,State,Pars) 
{
  with( as.list(c(State,Pars)), {
    I <- Ibar+gamma*k2star*((k1star+k2star)/(k1star*exp(-(v*Y-K))+k2star) - 1)
    C <- c1 + c2*Y + c3*P
    Z <- C + I
    dY <- alpha*(Z-Y) 
    dK <- I
    dP <- (f*(1-x)/2 - c*(1+x)/2)*(d*Y - P)
    dx <- k*(1-x)*exp(mu*wx*x+mu*wy*dY)-k*(1+x)*exp(-mu*wx*x-mu*wy*dY)
    return(list(c(dY, dK, dP, dx)))
  })
}

Equi <- function(Time,State,Pars) 
{
  with( as.list(c(State,Pars)), {
    dY <- 0 
    dK <- 0
    dP <- 0
    dx <- 0
    return(list(c(dY, dK, dP, dx)))
 })
}

Eigenvalue <- function(pars){
  with(as.list(pars),{
    R <- (gamma*k1star+Ibar)*(gamma*k2star-Ibar)/(gamma*(k1star+k2star))
    v1 <- c(alpha*(c2-1+v*R),v*R,(f-c)*d/2,2*k*mu*wy*alpha*(c2-1+v*R))
    v2 <- c(-alpha*R,-R,0,2*k*mu*wy*(-alpha*R))
    v3 <- c(alpha*c3,0,-(f-c)/2,2*k*mu*wy*alpha*c3)
    v4 <- c(0,0,0,2*k*(mu*wx-1))

    J <- matrix(c(v1,v2,v3,v4),nrow=4,ncol=4)
    return(eigen(J)$val)
    })  
}
FindSolution <- function(pars){
  with(as.list(pars),{
    R <- (gamma*k1star+Ibar)*(gamma*k2star-Ibar)/(gamma*(k1star+k2star))
    A <- ((f-c)*(alpha*R)^2)/2
    B <- -0.5*alpha*R*(f-c)*(alpha*(1-c2)+R+(f-c)/2)-alpha*R*((1-c2)+R*(f-c)/2 +alpha*(f-c)*(1-c2-c3*d)/2)
    C <- (alpha*(1-c2)+(f-c)/2)*((1-c2)+R*(f-c)/2+alpha*(f-c)*(1-c2-c3*d)/2)+(1-c2)*R+(R^2)*(f-c)/2
    bound1 <- 1/alpha + (f-c)/(2*alpha*R) + (1-c2)/R
    bound2 <- 1/alpha + 2*(1-c2)/(alpha*R*(f-c)) + (1-c2-c3*d)/R
    if(B^2 - 4*A*C < 0)
    {
      message("不能找到关于v的方程的实数根！")
    } else if(B^2 - 4*A*C >= 0)
    {
      x1 <- (-B-sqrt(B^2-4*A*C))/(2*A)
      x2 <- (-B+sqrt(B^2-4*A*C))/(2*A)
      x <- c(x1,x2)
      benchmark <- (x<min(bound1,bound2)) 
      return(x[benchmark])
    }
  })
}

#注意参数选取只考虑mu*wx<1的情形；
alpha <- 1
gamma <- 1.5
c1 <- 10
c2 <- 0.55
c3 <- 0.03
f <- 4
c <- 2
d <- 1
v1 <- 1
v2 <- 2.5
wx <- 0.2
wy <- 1-wx
mu <- 0.5
k <- 0.1
k1star <- 1.5
k2star <- 0.5

Ibar <- 0.2
pars1 <- c(alpha=alpha,gamma=gamma,c1=c1,c2=c2,c3=c3,f=f,c=c,d=d,v=v1,wx=wx,wy=wy,mu=mu,k=k,k1star=k1star,k2star=k2star,Ibar=Ibar)
pars2 <- c(alpha=alpha,gamma=gamma,c1=c1,c2=c2,c3=c3,f=f,c=c,d=d,v=v2,wx=wx,wy=wy,mu=mu,k=k,k1star=k1star,k2star=k2star,Ibar=Ibar)
Ybar <- c1/(1-c2-c3*d)
g <- (k2star*(gamma*k1star+Ibar))/(k1star*(gamma*k2star-Ibar))
Kbar1 <- (v1*c1)/(1-c2-c3*d) + log(g)
Kbar2 <- (v2*c1)/(1-c2-c3*d) + log(g)
Pbar <- c1*d/(1-c2-c3*d)
xbar <- 0

equi1 <- c(Y=Ybar,K=Kbar1,P=Pbar,x=xbar)
equi2 <- c(Y=Ybar,K=Kbar2,P=Pbar,x=xbar)
deviation <- c(5,0.5,5,0.2)

Length <- 1500
Interval <- 0.005
time <- seq(0,Length,by=Interval)
start1 <- equi+deviation
start2 <- equi-deviation

v1out1 <- ode(y = start1, times = time, func = SI, parms = pars1,method="rk4")
v1out2 <- ode(y = start2, times = time, func = SI, parms = pars1,method="rk4")
v2out1 <- ode(y = start1, times = time, func = SI, parms = pars2,method="rk4")
v2out2 <- ode(y = start2, times = time, func = SI, parms = pars2,method="rk4")

v1outequi <- ode(y = equi1, times = time, func = Equi, parms = pars1,method="rk4")
v2outequi <- ode(y = equi2, times = time, func = Equi, parms = pars2,method="rk4")

#sd <- scatterplot3d(v1out1[,c("Y","P","x")],color="red",type="l",main="S型投资额",xlab="国民收入",ylab="股票价格",zlab="市场情绪",lwd=1,col.axis="darkgrey",col.grid="lightgrey")
#sd$points3d(v1out2[,c("Y","P","x")],col="blue",type="l",lwd=1)
##标记轨道出发点；
#sd$points3d(Ybar+deviation[1],Pbar+deviation[3],xbar+deviation[4],type="p",col="brown",pch=20)
#sd$points3d(Ybar-deviation[1],Pbar-deviation[3],xbar-deviation[4],type="p",col="brown",pch=20)

##标记系统的平衡点;
#sd$points3d(Ybar,Pbar,xbar,type="p",col="black",pch=20)

##Pic2,3,4,5分别是存在周期解的条件下各个分量的动态模拟；
#Pic1 <- ggplot()+geom_path(data= data.frame(Time=v1out1[280000:300000,1],Capital_Stock=v1out1[280000:300000,"K"]),aes(x=Time, y=Capital_Stock),color="red")+geom_path(data=data.frame(Time=v1out2[280000:300000,1],Capital_Stock=v1out2[280000:300000,"K"]),aes(x=Time, y=Capital_Stock),color="blue")+geom_path(data= data.frame(Time=v1outequi[280000:300000,1],Capital_Stock=v1outequi[280000:300000,"K"]),aes(x=Time, y=Capital_Stock),color="black",linetype=2)
Pic2 <- ggplot()+geom_path(data= data.frame(Time=v2out1[280000:300000,1],Capital_Stock=v2out1[280000:300000,"K"]),aes(x=Time, y=Capital_Stock),color="red")+geom_path(data=data.frame(Time=v2out2[280000:300000,1],Capital_Stock=v2out2[280000:300000,"K"]),aes(x=Time, y=Capital_Stock),color="blue")+geom_path(data= data.frame(Time=v2outequi[280000:300000,1],Capital_Stock=v2outequi[280000:300000,"K"]),aes(x=Time, y=Capital_Stock),color="black",linetype=2)
Pic3 <- ggplot()+geom_path(data= data.frame(Time=v2out1[280000:300000,1],Production=v2out1[280000:300000,"Y"]),aes(x=Time, y=Production),color="red")+geom_path(data=data.frame(Time=v2out2[280000:300000,1],Production=v2out2[280000:300000,"Y"]),aes(x=Time, y=Production),color="blue")+geom_path(data= data.frame(Time=v2outequi[280000:300000,1],Production=v2outequi[280000:300000,"Y"]),aes(x=Time, y=Production),color="black",linetype=2)
Pic4 <- ggplot()+geom_path(data= data.frame(Time=v2out1[280000:300000,1],Stock_Price=v2out1[280000:300000,"P"]),aes(x=Time, y=Stock_Price),color="red")+geom_path(data=data.frame(Time=v2out2[280000:300000,1],Stock_Price=v2out2[280000:300000,"P"]),aes(x=Time, y=Stock_Price),color="blue")+geom_path(data= data.frame(Time=v2outequi[280000:300000,1],Stock_Price=v2outequi[280000:300000,"P"]),aes(x=Time, y=Stock_Price),color="black",linetype=2)
Pic5 <- ggplot()+geom_path(data= data.frame(Time=v2out1[280000:300000,1],Sentiment=v2out1[280000:300000,"x"]),aes(x=Time, y=Sentiment),color="red")+geom_path(data=data.frame(Time=v2out2[280000:300000,1],Sentiment=v2out2[280000:300000,"x"]),aes(x=Time, y=Sentiment),color="blue")+geom_path(data= data.frame(Time=v2outequi[280000:300000,1],Sentiment=v2outequi[280000:300000,"x"]),aes(x=Time, y=Sentiment),color="black",linetype=2)


result11 <- Eigenvalue(pars1)
result12 <- Eigenvalue(pars2)
result21 <- FindSolution(pars1)
result22 <- FindSolution(pars2)

