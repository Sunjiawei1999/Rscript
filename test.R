library("deSolve")
library("ggplot2")
library("scatterplot3d")

ThreeDimModel <- function(Time,State,Pars) 
{
  with( as.list(c(State,Pars)), {
    dY <- alpha*(c1 + (c2 - 1)*Y + c3 * P + I) 
    dP <- beta*(f*(d*Y - P)^3 + c*(P - d*Y))
    dI <- gamma*(alpha*v*c1 + alpha*v*(c2 - 1)*Y + alpha*v*c3*P + (alpha*v - 1)*I) 
    return(list(c(dY, dP, dI)))
  })
}
epsilon1 <- 2
epsilon2 <- 2
epsilon3 <- 0.5
##要求Length >= 50
Length <- 50
#Length <- 100
#Length <- 350
#Length <- 1000
Interval <- 0.01

#设定参数；
alpha=1
beta=1
gamma=1
c1=5
c2=0.6
c3=0.03
f=1
c=1
d=1
v=0.9

epsilon <- c(epsilon1,epsilon2,epsilon3)
time <- seq(0, Length, by = Interval)
stateSP1 <- c(Y=c1/(1-c2-c3*d),P=(c1*d)/(1-c2-c3*d),I=0)
stateSP2 <- c(Y=(c1+c3*sqrt(c/f))/(1-c2-c3*d),P=(c1*d+(1-c2)*sqrt(c/f))/(1-c2-c3*d),I=0)
stateSP3 <- c(Y=(c1-c3*sqrt(c/f))/(1-c2-c3*d),P=(c1*d-(1-c2)*sqrt(c/f))/(1-c2-c3*d),I=0)

state1 <- stateSP2+epsilon
state2 <- stateSP3-epsilon
#state3 <- c(Y=c1/(1-c2-c3*d),P=(c1*d)/(1-c2-c3*d),I=0.01)
#state4 <- c(Y=c1/(1-c2-c3*d),P=(c1*d)/(1-c2-c3*d),I=-0.01)

pars1 <- c(alpha=alpha,beta=beta,gamma=gamma,c1=c1,c2=c2,c3=c3,f=f,c=c,d=d,v=v)
#以stateSP1,stateSP2,stateSP3出发的轨道都应是平衡状态时的常值解；
#使用的数值求解方法设定为古典四阶显式Runge-Kutta方法；
out1 <- ode(y = state1, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
out2 <- ode(y = state2, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
#out3 <- ode(y = state3, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
#out4 <- ode(y = state4, times = time, func = ThreeDimModel, parms = pars1,method="rk4")

#若分别考虑每个内生变量关于时间的变动，则用下述数据绘图；
outSP1 <- ode(y = stateSP1, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
outSP2 <- ode(y = stateSP2, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
outSP3 <- ode(y = stateSP3, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
#下一行作图代码标题中的“v=？？？”在具体作图时记得更改；
plot(out1,out2,outSP1,outSP2,outSP3,col=c("red","blue","black","black","black"),ylab=c("Production","Stock Price","Induced Investment"),xlab="Time",main="Time Series(v=1.387)")


#若考虑相空间中内生变量的联动，则用下行绘图；
#接下来几行想要绘制平衡点附近出现极限环，故参数v取值需要调整到比临界值适当大；（建议取v=1.387）
#split.screen(c(1,2))
#screen(1)
#scatterplot3d(out1[,-1],color="blue",type="l",main="渐近稳定于极限环一",xlab="国民收入",ylab="股票价格",zlab="引致投资",lwd=1,col.axis="darkgrey",col.grid="lightgrey")
#screen(2)
#scatterplot3d(out2[,-1],color="red",type="l",main="渐近稳定于极限环二",xlab="国民收入",ylab="股票价格",zlab="引致投资",lwd=1,col.axis="darkgrey",col.grid="lightgrey")

#接下来几行想要绘制平衡点渐近稳定的三维图形，故运行前参数v的取值不能超过临界值；(不改变其它参数的条件下，建议取v<1.386262)
#split.screen(c(1,2))
#screen(1)
#scatterplot3d(out1[,-1],type="l",main="渐近稳定于SP2",xlab="国民收入",ylab="股票价格",zlab="引致投资",col.axis="darkgrey",col.grid="lightgrey")
#screen(2)
#scatterplot3d(out2[,-1],type="l",main="渐近稳定于SP3",xlab="国民收入",ylab="股票价格",zlab="引致投资",col.axis="darkgrey",col.grid="lightgrey")

#当存在极限环时，绘制两条分别渐近稳定于两个极限环的轨道，并取与极限环充分接近的一部分轨道数据分析值得分布情况；
#split.screen(c(3,2))
#screen(1)
#plot(density(out3[(Length/Interval)-3111:(Length/Interval)+1,2]),main="极限环一附近轨道值分布",xlab="国民收入",ylab="密度",type="l")
#rug(out3[(Length/Interval)-798:(Length/Interval)+1,2])
#screen(2)
#plot(density(out4[(Length/Interval)-3111:(Length/Interval)+1,2]),main="极限环二附近轨道值分布",xlab="国民收入",ylab="密度",type="l")
#rug(out4[(Length/Interval)-798:(Length/Interval)+1,2])
#screen(3)
#plot(density(out3[(Length/Interval)-3111:(Length/Interval)+1,3]),main="极限环一附近轨道值分布",xlab="股票价格",ylab="密度",type="l")
#rug(out3[(Length/Interval)-798:(Length/Interval)+1,3])
#screen(4)
#plot(density(out4[(Length/Interval)-3111:(Length/Interval)+1,3]),main="极限环二附近轨道值分布",xlab="股票价格",ylab="密度",type="l")
#rug(out4[(Length/Interval)-798:(Length/Interval)+1,3])
#screen(5)
#plot(density(out3[(Length/Interval)-3111:(Length/Interval)+1,4]),main="极限环一附近轨道值分布",xlab="引致投资",ylab="密度",type="l")
#rug(out3[(Length/Interval)-798:(Length/Interval)+1,4])
#screen(6)
#plot(density(out4[(Length/Interval)-3111:(Length/Interval)+1,4]),main="极限环二附近轨道值分布",xlab="引致投资",ylab="密度",type="l")
#rug(out4[(Length/Interval)-798:(Length/Interval)+1,4])

#由于发现从state3和state4出发的轨道在经历一段充分长的时间后收敛到同一个极限环，（建议取v=1.4）
#下面的代码旨在将该极限环附近在Y-I平面和Y-P平面的投影绘制出来，正好对应于解释实体经济与股票市场之间的相互作用；
##Y-I平面
#Pic1 <- ggplot()+geom_path(data= data.frame(Production=out3[,2],Stock_Price=out3[,3]),aes(x=Production, y=Stock_Price),color = "red")+geom_path(data= data.frame(Production=out4[,2],Stock_Price=out4[,3]),aes(x=Production, y=Stock_Price),color = "blue")
##Y-P平面
#Pic2 <- ggplot()+geom_path(data= data.frame(Production=out3[,2],Induced_Investment=out3[,4]),aes(x=Production, y=Induced_Investment),color = "red")+geom_path(data= data.frame(Production=out4[,2],Induced_Investment=out4[,4]),aes(x=Production, y=Induced_Investment),color = "blue")
