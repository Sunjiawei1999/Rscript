基础模型R程序脚本：
以下为R脚本文件baseline model.R
alpha <- 0.6
beta <- 0.8
two_d_trajectory_simulation <- function (y0,p0,interval,step,a,m,n,f,c,d)
{
  y <- numeric(step)
  p <- numeric(step)
  y[1] <- y0
  p[1] <- p0
  for(i in 2:step){
    y[i] = y[i-1] + interval*alpha*(a-(1-m)*y[i-1]+n*p[i-1])
    p[i] = p[i-1] + interval*beta*(f*(d*y[i-1]-p[i-1])^3 + c*(p[i-1]-d*y[i-1]))
  }
  return(data.frame(Income=y,Price=p))
}
library("ggplot2")
a <- 5
m <- 0.4
n <- 0.03
f <- 0.6
c <- 0.4
d <- 1
#以上给出了基础模型数值模拟默认的参数取值
Y1 <- a/(1-m-n*d)
P1 <- a*d/(1-m-n*d)
u1 <- c(-2.715578,-1.294247)
u2 <- c(-1.294247,-48.276943)
d1 <- two_d_trajectory_simulation(7.5,7.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d2 <- two_d_trajectory_simulation(6,6.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d5 <- two_d_trajectory_simulation(5.5,9.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d6 <- two_d_trajectory_simulation(12,12,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d7 <- two_d_trajectory_simulation(12,11.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d8 <- two_d_trajectory_simulation(12,11,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d9 <- two_d_trajectory_simulation(12,10.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d11 <- two_d_trajectory_simulation(6,12,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r12 <- two_d_trajectory_simulation(12,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r13 <- two_d_trajectory_simulation(8.5,12,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r14 <- two_d_trajectory_simulation(9,12,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r15 <- two_d_trajectory_simulation(9.5,12,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r16 <- two_d_trajectory_simulation(8,12,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r17 <- two_d_trajectory_simulation(7.5,12,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r18 <- two_d_trajectory_simulation(7,12,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d19 <- two_d_trajectory_simulation(6.5,12,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d22 <- two_d_trajectory_simulation(7  ,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d23 <- two_d_trajectory_simulation(7.5,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d24 <- two_d_trajectory_simulation(8  ,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d25 <- two_d_trajectory_simulation(8.5,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d26 <- two_d_trajectory_simulation(9  ,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d27 <- two_d_trajectory_simulation(9.5,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d28 <- two_d_trajectory_simulation(10 ,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d29 <- two_d_trajectory_simulation(10.5,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r30 <- two_d_trajectory_simulation(11 ,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r31 <- two_d_trajectory_simulation(11.5,5.5,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d32 <- two_d_trajectory_simulation(6,6,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d33 <- two_d_trajectory_simulation(8.772,8.772,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r34 <- two_d_trajectory_simulation(9.2,9,0.01,2000,5,0.4,0.03,0.6,0.4,1)
d35 <- two_d_trajectory_simulation(6,7,0.01,2000,5,0.4,0.03,0.6,0.4,1)
r36 <- two_d_trajectory_simulation(7,9,0.01,2000,5,0.4,0.03,0.6,0.4,1)
(graph <- ggplot() + 
  geom_path(data = d1, aes(Income, Price),color = "black",arrow=arrow(angle=10,type="closed")) + 
  geom_path(data = d2, aes(Income, Price),color = "black",arrow=arrow(angle=10,type="closed")) + 
  geom_path(data = d5, aes(Income, Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d6, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d7, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d8, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d9, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d11, aes(Income, Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r12, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r13, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r14, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r15, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r16, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r17, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r18, aes(Income, Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d19, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d22, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d23, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d24, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d25, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d26, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d27, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d28, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d29, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r30, aes(Income,Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r31, aes(Income,Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d32, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_line(data = d33, aes(Income, Price), size=2.5) +
  geom_path(data = r34, aes(Income,Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = d35, aes(Income,Price),color = "black",arrow=arrow(angle=10,type="closed")) +
  geom_path(data = r36, aes(Income,Price),color = "red",arrow=arrow(angle=10,type="closed")) +
  geom_rect(aes(xmin = 8.78, ymin = 9.6, xmax = 8.84, ymax = 9.66),color = "brown", size =1.5) +
  geom_rect(aes(xmin = 8.699, ymin = 7.8825, xmax = 8.759, ymax = 7.9425),color = "brown", size =1) +
  geom_segment(aes(x = Y1, y = P1, xend = Y1+0.25*u1[1], yend = P1+0.25*u1[2]),alpha=0.4,size=1,color="green")+
  geom_segment(aes(x = Y1, y = P1, xend = Y1-0.25*u1[1], yend = P1-0.25*u1[2]),alpha=0.4,size=1,color="green")+
  geom_segment(aes(x = Y1, y = P1, xend = Y1+0.01*u2[1], yend = P1+0.01*u2[2]),alpha=0.4,size=1,color="brown")+
  geom_segment(aes(x = Y1, y = P1, xend = Y1-0.01*u2[1], yend = P1-0.01*u2[2]),alpha=0.4,size=1,color="brown") )
模型延伸一R程序脚本：
以下为R语言脚本文件test.R
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
pars1 <- c(alpha=1,beta=1,gamma=1,c1=5,c2=0.6,c3=0.03,f=1,c=1,d=1,v=1.46)
epsilon1 <- 0.3
epsilon2 <- 0.3
epsilon3 <- 0.5
##要求Length >= 50
#Length <- 50
#Length <- 100
#Length <- 350
Length <- 1000
#Length <- 2000
#Length <- 4000
Interval <- 0.01

time <- seq(0, Length, by = Interval)
state1 <- c(Y=(c1+c3*sqrt(c/f)-epsilon1)/(1-c2-c3*d),P=(c1*d+(1-c2)*sqrt(c/f)-epsilon2)/(1-c2-c3*d),I=-epsilon3)
state2 <- c(Y=(c1-c3*sqrt(c/f)+epsilon1)/(1-c2-c3*d),P=(c1*d-(1-c2)*sqrt(c/f)+epsilon2)/(1-c2-c3*d),I=epsilon3)
state3 <- c(Y=c1/(1-c2-c3*d),P=(c1*d)/(1-c2-c3*d),I=0.01)
state4 <- c(Y=c1/(1-c2-c3*d),P=(c1*d)/(1-c2-c3*d),I=-0.01)
stateSP1 <- c(Y=c1/(1-c2-c3*d),P=(c1*d)/(1-c2-c3*d),I=0)
stateSP2 <- c(Y=(c1+c3*sqrt(c/f))/(1-c2-c3*d),P=(c1*d+(1-c2)*sqrt(c/f))/(1-c2-c3*d),I=0)
stateSP3 <- c(Y=(c1-c3*sqrt(c/f))/(1-c2-c3*d),P=(c1*d-(1-c2)*sqrt(c/f))/(1-c2-c3*d),I=0)

#以stateSP1,stateSP2,stateSP3出发的轨道都应是平衡状态时的常值解；
#使用的数值求解方法设定为古典四阶显式Runge-Kutta方法；
#out1 <- ode(y = state1, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
#out2 <- ode(y = state2, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
out3 <- ode(y = state3, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
out4 <- ode(y = state4, times = time, func = ThreeDimModel, parms = pars1,method="rk4")


#在出现渐近稳定的极限环时，希望给出极限环内更靠近平衡点SP2,SP3的出发的轨道；
#epsilon <- 0.005
#nearSP2 <- c(Y=((c1+c3*sqrt(c/f))/(1-c2-c3*d))+epsilon,P=((c1*d+(1-c2)*sqrt(c/f))/(1-c2-c3*d))+epsilon,I=epsilon)
#nearSP3 <- c(Y=((c1-c3*sqrt(c/f))/(1-c2-c3*d))+epsilon,P=((c1*d-(1-c2)*sqrt(c/f))/(1-c2-c3*d))+epsilon,I=epsilon)
#time2 <- seq(0, 200, by = 0.001)
#outnearSP2 <- ode(y = nearSP2, times = time2, func = ThreeDimModel, parms = pars1,method="rk4")
#outnearSP3 <- ode(y = nearSP3, times = time2, func = ThreeDimModel, parms = pars1,method="rk4")

#若分别考虑每个内生变量关于时间的变动，则用下述数据绘图；
#outSP1 <- ode(y = stateSP1, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
#outSP2 <- ode(y = stateSP2, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
#outSP3 <- ode(y = stateSP3, times = time, func = ThreeDimModel, parms = pars1,method="rk4")
#plot(out1,out2,outSP1,outSP2,outSP3,col=c("red", "blue","black"),ylab=c("Production","Stock Price","Induced Investment"),xlab="Time",main="Time Series(v=1.387)")

#若考虑相空间中内生变量的联动，则用下行绘图；
#接下来几行想要绘制平衡点附近出现极限环，故参数v取值需要调整到比临界值适当大；（建议取v=1.387）
#split.screen(c(1,2))
#screen(1)
#scatterplot3d(out1[,-1],color="blue",type="l",main="渐近稳定于极限环一",xlab="国民收入",ylab="股票价格",zlab="引致投资",lwd=1,col.axis="darkgrey",col.grid="lightgrey")
scatterplot3d(out3[,-1],color="blue",type="l",xlab="国民收入",ylab="股票价格",zlab="引致投资",lwd=1,col.axis="darkgrey",col.grid="lightgrey")
#screen(2)
#scatterplot3d(out2[,-1],color="red",type="l",main="渐近稳定于极限环二",xlab="国民收入",ylab="股票价格",zlab="引致投资",lwd=1,col.axis="darkgrey",col.grid="lightgrey")
#scatterplot3d(out4[,-1],color="red",type="l",xlab="国民收入",ylab="股票价格",zlab="引致投资",lwd=1,col.axis="darkgrey",col.grid="lightgrey")

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
Pic1<-ggplot()+geom_path(data=data.frame(Production=out3[,2],Stock_Price=out3[,3]),aes(x=Production, y=Stock_Price),color = "red")
+  geom_path(data=data.frame(Production=out4[,2],Stock_Price=out4[,3]),aes(x=Production, y=Stock_Price),color = "blue")
##Y-P平面
Pic2<-ggplot()+geom_path(data=data.frame(Production=out3[,2],Induced_Investment=out3[,4]),aes(x=Production, y=Induced_Investment),color = "red")
+  geom_path(data=data.frame(Production=out4[,2],Induced_Investment=out4[,4]),aes(x=Production, y=Induced_Investment),color = "blue")


以下为R语言脚本文件testHelper.R
#默认模型中几个参数alpha=1,beta=1,gamma=1
Eigenvalue <- function(pars){
  with(as.list(pars),{
    v1 <- c(c2-1,-c*d,v*(c2-1),c3,c,v*c3,1,0,v-1)
    v2 <- c(c2-1,2*c*d,v*(c2-1),c3,-2*c,v*c3,1,0,v-1)
    v3 <- c(c2-1,2*c*d,v*(c2-1),c3,-2*c,v*c3,1,0,v-1)

    J1 <- matrix(v1,nrow=3,ncol=3)
    J2 <- matrix(v2,nrow=3,ncol=3)
    J3 <- matrix(v3,nrow=3,ncol=3)
    return(rbind(eigen(J1)$val,eigen(J2)$val,eigen(J3)$val))
    })  
}
FindSolution <- function(pars){
  with(as.list(pars),{
    A <- 2*c
    B <- -2*c*(1-c2)-2*c*(1-c2-c3*d)-4*(c^2)-(1-c2)
    C <- (1-c2)^2 + 2*c*c3*d + 2*c*(1-c2-c3*d)*(1-c2 + 2*c)
    bound1 <- 2*c + (1-c2)
    bound2 <- (1-c2)/(2*c) + (1-c2-c3*d)
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
c1 <- 5
c2 <- 0.6
c3 <- 0.03
f <- 1
c <- 1
d <- 1
v <- 1.5
pars1 <- c(c1=c1,c2=c2,c3=c3,f=f,c=c,d=d,v=v)
result1 <- Eigenvalue(pars1)
result2 <- FindSolution(pars1)
print(sprintf("Given that alpha=1, beta=1, gamma=1, c1=5, c2=0.6, c3=0.03, f=1, c=1, d=1, v=1.386262, J1's eigenvalues:%.4f+i(%.4f),%.4f+i(%.4f),%.4f+i(%.4f).",Re(result1[1,1]),Im(result1[1,1]),Re(result1[1,2]),Im(result1[1,2]),Re(result1[1,3]),Im(result1[1,3])))
print(sprintf("Given that alpha=1, beta=1, gamma=1, c1=5, c2=0.6, c3=0.03, f=1, c=1, d=1, v=1.386262, J2's eigenvalues:%.4f+i(%.4f),%.4f+i(%.4f),%.4f+i(%.4f).",Re(result1[2,1]),Im(result1[2,1]),Re(result1[2,2]),Im(result1[2,2]),Re(result1[2,3]),Im(result1[2,3])))
print(sprintf("Given that alpha=1, beta=1, gamma=1, c1=5, c2=0.6, c3=0.03, f=1, c=1, d=1, v=1.386262, J3's eigenvalues:%.4f+i(%.4f),%.4f+i(%.4f),%.4f+i(%.4f).",Re(result1[3,1]),Im(result1[3,1]),Re(result1[3,2]),Im(result1[3,2]),Re(result1[3,3]),Im(result1[3,3])))
print(sprintf("The critical value that satisfies the requirements of Hopf Bifurcation Theorem:%.6f",1+result2))

模型延伸二R程序脚本
以下为R脚本文件test2.R
library("deSolve")
library("ggplot2")
library("scatterplot3d")

c1 <- 50
c2 <- 0.5
c3 <- 0.03 
f <- 4 
c <- 2 
d <- 1 
Length = 1000
Interval <- 0.005
time <- seq(0, Length, by = Interval)
Ybar <- c1/(1-c2-c3*d)
Pbar <- (c1*d)/(1-c2-c3*d)
Ibar <- 0
xbar <- 0

##第一部分的代码适用于对投资金融子系统进行数值模拟和相关计算；若要运行请将第二部分代码注释掉；
if(FALSE){
InvestmentFinSubsystem <- function(Time,State,Pars) 
{
  with( as.list(c(State,Pars)), {
    dY <- c1 + (c2 - 1)*Y + c3 * P + I 
    dP <- ((f-c)/2) *(d*Y - P)
    if(v*dY < k1star & v*dY > k2star){
      dI <- v*c1 + v*(c2 - 1)*Y + v*c3*P + (v - 1)*I }
    if(v*dY > k1star){dI <- k1star - I}
    if(v*dY < k2star){dI <- k2star - I}
    return(list(c(dY, dP, dI)))
 })
}
FindSolution2 <- function(pars){
  with(as.list(pars),{
    A <- (f-c)/2
    B <- -(f-c)*(1-c2-c3*d)/2 - (f-c)*(1-c2+(f-c)/2)/2
    C <- (1-c2)*(1-c2+(f-c)/2)+(f-c)*(1-c2+c3*d*v)/2
    bound1 <- (1-c2)+(f-c)/2
    bound2 <- (1-c2-c3*d)+2*(1-c2)/(f-c)
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
v <- 1
k1star <- 1
k2star <- -1

epsilon11 <- 2
epsilon21 <- 0
epsilon31 <- 0
init11 <- c(Y=Ybar+epsilon11,P=Pbar+epsilon21,I=Ibar+epsilon31)
init21 <- c(Y=Ybar-epsilon11,P=Pbar-epsilon21,I=Ibar-epsilon31)
pars11 <- c(c1=c1,c2=c2,c3=c3,f=f,c=c,d=d,v=v,k1star=k1star,k2star=k2star)
out11 <- ode(y = init11, times=time, func=InvestmentFinSubsystem, parms=pars11, method="rk4")
out21 <- ode(y = init21, times=time, func=InvestmentFinSubsystem, parms=pars11, method="rk4")
print(sprintf("Our core investment-finacial subsystem's critical v is %.6f",1+FindSolution2(pars1)))

logicVec1 <- (v*(out11[,2]-c(0,out11[-(Length/Interval + 1),2]))/Interval >= k2star)&(v*(out11[,2]-c(0,out11[-(Length/Interval + 1),2]))/Interval <= k1star)
logicVec1[1] <- (v*(c1 + (c2 -1)*(Ybar+epsilon1) + c3*(Pbar+epsilon2) +(Ibar+epsilon3)) <= k1star)&(v*(c1 + (c2 -1)*(Ybar+epsilon1) + c3*(Pbar+epsilon2) +(Ibar+epsilon3)) >= k2star)
logicVec2 <- (v*(out21[,2]-c(0,out21[-(Length/Interval + 1),2]))/Interval >= k2star)&(v*(out21[,2]-c(0,out21[-(Length/Interval + 1),2]))/Interval <= k1star)
logicVec2[1] <- (v*(c1 + (c2 -1)*(Ybar-epsilon1) + c3*(Pbar-epsilon2) +(Ibar-epsilon3)) <= k1star)&(v*(c1 + (c2 -1)*(Ybar-epsilon1) + c3*(Pbar-epsilon2) +(Ibar-epsilon3)) >= k2star)
gap1 <- matrix(c(NA,NA,NA,NA),nrow=1,ncol=4)
out1 <- rbind(out11,gap1,out21)
if(all(c(logicVec1,logicVec2)) == TRUE){
  p <- scatterplot3d(out1[,-1],type="l",main="渐近稳定的投资金融子系统",xlab="国民收入",ylab="股票价格",zlab="引致投资",lwd=1,col.axis="darkgrey",col.grid="lightgrey")
  p$points3d(c1/(1-c2-c3*d),c1*d/(1-c2-c3*d),0,type="p",col="blue",pch=20)
}else
  {print("Attention! During the numerical computation, the investment accelerator has lost its flexibility!")
  split.screen(c(1,2))
  screen(1)
  p1 <- scatterplot3d(out11[,-1],type="l",color="red",main="投资变动达到弹性界外的投资金融子系统",xlab="国民收入",ylab="股票价格",zlab="引致投资",lwd=1,col.axis="darkgrey",col.grid="lightgrey")
  p1$points3d(c1/(1-c2-c3*d),c1*d/(1-c2-c3*d),0,type="p",col="black",pch=20)
  screen(2)
  p2 <- scatterplot3d(out21[,-1],type="l",color="blue",main="投资变动达到弹性界外的投资金融子系统",xlab="国民收入",ylab="股票价格",zlab="引致投资",lwd=1,col.axis="darkgrey",col.grid="lightgrey")
  p2$points3d(c1/(1-c2-c3*d),c1*d/(1-c2-c3*d),0,type="p",col="black",pch=20)
  }
}

##第二部分代码适用于市场情绪金融子系统的数值模拟；其中已经将羊群效应项融合到模型中，若不考虑羊群效应项，则将wx取为0，wy取为1；
##若要运行请将第一部分代码注释掉；

SentimentFinSubsystem <- function(Time,State,Pars) 
{
  with( as.list(c(State,Pars)), {
    dY <- c1 + (c2 - 1)*Y + c3 * P
    dP <- ((1-x)*f/2 - (1+x)*c/2)*(d*Y - P)
    dx <- k*(1-x)*exp(mu*wy*(c1 + (c2 - 1)*Y + c3 * P)+mu*wx*x) - k*(1+x)*exp(-mu*wy*(c1 + (c2 - 1)*Y + c3 * P)-mu*wx*x)
    return(list(c(dY, dP, dx)))
 })
}

##由于在论文中已经针对函数F进行了相关讨论得知其必然存在除零以外的两个零点，故使用二分法和牛顿迭代法找根；
##若进行数值模拟时选取的参数mu和wx乘积不大于1，则不妨将函数F，Fprime和Dichotomy注释掉；
F <- function(x,mu,wx)
{
  result <- (1-x)-(1+x)*exp(-2*mu*wx*x)
}
Fprime <- function(x,mu,wx)
{
  result <- -1-exp(-2*mu*wx*x)+2*mu*wx*(1+x)*exp(-2*mu*wx*x)
}

#该函数旨在使用二分法和牛顿迭代法找根；
Dichotomy <- function(mu,wx,tol)
{
  with(as.list(c(mu,wx)),{
    l1 <- -1
    l2 <- 1/(mu*wx) - 1
    r <- 1
    error <- 1
    while(error>=tol){
      if(F((l1+l2)/2,mu,wx)>=0){l1 <- (l1+l2)/2}
      if(F((l1+l2)/2,mu,wx)<0){l2 <- (l1+l2)/2}
      temp <- F(r,mu,wx)/Fprime(r,mu,wx)
      r <- r-temp
      error <- max(l2-l1,abs(temp))
    }
    return(c((l1+l2)/2,r))
  })
}
JacobiComputation <- function(x,Pars)
{
  with(as.list(c(x,Pars)),{
    v1 <- c(c2-1,c3,0)
    v2 <- c((f*(1-x)/2 - c*(1+x)/2)*d,-(f*(1-x)/2-c*(1+x)/2),0)
    J31 <- k*mu*wy*(c2-1)*((1-x)*exp(mu*wx*x)+(1+x)*exp(-mu*wx*x))
    J32 <- k*mu*wy*c3*((1-x)*exp(mu*wx*x)+(1+x)*exp(-mu*wx*x))
    J33 <- k*mu*wx*((1-x)*exp(mu*wx*x)+(1+x)*exp(-mu*wx*x))-k*(exp(mu*wx*x)+exp(-mu*wx*x))
    v3 <- c(J31,J32,J33)
    J <- matrix(c(v1,v2,v3),nrow=3,byrow=TRUE)
    re <- eigen(J)$val
    return(re)
  })
}
tol <- 1.0e-8
k <- 0.1
mu <- 1.5
wx <- 0.7
wy <- 1-wx
epsilon12 <- 5
epsilon22 <- 0
epsilon32 <- 0
init12 <- c(Y=Ybar+epsilon12,P=Pbar+epsilon22,x=xbar+epsilon32)
init22 <- c(Y=Ybar-epsilon12,P=Pbar-epsilon22,x=xbar-epsilon32)

#组成参数向量并进行轨道模拟；
pars12 <- c(c1=c1,c2=c2,c3=c3,f=f,c=c,d=d,k=k,mu=mu,wx=wx,wy=wy)
out12 <- ode(y=init12, times=time, func=SentimentFinSubsystem, parms=pars12, method="rk4")
out22 <- ode(y=init22, times=time, func=SentimentFinSubsystem, parms=pars12, method="rk4")

#在交互界面给出满足mu*wx>1参数条件下系统的新增平衡点x的值;
#对几个平衡点处的jacobi矩阵特征值进行计算；想要知道具体值则在编译完后输入value1,value2,value3；
value3 <- JacobiComputation(xbar,pars12)
if(mu*wx > 1){
  roots <- Dichotomy(mu,wx,tol)
  print(sprintf("We find the roots:%.7f and %.7f",roots[1],roots[2]))
  value1 <- JacobiComputation(roots[1],pars12)
  value2 <- JacobiComputation(roots[2],pars12)
}
#对命题中给出的平衡点局部稳定性条件进行判断，并在交互界面反馈信息；
if(mu*wx <= 1){
  message("The sentiment-financial subsystem has the only one equilibrium E1 which is locally asymptotically stable.")
  ch <- "Asympototically Stable Sentiment-Financial Subsystem"
}
if(mu*wx > 1){
  message("The sentiment-financial subsystem has three equilibrium points E1, E2 and E3 among which E1 is unstable.")
  ch <- "Sentiment-Financial Subsystem with New Equilibrium"
}


#下面绘制三维空间的动态变化
gap2 <- matrix(c(NA,NA,NA,NA),nrow=1,ncol=4)
out2 <- rbind(out12,gap2,out22)
sd <- scatterplot3d(out2[,-1],color=rep("red",length(out2[,2])),type="l",main=ch,xlab="国民收入",ylab="股票价格",zlab="市场情绪",lwd=1,col.axis="darkgrey",col.grid="lightgrey")

#标记轨道出发点
sd$points3d(Ybar+epsilon12,Pbar+epsilon22,xbar+epsilon32,type="p",col="brown",pch=20)
sd$points3d(Ybar-epsilon12,Pbar-epsilon22,xbar-epsilon32,type="p",col="brown",pch=20)

#标记子系统的平衡点，由上至下分别为E1,E2,E3;
if(mu*wx > 1){
  sd$points3d(Ybar,Pbar,roots[1],type="p",col="black",pch=20)
  sd$points3d(Ybar,Pbar,roots[2],type="p",col="black",pch=20)
}
sd$points3d(Ybar,Pbar,xbar,type="p",col="black",pch=20)

#下面考虑绘制轨道的投影变化；
new_out2 <- cbind( rbind(out12,out22),Trajectory=c(rep(1,length(out12[,2])),rep(2,length(out22[,2]))) )
new_out2 <- as.data.frame(new_out2)
twod1 <- ggplot(data=new_out2,mapping=aes(x=Y,y=P,group=Trajectory))+geom_path(aes(color=Trajectory),lwd=1)+
  geom_point(aes(x=Ybar, y=Pbar),color="black",size=2)+
  geom_point(aes(x=Ybar+epsilon12, y=Pbar+epsilon22),color="brown",size=2)+
  geom_point(aes(x=Ybar-epsilon12, y=Pbar-epsilon22),color="brown",size=2)
twod2 <- ggplot(data=new_out2,mapping=aes(x=Y,y=x,group=Trajectory))+geom_path(aes(color=Trajectory),lwd=1)+
  geom_point(aes(x=Ybar, y=xbar),color="black",size=2)+
  geom_point(aes(x=Ybar+epsilon12, y=xbar+epsilon32),color="brown",size=2)+
  geom_point(aes(x=Ybar-epsilon12, y=xbar-epsilon32),color="brown",size=2)
twod3 <- ggplot(data=new_out2,mapping=aes(x=P,y=x,group=Trajectory))+geom_path(aes(color=Trajectory),lwd=1)+
  geom_point(aes(x=Pbar, y=xbar),color="black",size=2)+
  geom_point(aes(x=Pbar+epsilon22, y=xbar+epsilon32),color="brown",size=2)+
  geom_point(aes(x=Pbar-epsilon22, y=xbar-epsilon32),color="brown",size=2)
if(mu*wx > 1){
twod1 <- twod1 + geom_point(aes(x=Ybar, y=Pbar),color="black",size=2) + geom_point(aes(x=Ybar, y=Pbar),color="black",size=2)
twod2 <- twod2 + geom_point(aes(x=Ybar, y=roots[1]),color="black",size=2) + geom_point(aes(x=Ybar, y=roots[2]),color="black",size=2)
twod3 <- twod3 + geom_point(aes(x=Pbar, y=roots[1]),color="black",size=2) + geom_point(aes(x=Ybar, y=roots[2]),color="black",size=2)
}
