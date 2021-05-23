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
    dY <- c1 + (c2 - 1)*Y + c3*P + I 
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
