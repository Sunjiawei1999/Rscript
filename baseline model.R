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
}
