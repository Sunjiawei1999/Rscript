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
findLF <- function(Pars,xco,yco,zco){
  alpha=beta=gamma=1
  with(as.list(Pars,xco,yco,zco),{
    w1 <- c(-(1-c2)/c3,(alpha*(1-c2)+c*beta)/(c*d*beta),-1/gamma*v*c3)
    w2 <- c(-gamma*(alpha*v-1)/alpha,-gamma*c3/(2*c*d*beta),-1/gamma*v)
    w3 <- c((2*c*beta*(1-c2-c3*d)-gamma*(1-c2)*(alpha*v-1))/(alpha*c3),(gamma*(1-c2)*(2*alpha*v-1)-2*c*beta*(1-c2-c3*d))/(2*c*d*beta),(2*c*beta-gamma*(alpha*v-1))/(alpha*gamma*v*c3))
    re <- xco*w1 + yco*w2 + zco*w3
    L1 <- c(alpha*(c2-1)*re[1]+2*c*d*beta*xco+alpha*gamma*v*(c2-1)*yco,alpha*c3*xco-2*c*beta*re[2]+alpha*gamma*v*c3*zco,alpha*yco+gamma*(alpha*v-1)*re[3])<=0
    L2 <- c(re[1],re[1]*n - xco^2,re[1]*(re[2]*re[3] - zco^2)+re[2]*(re[1]*re[3] - yco^2)+re[3]*(re[1]*re[2] - xco^2))>0
    return(c(L1,L2))
  })  
}
c1 <- 5
c2 <- 0.6
c3 <- 0.03
f <- 1
c <- 1
d <- 1
v <- 1.386262
pars1 <- c(c1=c1,c2=c2,c3=c3,f=f,c=c,d=d,v=v)
result1 <- Eigenvalue(pars1)
result2 <- FindSolution(pars1)
result3 <- findLF(pars1,0,0,1)
print(sprintf("Given that alpha=1, beta=1, gamma=1, c1=5, c2=0.6, c3=0.03, f=1, c=1, d=1, v=1.386262, J1's eigenvalues:%.4f+i(%.4f),%.4f+i(%.4f),%.4f+i(%.4f).",Re(result1[1,1]),Im(result1[1,1]),Re(result1[1,2]),Im(result1[1,2]),Re(result1[1,3]),Im(result1[1,3])))
print(sprintf("Given that alpha=1, beta=1, gamma=1, c1=5, c2=0.6, c3=0.03, f=1, c=1, d=1, v=1.386262, J2's eigenvalues:%.4f+i(%.4f),%.4f+i(%.4f),%.4f+i(%.4f).",Re(result1[2,1]),Im(result1[2,1]),Re(result1[2,2]),Im(result1[2,2]),Re(result1[2,3]),Im(result1[2,3])))
print(sprintf("Given that alpha=1, beta=1, gamma=1, c1=5, c2=0.6, c3=0.03, f=1, c=1, d=1, v=1.386262, J3's eigenvalues:%.4f+i(%.4f),%.4f+i(%.4f),%.4f+i(%.4f).",Re(result1[3,1]),Im(result1[3,1]),Re(result1[3,2]),Im(result1[3,2]),Re(result1[3,3]),Im(result1[3,3])))
print(sprintf("The critical value that satisfies the requirements of Hopf Bifurcation Theorem:%.6f",1+result2))
