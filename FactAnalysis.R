library("simode")

pars <- c('alpha','alphac1','alphac2','alphac3','f','c','d','gamma','gammav')
vars <- c('Y','P','I')
eq_Y <- 'alphac1+alphac2*Y+alphac3*P+alpha*(I-Y)'
eq_P <- 'f*(d*Y-P)^3 + c*(P-d*Y)'
eq_I <- 'gammav*alphac1+gammav*alphac2*Y-gammav*alpha*Y+gammav*alphac3*P+gammav*alpha*I-gamma*I'
equations <- c(eq_Y,eq_P,eq_I)
names(equations)<-vars
theta <- c(1, 5, 0.6, 0.03, 1, 1, 1, 1, 1.4)
names(theta) <- pars

c1 <- theta[2]/theta[1]
c2 <- theta[3]/theta[1]
c3 <- theta[4]/theta[1]
f <- theta[5]
c <- theta[6]
d <- theta[7]

stateSP1 <- c(c1/(1-c2-c3*d),(c1*d)/(1-c2-c3*d),0)
stateSP2 <- c((c1+c3*sqrt(c/f))/(1-c2-c3*d),(c1*d+(1-c2)*sqrt(c/f))/(1-c2-c3*d),0)
stateSP3 <- c((c1-c3*sqrt(c/f))/(1-c2-c3*d),(c1*d-(1-c2)*sqrt(c/f))/(1-c2-c3*d),0)

epsilon <- c(5,3,0.5)
x0 <- stateSP2+epsilon
names(x0) <- vars

n=500
set.seed(1000)
time <- seq(0,50,length.out=n)
model_out <- solve_ode(equations,theta,x0,time)
x_det <- model_out[,vars]
sigma <- 0.05
obs <- list()
for(i in 1:length(vars)){
  obs[[i]] <- x_det[,i] + rnorm(n,0,sigma)
}
names(obs) <- vars

lin_pars <- c('alpha','alphac1','alphac2','alphac3','f','c','gamma')
nlin_pars <- setdiff(pars,lin_pars)

nlin_init <- rnorm(length(theta[nlin_pars]),theta[nlin_pars],0.1*theta[nlin_pars])
names(nlin_init) <- nlin_pars

est_semilin <- simode(equations=equations,pars=pars,fixed=x0,time=time,obs=obs,nlin_pars=nlin_pars,start=nlin_init)
summary(est_semilin)

#plot(est_semilin, type='est', show='both', pars_true=theta, legend=T)
plot(est_semilin, type='fit', pars_true=theta,mfrow=c(2,2),legend=T)
