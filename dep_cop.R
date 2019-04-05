library(copula)
library(psych)

set.seed(100)
myCop <- normalCopula(param=c(0.4,0.2,-0.8), dim = 3, dispstr = "un")
myMvd <- mvdc(copula=myCop, margins=c("gamma", "beta", "t"),
              paramMargins=list(list(shape=2, scale=1),
                                list(shape1=2, shape2=2), 
                                list(df=5)) )

Z2 <- rMvdc(2000, myMvd)
colnames(Z2) <- c("x1", "x2", "x3")
pairs.panels(Z2)

cree <- read.csv('cree_r.csv',header=F)$V2
yahoo <- read.csv('yahoo_r.csv',header=F)$V2

plot(cree,yahoo,pch='.')
abline(lm(yahoo~cree),col='red',lwd=1)
cor(cree,yahoo,method='spearman')

library(VineCopula)
u <- pobs(as.matrix(cbind(cree,yahoo)))[,1]
v <- pobs(as.matrix(cbind(cree,yahoo)))[,2]
selectedCopula <- BiCopSelect(u,v,familyset=NA)
selectedCopula

t.cop <- tCopula(dim=2)
set.seed(500)
m <- pobs(as.matrix(cbind(cree,yahoo)))
fit <- fitCopula(t.cop,m,method='ml')
coef(fit)

rho <- coef(fit)[1]
df <- coef(fit)[2]
persp(tCopula(dim=2,rho,df=df),dCopula)

u <- rCopula(3965,tCopula(dim=2,rho,df=df))
plot(u[,1],u[,2],pch='.',col='blue')
cor(u,method='spearman')

cree_mu <- mean(cree)
cree_sd <- sd(cree)
yahoo_mu <- mean(yahoo)
yahoo_sd <- sd(yahoo)

hist(cree,breaks=80,main='Cree returns',freq=F,density=30,col='cyan',ylim=c(0,20),xlim=c(-0.2,0.3))
lines(seq(-0.5,0.5,0.01),dnorm(seq(-0.5,0.5,0.01),cree_mu,cree_sd),col='red',lwd=2)
legend('topright',c('Fitted normal'),col=c('red'),lwd=2)
hist(yahoo,breaks=80,main='Yahoo returns',density=30,col='cyan',freq=F,ylim=c(0,20),xlim=c(-0.2,0.2))
lines(seq(-0.5,0.5,0.01),dnorm(seq(-0.5,0.5,0.01),yahoo_mu,yahoo_sd),col='red',lwd=2)
legend('topright',c('Fitted normal'),col=c('red'),lwd=2)

copula_dist <- mvdc(copula=tCopula(rho,dim=2,df=df), margins=c("norm","norm"),
                    paramMargins=list(list(mean=cree_mu, sd=cree_sd),
                                      list(mean=yahoo_mu, sd=yahoo_sd)))
sim <- rMvdc(3965, copula_dist)

plot(cree,yahoo,main='Returns')
points(sim[,1],sim[,2],col='red')
legend('bottomright',c('Observed','Simulated'),col=c('black','red'),pch=21)
