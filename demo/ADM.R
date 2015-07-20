#1. Load the data package and the data
library("Isi2015Sae")
data(baseball);
attach(baseball)
#2. Estimate via ADM method
psi=hat.P*(1-hat.P)/nrow(baseball)
library("Rgbp")
g<-gbp(hat.P,sqrt(psi),x1,model="gaussian")
#3. Use estimated values to  plot posterior distribution
#3.1 Print the summary
summary(g)
#3.2 Obtain Shrinkage plot
plot(g)
#3.3 Posterior densities 
#Computation of the posterior distribution of A parameters
sapply(c("Rgbp","abind","surv798B","plyr","lattice","TANOVA"),require, character.only=T)
#we load this library to get trigammaInverse and digamma functions 
library("TANOVA")
alpha=trigammaInverse(g$a.var)
beta=exp(digamma(alpha)-g$a.new)
A.mean<-alpha/beta
A.var<-alpha/(beta^2)
A.sd<-sqrt(A.var)
A.summary<-c(Mean=A.mean,sd=A.sd, qgamma(c(.5,.025,.975),shape=alpha,rate=beta))
names(A.summary)[3:5]<-c("Median","1st Qu.","3rd Qu.")
#Definition of the x sequences for the plots 
x=seq(-1,1,length.out=200)
x1<-g$beta.new[2,1]+x*3*sqrt(g$beta.var[2,2])
x2<-seq(qgamma(.001,alpha,rate=beta),qgamma(.999,alpha,rate=beta),length.out=200)
x3<-g$post.mean[1,1]+x*3*g$post.sd[1,1]
#plot posterior densityes
par(mfrow=c(3,1))
plot(x1,dnorm(x1,mean=g$beta.new[2,1],sd=sqrt(g$beta.var[2,2])),xlab='Posterior density of beta1',ylab='density',type='l',col=couleurdebase,lwd=3)
plot(x2,dgamma(x2,shape=alpha,rate=beta),xlab='Posterior density of A',ylab='density',type='l',col=couleurdebase,lwd=3)
abline(v=A.mean,lwd=3,col='gray')
abline(v=A.summary['Median'],lwd=3,col='darkgray')
par(mfrow=c(2,1))
plot(x3,dnorm(x3,mean=g$post.mean[1,1],sd=g$post.sd[1,1]),xlab='Posterior density of $theta_i$ for Clemente.',ylab='density',type='l',col=couleurdebase,lwd=3)
