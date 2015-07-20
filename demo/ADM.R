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

<<echo=FALSE,results=hide>>=
  graph1<-
  graphtikzcode(
    "
    par(mfrow=c(2,1))
    plot(x1,dnorm(x1,mean=g$beta.new[2,1],sd=sqrt(g$beta.var[2,2])),xlab='Posterior density of $\\\\beta_1$',ylab='density',type='l',col=couleurdebase,lwd=3)
    plot(x2,dgamma(x2,shape=alpha,rate=beta),xlab='Posterior density of $A$',ylab='density',type='l',col=couleurdebase,lwd=3)
    abline(v=A.mean,lwd=3,col='gray')
    abline(v=A.summary['Median'],lwd=3,col='darkgray')"
  )

graph2<-
  graphtikzcode(
    "
    par(mfrow=c(2,1))
    plot(x3,dnorm(x3,mean=g$post.mean[selection[1],1],sd=g$post.sd[selection[1],1]),xlab='Posterior density of $\\\\theta_i$ for smallest s.d.',ylab='density',type='l',col=couleurdebase,lwd=3)  
    plot(x4,dnorm(x4,mean=g$post.mean[selection[10],1],sd=g$post.sd[selection[10],1]),xlab='Posterior density of $\\\\theta_i$ for largest s.d.',ylab='density',type='l',col=couleurdebase,lwd=3)
    "
  )



@

<<R2,results=hide,echo=FALSE>>=
  table1<-data.frame(Parameter=c("$A$","$\\beta_1$"),
                     rbind(A.summary[],
                           c(sqrt(g$beta.var[2,2]),qnorm(c(.5,.5,.025,.975),g$beta.new[2,1],sqrt(g$beta.var[2,2])))[c(2,1,3:5)]))


table2<-data.frame(Parameter=paste0("\\theta_{",selection,"}"),
                   signif(cbind("Mean"=g$post.mean,
                                "sd"=sqrt(g$post.sd),
                                "Median"=g$post.mean,
                                "1st Qu."=g$post.intv.low,
                                "3rd Qu."=g$post.intv.upp)[selection,],3))

names(table1)<-names(table2)<-c("Parameter","Mean","s.d.","median","1st Qu.","3rd Qu." )

@
<<echo=FALSE,results=hide>>=
  table2$Parameter<-gsub("}","}$",table2$Parameter)
table2$Parameter<-gsub("\\\\theta","$\\\\theta",table2$Parameter)
@


\begin{enumerate}
\item 
The following graphs show the estimated posterior densities of $\beta$ and $A$.
\begin{figure}[H]
\caption{distribution of $\beta$}
\Sexpr{graph1}
\end{figure}




\item
Next table gives mean, median, standard deviation, and 95\% credible intervals of $\beta_1$ and $A$:
  
  \Sexpr{stargazer2(table1,summary=FALSE,title="Summary statistics on of $\\beta_1$ and $A$.")}

\item
Next table gives mean, median, standard deviation, and 95\% credible intervals of $\theta_i$ for counties with larger an smaller s.d.:
  \Sexpr{stargazer2(table2,summary=FALSE,title="Summary statistics on for  $\\theta_i$.")}

\item Next graphs represent the estimated posterior densities of $\theta_i$ for counties with  largest and smallest s.d. 
\begin{figure}[H]
\caption{Distribution of $\theta_i$, smallest and largest s.d.}
\Sexpr{graph2}
\end{figure}


\end{enumerate}

<<eval=FALSE>>=
  <<R1>>
  <<R2>>
  @
