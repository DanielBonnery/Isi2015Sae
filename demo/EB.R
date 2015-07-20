
data(baseball)
attach(baseball)
m = nrow(baseball)
n = 45
#1. direct(sample proportion)
hat.P.d = hat.P
#2. overall sample proportion, a synthetic estimator when domain specific auxiliary information is not available
osp = mean(hat.P)
#3. regression synthetic estimator(when area specific auxiliary information is available)
baseball.reg = lm(hat.P ~ x1, data = baseball)
b.hat = baseball.reg$coeff
hat.P.s = baseball.reg$fitted.values
#4. regression synthetic estimator using a logit link
logit.hat.P = log(hat.P/(1-hat.P))
baseball.reg.logit = lm(logit.hat.P ~ x1, data = baseball)
b.hat.logit = baseball.reg.logit$coeff
hat.P.s.logit = exp(baseball.reg.logit$fitted.values)/(1+ exp(baseball.reg.logit$fitted.values))
#5. composite 1: different weight for different Players
var.dir = (hat.P.d*(1 - hat.P.d))/n
phi.i.hat.star = 1 - var.dir/(hat.P.s - hat.P.d)^2
hat.P.c1 = phi.i.hat.star*hat.P.d + (1 - phi.i.hat.star)*hat.P.s
### composite 2: same weight
num = sum(var.dir)
den = sum((hat.P.s - hat.P.d)^2)
phi.hat.star = 1 - (num/den)
hat.P.c2 = phi.hat.star*hat.P.d + (1 - phi.hat.star)*hat.P.s
### EB or EBLUP using common mean model(assuming no auxiliary information is available)
shi.hat = osp*(1 - osp)/n
A.hat = var(hat.P) - shi.hat
B.hat = shi.hat/(shi.hat + A.hat)
B.hat
hat.P.cm = (1 - B.hat)*hat.P + B.hat*osp
### EB or EBLUP using Fay-Herriot model(considering equal sampling variance and x1 and intercept as covariate)
shi.hat.s = osp*(1 - osp)/n
S.fh = sum((hat.P - hat.P.s)^2)
A.hat.fh = S.fh/(m - 2) - shi.hat.s
B.hat.fh = shi.hat.s/(shi.hat.s + A.hat.fh)
B.hat.fh
hat.P.fh = (1 - B.hat.fh)*hat.P + B.hat.fh*hat.P.s
res = round(cbind("Direct estimator"=hat.P.d, 
                  "Overall sample proportion"=osp, 
                  "Regression Synthetic"=hat.P.s, 
                  reg.synth.logit=hat.P.s.logit, "Comp1(diff wt)"=hat.P.c1,"Comp2(same wt)"=hat.P.c2, "EB:CM"=hat.P.cm, "EB:FH"=hat.P.fh),3)
res.baseball = data.frame("Player"=Player,res)

evalu=t(round(apply(res,2,function(r){
  c(ASD=sum((res[,i] - P)^2)/m,
    ARSD=sum(((res[,i] - P)/P)^2)/m,
    AAD=sum(abs(res[,i] - P))/m,
    ARAD=sum(abs((res[,i] - P)/P))/m)}),3))
evalu
@

