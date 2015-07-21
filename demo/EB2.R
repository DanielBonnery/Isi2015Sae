data(baseball);attach(baseball)
library("sae")
resultML<-mseFH(hat.P~x1,mean(hat.P)*(1-mean(hat.P))/45,method="ML")
resultML