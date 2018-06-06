
####nonparametric @parissa.19
###one sample sign test-21 p72 behboudian
install.packages("BSDA")
library(BSDA)
m<-c(163,165,162,189,161,171,158,151,169,182,163,139,172,165,148,166
     ,172,163,187,173)
SIGN.test(m,md=160,alternative = "greater")

####1-p96
#wilcox-test

x<-c(12,9,13,11,14)
y<-c(8,10,7)
wilcox.test(y,x,alt="less")

###22-p73

x<-c(89,90,86,80,97,81,94,82,87,93,94,84,83,78,98)
z<-x>=85
z1<-x<=85
s<-sum(z,z1)
binom.test(s,15,p=0.75,alt="t")

###3-p131
#wilcox signed rank test

x<-c(7,20,6,13,14,8,12,9,10)
y<-c(15,-7,3,7,6,1,3,11,9)
wilcox.test(y,x,exact = F,paired = T,alt="l")

####sign test for (X,Y)
x<-c(7,20,6,13,14,8,12,9,10)
y<-c(15,-7,3,7,6,1,3,11,9)
d<-y-x
z<-sum(d>0)
binom.test(z,9,p=0.548,alt="less")

#CI
prop.test(z,9,conf.level = 0.99,p=0.548)

####3-p227 fisher exact test
m<-c(6,4)
w<-c(8,5)
z<-c(m,w)
a<-matrix(z,2,2,byrow="T")
colnames<-c("live","dead")
rownames<-c("man","woman")
##x=matrix(c(6,4,8,5),2,2,byrow=T)
fisher.test(a)

####5 p216 ks.test
x<-c(10.3,11.2,11.5,11.9,12.8)
y<-c(10.4,11.8,12.5,12.6,13.8,13.9)
ks.test(x,y)

##1-p222
####kruskal test
y<-c(19,11.7,17.8,14.8,13.9,18.2,14.8,13.1,12.6,15.2,12.8,13.4,14.1,12.3,12.3,14.7,13.9,13.8,14.3)
#groups<-factor(c(1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3))
groups<-factor(rep(1:3,c(5,6,8)))
labels<-c("A uni","B uni","C uni")
kruskal.test(y,groups)

####feridman
#4-p230
y<-matrix(c(3,4,3,4,3,4,2,2,1,1,1,2),ncol = 3,nrow =4,byrow = T)
groups<-factor(as.vector(row(y)))
blocks<-factor(as.vector(col(y)))
friedman.test(y,groups,blocks)
