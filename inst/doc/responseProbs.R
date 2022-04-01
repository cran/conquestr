## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----slmItem------------------------------------------------------------------
library(conquestr)
myItem<- matrix(c(0, 0, 0, 1, 1, 0), ncol =3, byrow=TRUE)
colnames(myItem)<- c("k", "d", "t")
print(myItem)

## ----slmProbs-----------------------------------------------------------------
myProbs<- simplep(0.5, myItem)
print(myProbs)

## ----slmICC-------------------------------------------------------------------
myProbsList<- list()
myThetaRange<- seq(-4, 4, by = 0.1)
for(i in seq(myThetaRange)){
  myProbsList[[i]]<- pX(x = 1, probs = simplep(myThetaRange[i], myItem))
}
plot(unlist(myProbsList))

## ----polyItem-----------------------------------------------------------------
library(conquestr)
myItem<- matrix(c(0, 0, 0, 1, 1, -0.2, 2, 1, 0.2), ncol =3, byrow=TRUE)
colnames(myItem)<- c("k", "d", "t")
print(myItem)

## ----polyProbs----------------------------------------------------------------
myProbs<- simplep(0.5, myItem)
print(myProbs)

## ----polyICC------------------------------------------------------------------
myProbsList<- list()
myThetaRange<- seq(-4, 4, by = 0.1)
for(i in seq(myThetaRange)){
  myProbsList[[i]]<- simplep(myThetaRange[i], myItem)
}
myProbs<- (matrix(unlist(myProbsList), ncol = 3, byrow = TRUE))
plot(myThetaRange, myProbs[,1])
points(myThetaRange, myProbs[,2])
points(myThetaRange, myProbs[,3])
abline(v = c(myItem[2, 2], sum(myItem[2, 2:3]), sum(myItem[3, 2:3])))

## ----Expected-----------------------------------------------------------------
library(conquestr)
myItems<- list()
myItems[[1]]<- matrix(c(0, 0, 0, 1, 1, -0.2, 2, 1, 0.2), ncol =3, byrow=TRUE)
myItems[[2]]<- matrix(c(0, 0, 0, 1, -1, -0.4, 2, -1, 0.4), ncol =3, byrow=TRUE)
myItems[[3]]<- matrix(c(0, 0, 0, 1, 1.25, -0.6, 2, 1.25, 0.6), ncol =3, byrow=TRUE)
myItems[[4]]<- matrix(c(0, 0, 0, 1, 2, 0.2, 2, 2, -0.2), ncol =3, byrow=TRUE)
myItems[[5]]<- matrix(c(0, 0, 0, 1, -2.5, -0.2, 2, -2.5, 0.2), ncol =3, byrow=TRUE)
for(i in seq(myItems)){
  colnames(myItems[[i]])<- c("k", "d", "t")
}
print(myItems)

expectedRes<- list()
for(i in seq(myThetaRange)){
  tmpExp<- 0
  for(j in seq(myItems)){
    tmpE<- simplef(myThetaRange[i], myItems[[j]])
    tmpExp<- tmpExp + tmpE
  }
  expectedRes[[i]]<- tmpExp
}

plot(myThetaRange, unlist(expectedRes))


## ----working, include=FALSE---------------------------------------------------

simplep(0.5, myItem)

myProbs<- simplep(0.5, myItem)

pX(2, simplep(0.5, myItem))

tTheta<- 0.5
p0tmp<- exp((0*tTheta) - (0)) # by def. this = 1
p1tmp<- exp((1*tTheta) - (0 + 0.8))
p2tmp<- exp((2*tTheta) - (0 + 0.8 + 1.2))
p_denom<- sum(p0tmp, p1tmp, p2tmp)
p_of_0<- p0tmp/p_denom
p_of_1<- p1tmp/p_denom
p_of_2<- p2tmp/p_denom


myItem1<- matrix(c(0, 0, 0, 1, 1, 0), ncol =3, byrow=TRUE)
simplep(0.5, myItem1)

exp(0.5-1)/(1+exp(0.5-1))
sum(exp(2*0.5-(0.8+1.2)))/sum(1, exp(0.5-0.8), exp(2*0.5-(0.8+1.2)))


