## File to estimate parameters from a Dirichlet tree
## Tree is found using treeFinder.R
library(sirt)
library(DirichletReg)
## Tree results are in the plain text file ../Data/treesByYear.txt
## LRT results are in plain text file ../Data/LRTresultsByYear.txt

## Data 
# batter.dat <- read.csv("../Data/battersByPercentRhp.csv", stringsAsFactors = TRUE)
# batter.dat <- batter.dat[,-1]

## Determine alpha values for overall tree
## Indices refer to outcomes 1 = HR, 2 = T, 3 = D
## 4 = S, 5 = Out, 6 = Other
# Fix Zeroes
eps <- 10^-5
batter.dat[,1:6] <- (batter.dat[,1:6] + eps)/(1+2*eps)
batter.dat[,1:6] <- batter.dat[,1:6]/rowSums(batter.dat[,1:6])

## Create Layers
GM <- batter.dat[,7]
S1 <- rowSums(batter.dat[, 1:3]) # indices are based on tree
S2 <- rowSums(batter.dat[,4:6])
S3 <- rowSums(batter.dat[,1:2]) # sum of HR and T
S4 <- rowSums(batter.dat[,c(4, 6)]) # sum of S and other

# source("LRTfunction.R")
## LR statistic for top split ("good" events from "bad" events)
L1.dat <- cbind(S1, S2)
# a1 <- my.lrt.fun.3(L1.dat)
alpha1 <- dirichlet.mle(L1.dat)$alpha
L1.dat <- DR_data(L1.dat) # make sure it's a DirchletReg object
mod_root <- DirichletReg::DirichReg(L1.dat ~ GM | GM, model="alternative")
mod_rootnull <- DirichletReg::DirichReg(L1.dat ~ 1 | GM, model="alternative")
LRT_root<-2*(mod_root$logLik-mod_rootnull$logLik)
LRT_root
pchisq(LRT_root, 2, lower.tail=FALSE)

L2.dat <- cbind(batter.dat[,3]/S1, S3/S1)
alpha2 <- dirichlet.mle(L2.dat)$alpha
# a2 <- my.lrt.fun.3(L2.dat)
L2.dat <- DR_data(L2.dat) # make sure it's a DirchletReg object
mod_l2 <- DirichletReg::DirichReg(L2.dat ~ GM | GM, model="alternative")
summary(mod_l2) # to get precision estimates
mod_l2null <- DirichletReg::DirichReg(L2.dat ~ 1 | GM, model="alternative")
LRT_layer2<-2*(mod_l2$logLik-mod_l2null$logLik)
LRT_layer2
pchisq(LRT_layer2, 2, lower.tail=FALSE)

L3.dat <- cbind(batter.dat[,5]/S2, S4/S2)
alpha3 <- dirichlet.mle(L3.dat)$alpha
# a3 <- my.lrt.fun.3(L3.dat)
L3.dat <- DR_data(L3.dat) # make sure it's a DirchletReg object
mod_l3 <- DirichletReg::DirichReg(L3.dat ~ GM | GM, model="alternative")
mod_l3null <- DirichletReg::DirichReg(L3.dat ~ 1 | GM, model="alternative")
LRT_layer3<-2*(mod_l3$logLik-mod_l3null$logLik)
LRT_layer3
pchisq(LRT_layer3, 2, lower.tail=FALSE)

L4.dat <- cbind(batter.dat[,1]/S3, batter.dat[,2]/S3)
alpha4 <- dirichlet.mle(L4.dat)$alpha
# a4 <- my.lrt.fun.3(L4.dat)
L4.dat <- DR_data(L4.dat) # make sure it's a DirchletReg object
mod_l4 <- DirichletReg::DirichReg(L4.dat ~ GM | GM, model="alternative")
mod_l4null <- DirichletReg::DirichReg(L4.dat ~ 1 | GM, model="alternative")
LRT_layer4<-2*(mod_l4$logLik-mod_l4null$logLik)
LRT_layer4
pchisq(LRT_layer4, 2, lower.tail=FALSE)

L5.dat <- cbind(batter.dat[,4]/S4, batter.dat[,6]/S4)
alpha5 <- dirichlet.mle(L5.dat)$alpha
# a5 <- my.lrt.fun.3(L5.dat)
L5.dat <- DR_data(L5.dat) # make sure it's a DirchletReg object
mod_l5 <- DirichletReg::DirichReg(L5.dat ~ GM | GM, model="alternative")
mod_l5null <- DirichletReg::DirichReg(L5.dat ~ 1 | GM, model="alternative")
LRT_layer5<-2*(mod_l5$logLik-mod_l5null$logLik)
LRT_layer5
pchisq(LRT_layer5, 2, lower.tail=FALSE)
alpha1
alpha2
alpha3
alpha4
alpha5
overall.stat <- sum(c(a1, a2, a3, a4, a5))
overall.stat
pchisq(overall.stat, 15, lower.tail=FALSE)
alpha1/sum(alpha1)
alpha2/sum(alpha2)
alpha3/sum(alpha3)
alpha4/sum(alpha4)
alpha5/sum(alpha5)

## Determine alpha values for Year = 2010
## 2010 Season for young batters using tree for middle batters
## Use data for the whole season to make layers
# Fix Zeroes
eps <- 10^-5
batter2010[,1:6] <- (batter2010[,1:6] + eps)/(1+2*eps)
batter2010[,1:6] <- batter2010[,1:6]/rowSums(batter2010[,1:6])

## Step 4: create Layers
# indices are based on tree
GM <- batter2010[,7] # age group
S1 <- rowSums(batter2010[, 1:3]) # favorable outcomes
S2 <- rowSums(batter2010[,4:6])  # unfavorable outcomes
S3 <- rowSums(batter2010[,1:2])  # sum of HR and T
S4 <- rowSums(batter2010[,c(4, 6)]) # sum of S and other

# source("../BaseballAnalysis/LRTfunction.R")
## LR statistic for top split ("good" events from "bad" events)
L1.dat <- cbind(S1, S2)
alpha1 <- dirichlet.mle(L1.dat)$alpha
# a1 <- my.lrt.fun.3(L1.dat)
L1.dat <- DR_data(L1.dat) # make sure it's a DirchletReg object
mod_root <- DirichletReg::DirichReg(L1.dat ~ GM | GM, model="alternative")
mod_rootnull <- DirichletReg::DirichReg(L1.dat ~ 1 | GM, model="alternative")
LRT_root<-2*(mod_root$logLik-mod_rootnull$logLik)
LRT_root
pchisq(LRT_root, 2, lower.tail=FALSE)

L2.dat <- cbind(batter2010[,3]/S1, S3/S1)
alpha2 <- dirichlet.mle(L2.dat)$alpha
# a2 <- my.lrt.fun.3(L2.dat)
L2.dat <- DR_data(L2.dat) # make sure it's a DirchletReg object
mod_l2 <- DirichletReg::DirichReg(L2.dat ~ GM | GM, model="alternative")
mod_l2null <- DirichletReg::DirichReg(L2.dat ~ 1 | GM, model="alternative")
LRT_layer2<-2*(mod_l2$logLik-mod_l2null$logLik)
LRT_layer2
pchisq(LRT_layer2, 2, lower.tail=FALSE)

L3.dat <- cbind(batter2010[,5]/S2, S4/S2)
alpha3 <- dirichlet.mle(L3.dat)$alpha
# a3 <- my.lrt.fun.3(L3.dat)
L3.dat <- DR_data(L3.dat) # make sure it's a DirchletReg object
mod_l3 <- DirichletReg::DirichReg(L3.dat ~ GM | GM, model="alternative")
mod_l3null <- DirichletReg::DirichReg(L3.dat ~ 1 | GM, model="alternative")
LRT_layer3<-2*(mod_l3$logLik-mod_l3null$logLik)
LRT_layer3
pchisq(LRT_layer3, 2, lower.tail=FALSE)

L4.dat <- cbind(batter2010[,1]/S3, batter2010[,2]/S3)
alpha4 <- dirichlet.mle(L4.dat)$alpha
# a4 <- my.lrt.fun.3(L4.dat)
L4.dat <- DR_data(L4.dat) # make sure it's a DirchletReg object
mod_l4 <- DirichletReg::DirichReg(L4.dat ~ GM | GM, model="alternative")
mod_l4null <- DirichletReg::DirichReg(L4.dat ~ 1 | GM, model="alternative")
LRT_layer4<-2*(mod_l4$logLik-mod_l4null$logLik)
LRT_layer4
pchisq(LRT_layer4, 2, lower.tail=FALSE)

L5.dat <- cbind(batter2010[,4]/S4, batter2010[,6]/S4)
alpha5 <- dirichlet.mle(L5.dat)$alpha
# a5 <- my.lrt.fun.3(L5.dat)
L5.dat <- DR_data(L5.dat) # make sure it's a DirchletReg object
mod_l5 <- DirichletReg::DirichReg(L5.dat ~ GM | GM, model="alternative")
mod_l5null <- DirichletReg::DirichReg(L5.dat ~ 1 | GM, model="alternative")
LRT_layer5<-2*(mod_l5$logLik-mod_l5null$logLik)
LRT_layer5
pchisq(LRT_layer5, 2, lower.tail=FALSE)
alpha1
alpha2
alpha3
alpha4
alpha5
overall.stat <- sum(c(a1, a2, a3, a4, a5))
overall.stat
pchisq(overall.stat, 15, lower.tail=FALSE)
alpha1/sum(alpha1)
alpha2/sum(alpha2)
alpha3/sum(alpha3)
alpha4/sum(alpha4)
alpha5/sum(alpha5)


## 2000 Season for young batters using tree for middle batters
## Use data for the whole season to make layers
# Fix Zeroes
eps <- 10^-5
batter2000[,1:6] <- (batter2000[,1:6] + eps)/(1+2*eps)
batter2000[,1:6] <- batter2000[,1:6]/rowSums(batter2000[,1:6])

## Step 4: create Layers
# indices are based on tree
GM <- batter2000[,7] # age group
S1 <- rowSums(batter2000[, 1:3]) # favorable outcomes
S2 <- rowSums(batter2000[,4:6])  # unfavorable outcomes
S3 <- rowSums(batter2000[,1:2])  # sum of HR and T
S4 <- rowSums(batter2000[,c(4, 6)]) # sum of S and other

# source("../BaseballAnalysis/LRTfunction.R")
## LR statistic for top split ("good" events from "bad" events)
L1.dat <- cbind(S1, S2)
alpha1 <- dirichlet.mle(L1.dat)$alpha
# a1 <- my.lrt.fun.3(L1.dat)
L1.dat <- DR_data(L1.dat) # make sure it's a DirchletReg object
mod_root <- DirichletReg::DirichReg(L1.dat ~ GM | GM, model="alternative")
mod_rootnull <- DirichletReg::DirichReg(L1.dat ~ 1 | GM, model="alternative")
LRT_root<-2*(mod_root$logLik-mod_rootnull$logLik)
LRT_root
pchisq(LRT_root, 2, lower.tail=FALSE)

L2.dat <- cbind(batter2000[,3]/S1, S3/S1)
alpha2 <- dirichlet.mle(L2.dat)$alpha
# a2 <- my.lrt.fun.3(L2.dat)
L2.dat <- DR_data(L2.dat) # make sure it's a DirchletReg object
mod_l2 <- DirichletReg::DirichReg(L2.dat ~ GM | GM, model="alternative")
mod_l2null <- DirichletReg::DirichReg(L2.dat ~ 1 | GM, model="alternative")
LRT_layer2<-2*(mod_l2$logLik-mod_l2null$logLik)
LRT_layer2
pchisq(LRT_layer2, 2, lower.tail=FALSE)

L3.dat <- cbind(batter2000[,5]/S2, S4/S2)
alpha3 <- dirichlet.mle(L3.dat)$alpha
# a3 <- my.lrt.fun.3(L3.dat)
L3.dat <- DR_data(L3.dat) # make sure it's a DirchletReg object
mod_l3 <- DirichletReg::DirichReg(L3.dat ~ GM | GM, model="alternative")
mod_l3null <- DirichletReg::DirichReg(L3.dat ~ 1 | GM, model="alternative")
LRT_layer3<-2*(mod_l3$logLik-mod_l3null$logLik)
LRT_layer3
pchisq(LRT_layer3, 2, lower.tail=FALSE)

L4.dat <- cbind(batter2000[,1]/S3, batter2000[,2]/S3)
alpha4 <- dirichlet.mle(L4.dat)$alpha
# a4 <- my.lrt.fun.3(L4.dat)
L4.dat <- DR_data(L4.dat) # make sure it's a DirchletReg object
mod_l4 <- DirichletReg::DirichReg(L4.dat ~ GM | GM, model="alternative")
mod_l4null <- DirichletReg::DirichReg(L4.dat ~ 1 | GM, model="alternative")
LRT_layer4<-2*(mod_l4$logLik-mod_l4null$logLik)
LRT_layer4
pchisq(LRT_layer4, 2, lower.tail=FALSE)

L5.dat <- cbind(batter2000[,4]/S4, batter2000[,6]/S4)
alpha5 <- dirichlet.mle(L5.dat)$alpha
# a5 <- my.lrt.fun.3(L5.dat)
L5.dat <- DR_data(L5.dat) # make sure it's a DirchletReg object
mod_l5 <- DirichletReg::DirichReg(L5.dat ~ GM | GM, model="alternative")
mod_l5null <- DirichletReg::DirichReg(L5.dat ~ 1 | GM, model="alternative")
LRT_layer5<-2*(mod_l5$logLik-mod_l5null$logLik)
LRT_layer5
pchisq(LRT_layer5, 2, lower.tail=FALSE)
alpha1
alpha2
alpha3
alpha4
alpha5
overall.stat <- sum(c(a1, a2, a3, a4, a5))
overall.stat
pchisq(overall.stat, 15, lower.tail=FALSE)
alpha1/sum(alpha1)
alpha2/sum(alpha2)
alpha3/sum(alpha3)
alpha4/sum(alpha4)
alpha5/sum(alpha5)

# overall.stat <- sum(c(a1, a2, a3, a4, a5))
# df = number of groups * number of 
# pchisq(overall.stat, 15, lower.tail=FALSE)
## Simulations to check model
## Create a data set with 10,000 observations.
## See if the estimates fit the Dirichlet distribution
set.seed(1983)
# a is on left split
# b is on right split
level.one.dat <- gtools::rdirichlet(10000, alpha1)
level.two.a.dat <- gtools::rdirichlet(10000, alpha2)
level.two.b.dat <- gtools::rdirichlet(10000, alpha3)
level.three.a.dat <- gtools::rdirichlet(10000, alpha4)
level.three.b.dat <- gtools::rdirichlet(10000, alpha5)

sim.baseball.dat <-
  cbind("HR" = level.one.dat[,1]*level.two.a.dat[,2]*level.three.a.dat[,1],
        "TRI" = level.one.dat[,1]*level.two.a.dat[,2]*level.three.a.dat[,2],
        "DBL"= level.one.dat[,1]*level.two.a.dat[,1],
        "SGL" = level.one.dat[,2]*level.two.b.dat[,2]*level.three.b.dat[,1],
        "OUT" =  level.one.dat[,2]*level.two.b.dat[,1],
        "OTR" = level.one.dat[,2]*level.two.b.dat[,2]*level.three.b.dat[,2])

## Check the row sums and column means.

rowSums(sim.baseball.dat)[1:200]
colMeans(sim.baseball.dat)
colMeans(batterAge1[,1:6])

## Check correlation structure

aa <- round(cor(batterAge2[, 1:6]), 3)
bb <- round(cor(sim.baseball.dat), 3)

xtable(aa, type=latex, digits = c(0, rep(3, 6)))
xtable(bb, type=latex, digits=c(0, rep(3,6)))

## 2005 Season using tree for 2005 season
# Fix Zeroes
eps <- 10^-5
batter2005[,1:6] <- (batter2005[,1:6] + eps)/(1+2*eps)
batter2005[,1:6] <- batter2005[,1:6]/rowSums(batter2005[,1:6])

## Step 4: create Layers
GM <- batter2005[,7]
S1 <- rowSums(batter2005[, c(2,5)]) # triple and out
S2 <- rowSums(batter2005[,c(1, 3, 4, 6)]) 
S3 <- rowSums(batter2005[, c(4,6)]) # sum of S and other
S4 <- rowSums(batter2005[,c(1, 3)]) # sum of HR and Double

L1.dat <- cbind(S1, S2)
alpha1 <- dirichlet.mle(L1.dat)$alpha
# a1 <- my.lrt.fun.3(L1.dat)
L1.dat <- DR_data(L1.dat) # make sure it's a DirchletReg object
mod_root <- DirichletReg::DirichReg(L1.dat ~ GM | GM, model="alternative")
mod_rootnull <- DirichletReg::DirichReg(L1.dat ~ 1 | GM, model="alternative")
LRT_root<-2*(mod_root$logLik-mod_rootnull$logLik)
LRT_root
pchisq(LRT_root, 2, lower.tail=FALSE)

L2.dat <- cbind(batter2005[,2]/S1, batter2005[,5]/S1)
alpha2 <- dirichlet.mle(L2.dat)$alpha
# a2 <- my.lrt.fun.3(L2.dat)
L2.dat <- DR_data(L2.dat) # make sure it's a DirchletReg object
mod_l2 <- DirichletReg::DirichReg(L2.dat ~ GM | GM, model="alternative")
mod_l2null <- DirichletReg::DirichReg(L2.dat ~ 1 | GM, model="alternative")
LRT_layer2<-2*(mod_l2$logLik-mod_l2null$logLik)
LRT_layer2
pchisq(LRT_layer2, 2, lower.tail=FALSE)

L3.dat <- cbind(S3/S2, S4/S2)
alpha3 <- dirichlet.mle(L3.dat)$alpha
# a3 <- my.lrt.fun.3(L3.dat)
L3.dat <- DR_data(L3.dat) # make sure it's a DirchletReg object
mod_l3 <- DirichletReg::DirichReg(L3.dat ~ GM | GM, model="alternative")
mod_l3null <- DirichletReg::DirichReg(L3.dat ~ 1 | GM, model="alternative")
LRT_layer3<-2*(mod_l3$logLik-mod_l3null$logLik)
LRT_layer3
pchisq(LRT_layer3, 2, lower.tail=FALSE)

L4.dat <- cbind(batter2005[,4]/S3, batter2005[,6]/S3)
alpha4 <- dirichlet.mle(L4.dat)$alpha
# a4 <- my.lrt.fun.3(L4.dat)
L4.dat <- DR_data(L4.dat) # make sure it's a DirchletReg object
mod_l4 <- DirichletReg::DirichReg(L4.dat ~ GM | GM, model="alternative")
mod_l4null <- DirichletReg::DirichReg(L4.dat ~ 1 | GM, model="alternative")
LRT_layer4<-2*(mod_l4$logLik-mod_l4null$logLik)
LRT_layer4
pchisq(LRT_layer4, 2, lower.tail=FALSE)

L5.dat <- cbind(batter2005[,1]/S4, batter2005[,3]/S4)
alpha5 <- dirichlet.mle(L5.dat)$alpha
#a5 <- my.lrt.fun.3(L5.dat)
L5.dat <- DR_data(L5.dat) # make sure it's a DirchletReg object
mod_l5 <- DirichletReg::DirichReg(L5.dat ~ GM | GM, model="alternative")
mod_l5null <- DirichletReg::DirichReg(L5.dat ~ 1 | GM, model="alternative")
LRT_layer5<-2*(mod_l5$logLik-mod_l5null$logLik)
LRT_layer5
pchisq(LRT_layer5, 2, lower.tail=FALSE)
alpha1
alpha2
alpha3
alpha4
alpha5
overall.stat <- sum(c(a1, a2, a3, a4, a5))
overall.stat
pchisq(overall.stat, 15, lower.tail=FALSE)
alpha1/sum(alpha1)
alpha2/sum(alpha2)
alpha3/sum(alpha3)
alpha4/sum(alpha4)
alpha5/sum(alpha5)

