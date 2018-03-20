### Fox News Effect
### 20 mars 2018

rm(list=ls())
setwd("/Users/jeremylhour/Documents/R/FoxNewsEffect")


library("foreign")
data = read.dta("MAGANewsFinalData.dta")

reg = lm(maganews2000 ~ reppresfv2p1996 + totpreslvpop1996, data=data, weights=totpresvotes1996)
summary(reg)

mean(data[maganews2000 ==1,"reppresfv2p00m96"])

## Diff in diff
uu = data[,"reppresfv2p2000"] - data[,"reppresfv2p1996"]
reg = lm(reppresfv2p00m96 ~ maganews2000, data=data, weights=totpresvotes1996)
summary(reg)

reg = lm(reppresfv2p00m96 ~ maganews2000, data=data, weights=totpresvotes1996)
summary(reg)