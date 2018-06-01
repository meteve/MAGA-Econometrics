### Fox News Effect - Elements de correction
### 20 mars 2018
### Jeremy L'Hour

rm(list=ls())
# setwd("/Users/jeremylhour/Documents/R/FoxNewsEffect")
setwd("//ulysse/users/JL.HOUR/1A_These/B. ENSAE Classes/TD_Econometrie2/Projets_2018/FoxNews")

### package and datasets
library("foreign")
library("gmm")
data = read.dta("MAGANewsFinalData.dta")

################
################
### Partie 1 ###
################
################


### Question 1
apply(data[,c("maganews1998","maganews2000","maganews2003")],2,mean,na.rm=T)

# MAGA News n'est pas disponible dans toutes les villes, possibilités de définir
# un groupe traité et un groupe contrôle. Il y a en oure un "avant-après".

### Question 2
# Option 1
DiD = lm(reppresfv2p2000-reppresfv2p1996 ~ maganews2000, data=data)
summary(DiD)

# Option 2
DiDbase = data.frame("Vote" = c(data$reppresfv2p1996,data$reppresfv2p2000),
                     "Date" = c(rep(0,length(data$reppresfv2p1996)),rep(1,length(data$reppresfv2p2000))),
                     "MAGANews" = c(data$maganews2000,data$maganews2000),
                     "town" = c(1:9256,1:9256)) 
classicDiD = lm(Vote ~ Date*MAGANews, data=DiDbase)
summary(classicDiD)

# les ecart-stypes ne sont pas corrects avec cette façon de faire.


# On trouve un effet de -0.0124 (0.005)

### Question 3

# The impact is negative which is contrary to what we expect.
# Need to test common trend assumption (CTA)

# Evolution of vote from 1992 to 1996
CTA1base = data.frame("Vote" = c(data$reppresfv2p1992,data$reppresfv2p1996),
                     "Date" = c(rep(0,length(data$reppresfv2p1992)),rep(1,length(data$reppresfv2p1996))),
                     "MAGANews" = c(data$maganews2000,data$maganews2000)) 

CTA1 = lm(Vote ~ Date*MAGANews, data=CTA1base)
summary(CTA1)

# Evolution of vote from 1988 to 1992
CTA2base = data.frame("Vote" = c(data$reppresfv2p1988,data$reppresfv2p1992),
                      "Date" = c(rep(0,length(data$reppresfv2p1988)),rep(1,length(data$reppresfv2p1992))),
                      "MAGANews" = c(data$maganews2000,data$maganews2000)) 

CTA2 = lm(Vote ~ Date*MAGANews, data=CTA2base)
summary(CTA2)

# Looks like the CTA does not hold, on the second test.
# Lots of NAs in the data


### Question 4: see Table in the original paper


################
################
### Partie 2 ###
################
################

### Question 1

# MAGA News may have targeted more Republican towns in priority (i.e. towns with a larger share of Republican vote).
# And Republican vote is likely to be auto-correlated, hence an omitted variable bias to come in the regression.
# It would be good if at the end of this part, we have a model with the coefficient associated with these variables close to zero.

### Question 2

Logit1 = glm(maganews2000 ~ reppresfv2p1996 + totpreslvpop1996,family=binomial(link='logit'),data=data)
summary(Logit1)

# With demographic controls:
Logit2 = glm(maganews2000 ~ reppresfv2p1996 + totpreslvpop1996 +
               pop2000+hs2000+hsp2000+college2000+male2000+black2000+hisp2000+empl2000+unempl2000+married2000+income2000+urban2000+
               pop00m90+hs00m90+hsp00m90+college00m90+male00m90+black00m90+hisp00m90+empl00m90+unempl00m90+married00m90+income00m90+urban00m90,
             family=binomial(link='logit'),data=data)
summary(Logit2)

# With demographic and cable controls
Logit3 = glm(maganews2000 ~ reppresfv2p1996 + totpreslvpop1996 +
               pop2000+hs2000+hsp2000+college2000+male2000+black2000+hisp2000+empl2000+unempl2000+married2000+income2000+urban2000+
               pop00m90+hs00m90+hsp00m90+college00m90+male00m90+black00m90+hisp00m90+empl00m90+unempl00m90+married00m90+income00m90+urban00m90 +
               I(poptot2000d2-poptot2000d10)+I(noch2000d2-noch2000d10),
             family=binomial(link='logit'),data=data)
summary(Logit3)


### LPM, as comparison
reg = lm(maganews2000 ~ reppresfv2p1996 + totpreslvpop1996, data=data, weights=totpresvotes1996)
summary(reg)

### Question 3: should be taken care of before, performing corresponding Wald tests.

### Question 4 and 5
g = function(beta,W){
  D = W[,1]; X = W[,2:ncol(W)]
  g_indiv = sweep(X,MARGIN=1,(D - (1-D)*exp(X%*%beta)),`*`)
  return(g_indiv)
}

gands = function(beta,W){
  # 2p moments conditions
  D = W[,1]; X = W[,2:ncol(W)]
  g_indiv = sweep(X,MARGIN=1,D - (1-D)*exp(X%*%beta),`*`)
  s_indiv = sweep(X,MARGIN=1,D - 1/(1+exp(-X%*%beta)),`*`)
  return(cbind(g_indiv,s_indiv))
}

# GMM only based on g
gmmdata= cbind(data$maganews2000,rep(1,length(data$maganews2000)),data$reppresfv2p1996,data$totpreslvpop1996,
               data$pop2000, data$hs2000, data$hsp2000, data$college2000, data$male2000, data$black2000, data$hisp2000, data$empl2000, data$unempl2000, data$married2000, data$income2000, data$urban2000,
               data$pop00m90, data$hs00m90, data$hsp00m90, data$college00m90, data$male00m90, data$black00m90, data$hisp00m90, data$empl00m90, data$unempl00m90, data$married00m90, data$income00m90, data$urban00m90)

JustID_gmm = gmm(g,gmmdata,rep(0,ncol(gmmdata)-1),type="twoStep")
summary(JustID_gmm)

### GMM based on both g and s
big_gmm = gmm(gands,gmmdata,rep(0,ncol(gmmdata)-1),type="twoStep")
summary(big_gmm)


################
################
### Partie 3 ###
################
################

### Question 1
# We can start from the diff-in-diff estimator
wDiD = lm(reppresfv2p00m96 ~ maganews2000, data=data, weights=totpresvotes1996)
summary(wDiD)
# ... and add variable that appeared significant in the previous step.
# Adding the lagged outcome can be a good idea
# State-level or cable companies fixed effect can be useful
# clusetring at the state level can be useful.

DiD_controls = lm(reppresfv2p00m96 ~ maganews2000 + reppresfv2p1996 + totpreslvpop1996 +
                    pop2000+hs2000+hsp2000+college2000+male2000+black2000+hisp2000+empl2000+unempl2000+married2000+income2000+urban2000+
                    pop00m90+hs00m90+hsp00m90+college00m90+male00m90+black00m90+hisp00m90+empl00m90+unempl00m90+married00m90+income00m90+urban00m90,
                  data=data, weights=totpresvotes1996)
summary(DiD_controls)

### Question 2
theta = function(Y,D,X,beta){
  t_indiv = (D - (1-D)*exp(X%*%beta))*Y
  return(mean(t_indiv)/mean(D))
} # theta estimator

thetaSD = function(Y,D,X,beta){
  t_hat = theta(Y,D,X,beta)
  mu_hat = coef(lm(Y ~ X-1, weight=(1-D)*exp(X%*%beta)))
  t_indiv = (D - (1-D)*exp(X%*%beta))*(Y - X%*%mu_hat) - D*t_hat
  return(var(t_indiv)/mean(D)^2)
} # standard error estimator

beta_h = coef(JustID_gmm)

theta_h = theta(Y=data$reppresfv2p00m96,
                D=data$maganews2000,
                X=cbind(rep(1,length(data$maganews2000)),data$reppresfv2p1996,data$totpreslvpop1996,
                        data$pop2000, data$hs2000, data$hsp2000, data$college2000, data$male2000, data$black2000, data$hisp2000, data$empl2000, data$unempl2000, data$married2000, data$income2000, data$urban2000,
                        data$pop00m90, data$hs00m90, data$hsp00m90, data$college00m90, data$male00m90, data$black00m90, data$hisp00m90, data$empl00m90, data$unempl00m90, data$married00m90, data$income00m90, data$urban00m90),
                beta_h)

sigma_theta_h = thetaSD(Y=data$reppresfv2p00m96,
                D=data$maganews2000,
                X=cbind(rep(1,length(data$maganews2000)),data$reppresfv2p1996,data$totpreslvpop1996,
                          data$pop2000, data$hs2000, data$hsp2000, data$college2000, data$male2000, data$black2000, data$hisp2000, data$empl2000, data$unempl2000, data$married2000, data$income2000, data$urban2000,
                          data$pop00m90, data$hs00m90, data$hsp00m90, data$college00m90, data$male00m90, data$black00m90, data$hisp00m90, data$empl00m90, data$unempl00m90, data$married00m90, data$income00m90, data$urban00m90),
                beta_h)
print(sqrt(sigma_theta_h/length(data$maganews2000)))

# NB: il faudrait ajouter les pondérations pour rendre les choses comparables.

### Autre possibilité: durectement inclure theta dans les GMM
gwtheta = function(param,W){
  theta = param[1]; beta = param[2:length(param)]
  Y = W[,1]; D = W[,2]; X = W[,3:ncol(W)]
  g_indiv = cbind((D - (1-D)*exp(X%*%beta))*Y - D*theta,
                  sweep(X,MARGIN=1,(D - (1-D)*exp(X%*%beta)),`*`))
  return(g_indiv)
}

fullgmmdata = cbind(data$reppresfv2p00m96,
                    data$maganews2000,
                    rep(1,length(data$maganews2000)),data$reppresfv2p1996,data$totpreslvpop1996,
                    data$pop2000, data$hs2000, data$hsp2000, data$college2000, data$male2000, data$black2000, data$hisp2000, data$empl2000, data$unempl2000, data$married2000, data$income2000, data$urban2000,
                    data$pop00m90, data$hs00m90, data$hsp00m90, data$college00m90, data$male00m90, data$black00m90, data$hisp00m90, data$empl00m90, data$unempl00m90, data$married00m90, data$income00m90, data$urban00m90)

full_gmm = gmm(gwtheta,fullgmmdata,rep(0,ncol(fullgmmdata)-1),type="twoStep",wmatrix="ident")
summary(full_gmm)

beta_h = coef(full_gmm)

theta_h = theta(Y=data$reppresfv2p00m96,
                D=data$maganews2000,
                X=cbind(rep(1,length(data$maganews2000)),data$reppresfv2p1996,data$totpreslvpop1996,
                        data$pop2000, data$hs2000, data$hsp2000, data$college2000, data$male2000, data$black2000, data$hisp2000, data$empl2000, data$unempl2000, data$married2000, data$income2000, data$urban2000,
                        data$pop00m90, data$hs00m90, data$hsp00m90, data$college00m90, data$male00m90, data$black00m90, data$hisp00m90, data$empl00m90, data$unempl00m90, data$married00m90, data$income00m90, data$urban00m90),
                beta_h[-1])