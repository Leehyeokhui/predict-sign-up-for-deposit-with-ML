rm(list=ls())
setwd("C:\\Users\\Administrator\\Desktop\\My\\R program\\final thesis")
source("data.mining.functions.2020.0602.R")
library(stats)
library(ggplot2)
library(corrplot)
library(car)
library(spatstat)
library(glmnet)
library(ncpen)
library(rpart)
library(rpart.plot)
library(MASS)
library(randomForest)
library(gbm)

bank = read.csv(file.choose())

# month 구간 분기별로 합치기
bankmonth = bank[,14]
bankmonth = ifelse(bank[,14]=="jan" | bank[,14]=="feb" | bank[,14]=="mar","1분기",ifelse(bank[,14]=="apr" | bank[,14]=="may" | bank[,14]=="jun","2분기",ifelse(bank[,14]=="jul" | bank[,14]=="aug" | bank[,14]=="sep","3분기","4분기")))
bank[,14] = bankmonth

# dummify
dummify(bank[,7])[,-1] #admin 제외
dummify(bank[,8])[,-1] #divorced 제외
dummify(bank[,9])[,-4] #unknown 제외
dummify(bank[,10])[,-1] # no 제외
dummify(bank[,11])[,-1] # no 제외
dummify(bank[,12])[,-1] # no 제외
dummify(bank[,13])[,-3] # unknown 제외
dummify(bank[,14])[,-1] # 1분기 제외
dummify(bank[,15])[,-4] # unknown 제외
dummify(bank[,16])[,-2] # yes 제외
bank = cbind(bank[,-c(7:16)],dummify(bank[,7])[,-1],dummify(bank[,8])[,-1],dummify(bank[,9])[,-4],dummify(bank[,10])[,-1],dummify(bank[,11])[,-1],dummify(bank[,12])[,-1],dummify(bank[,13])[,-3],dummify(bank[,14])[,-1],dummify(bank[,15])[,-4],dummify(bank[,16])[,-2])
names(bank)[c(23:25,34)] = c("default","housing","loan","deposit")
bank

# performance test by randomization
y.vec = as.vector(bank[,34])
x.mat = as.matrix(bank[,-34])

# ready
rm.vec = c("logit","ridge","lasso")
rb.mat = matrix(NA,nrow=1+ncol(x.mat),ncol=length(rm.vec))
colnames(rb.mat) = rm.vec
rownames(rb.mat) = c("intercept",colnames(x.mat))
cv.id = cv.index.fun(y.vec,k.val=10)

# 50 random test accuracy with cross validation
s.num = 50
r.mat = rand.index.fun(y.vec,s.num=s.num)
mod = c("logit","ridge","lasso","tree","prtree","boosting")
e.mat = matrix(NA,s.num,length(mod))
colnames(e.mat) = mod

for(s.id in 1:s.num){
  print(s.id)
  # partition
  set = r.mat[,s.id]
  txy.df = bank[set,]
  nxy.df = bank[!set,]
  tx.mat = x.mat[set,]
  nx.mat = x.mat[!set,]
  ty.vec = y.vec[set]
  ny.vec = y.vec[!set]

  # logistic regression
  log.fit = glm(deposit~.,family=binomial,data=txy.df)
  rb.mat[,"logit"] = coef(log.fit)

  # ridge penalty
  ridge.fit = cv.glmnet(tx.mat,ty.vec,family="binomial",alpha=0,type.measure="class")
  plot(ridge.fit$cvm)
  opt = which.min(ridge.fit$cvm)
  rb.mat[,"ridge"] = coef(ridge.fit$glmnet.fit)[,opt]

  # lasso penalty
  lasso.fit = cv.glmnet(tx.mat,ty.vec,family="binomial",alpha=1,type.measure="class")
  plot(lasso.fit$cvm)
  opt = which.min(lasso.fit$cvm)
  rb.mat[,"lasso"] = coef(lasso.fit$glmnet.fit)[,opt]

  # assessment
  r.ass = glm.ass.fun(ny.vec,nx.mat,b.mat=rb.mat,mod="binomial")$ass
  r.ass

  # decision tree
  tree = rpart(deposit~.,data=txy.df,method="class")
  rpart.plot(tree)
  pruned.tree = prune(tree,cp=0.03)
  rpart.plot(pruned.tree)
  prunedpred.tree = predict(pruned.tree,nxy.df,type="class")
  r.auc.prtr = sum(prunedpred.tree==ny.vec)/nrow(nxy.df)*100 # 78%
  
  pred.tree = predict(tree,nxy.df,type="class")
  r.auc.tr = sum(pred.tree==ny.vec)/nrow(nxy.df)*100 # 79%
  
  # generalized gradient boosting
  set.seed(1234)
  fit = gbm(deposit~.,data=txy.df,distribution = "adaboost",n.trees=1000,cv.fold=10)
  opt = gbm.perf(fit,method="cv",plot.it=FALSE)
  pred.b = predict(fit,newdata=nxy.df[,-34],n.trees=opt)>0
  r.auc.b = sum(pred.b==ny.vec)/nrow(nxy.df)*100 # 81%
  
  # acuracy 비교
  r.auc = c(r.ass[,10],r.auc.tr,r.auc.prtr,r.auc.b)
  names(r.auc)[4:6]=c("tree","prtree","boosting")
  r.auc
  
  e.mat[s.id,] = c(r.ass[,10],r.auc.tr,r.auc.prtr,r.auc.b)
}
e.mat
e.mat[,c(1,2,3)] = e.mat[,c(1,2,3)]*100

one = c(81.57654,81.48916,81.78934,82.45219,82.49872,81.54813,83.68451,81.04588,82.75924,81.59843)
two = c(81.54684,83.48412,83.54867,81.54945,82.48915,82.24684,81.45864,83.15648,82.15851,81.43156)
three = c(81.61898,81.05684,82.16826,83.15475,81.01564,81.45864,82.92546,81.56482,81.92315,82.15647)
four = c(81.15686,82.15674,83.01648,82.16489,82.95745,82.46536,83.66425,81.25257,81.89325,81.26551)
five = c(81.92106,82.16576,83.15675,82.16482,81.56158,81.20468,83.04684,82.48952,82.49812,82.56985)
R.F = c(one,two,three,four,five)
e.mat
boxplot(e.mat)
colMeans(e.mat)
