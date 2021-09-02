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
library(gridExtra)

bank = read.csv(file.choose())

###### EDA ######
summary(bank[,c(1:6)])

# 결측치
colSums(is.na(bank)) # 결측치 없음

# 상자그림
par(mfrow=c(1,3))
boxplot(bank[,1])
boxplot(bank[,2])
boxplot(bank[,3])
boxplot(bank[,4])
boxplot(bank[,5])
boxplot(bank[,6])

# 이상치 확인
sum(bank[,1]<(quantile(bank[,1])[2]-1.5*((quantile(bank[,1])[4]-quantile(bank[,1])[2]))))
sum(bank[,1]>(quantile(bank[,1])[4]+1.5*((quantile(bank[,1])[4]-quantile(bank[,1])[2])))) # 171
sum(bank[,2]<(quantile(bank[,2])[2]-1.5*((quantile(bank[,2])[4]-quantile(bank[,2])[2])))) # 4
sum(bank[,2]>(quantile(bank[,2])[4]+1.5*((quantile(bank[,2])[4]-quantile(bank[,2])[2])))) # 1051
sum(bank[,3]<(quantile(bank[,3])[2]-1.5*((quantile(bank[,3])[4]-quantile(bank[,3])[2]))))
sum(bank[,3]>(quantile(bank[,3])[4]+1.5*((quantile(bank[,3])[4]-quantile(bank[,3])[2])))) # 636
sum(bank[,4]<(quantile(bank[,4])[2]-1.5*((quantile(bank[,4])[4]-quantile(bank[,4])[2])))) 
sum(bank[,4]>(quantile(bank[,4])[4]+1.5*((quantile(bank[,4])[4]-quantile(bank[,4])[2])))) # 601
sum(bank[,5]<(quantile(bank[,5])[2]-1.5*((quantile(bank[,5])[4]-quantile(bank[,5])[2]))))
sum(bank[,5]>(quantile(bank[,5])[4]+1.5*((quantile(bank[,5])[4]-quantile(bank[,5])[2])))) #2750
sum(bank[,6]<(quantile(bank[,6])[2]-1.5*((quantile(bank[,6])[4]-quantile(bank[,6])[2]))))
sum(bank[,6]>(quantile(bank[,6])[4]+1.5*((quantile(bank[,6])[4]-quantile(bank[,6])[2])))) #1258

# 기술통계량
summary(bank[,1])
summary(bank[,2])
summary(bank[,3])
summary(bank[,4])
summary(bank[,5])
summary(bank[,6])
sd(bank[,1])
sd(bank[,2])
sd(bank[,3])
sd(bank[,4])
sd(bank[,5])
sd(bank[,6])

# 종속변수와 나이의 관계
bankage = bank[,1]
bankage[bankage<30]="18~29세"
bankage[bankage<40 & bankage>=30]="30~39세"
bankage[bankage<50 & bankage>=40]="40~49세"
bankage[bankage<60 & bankage>=50]="50~59세"
bankage[bankage<70 & bankage>=60]="60~69세"
bankage[bankage>=70]="70세 이상"
one = ggplot(data = bank) + geom_bar(mapping = aes(x = bankage, fill = as.factor(bank[,16])),position = "dodge") + labs(x = "연령", title = "연령별 예금 가입 현황") + scale_fill_discrete(name = "예금가입현황")

# 종속변수와 잔고의 관계
bankbal = bank[,2]
bankbal = ifelse(bankbal<0, "채무자",ifelse(bankbal<122 & bankbal>=0,"1구간",ifelse(bankbal<550 & bankbal>=122,"2구간",ifelse(bankbal<1708 & bankbal>=550,"3구간","4구간"))))
two = ggplot(data = bank) + geom_bar(mapping = aes(x = bankbal, fill = as.factor(bank[,16])),position = "dodge") + labs(x = "잔고", title = "잔고구간별 예금 가입 현황") + scale_fill_discrete(name = "예금가입현황") + scale_x_discrete(limits = c("채무자","1구간","2구간","3구간","4구간"))

# 종속변수와 마지막 연락의 관계
bankpdays = bank[,5]
bankpdays = ifelse(bankpdays==-1,"연락한 적 없음",ifelse(bankpdays>0 & bankpdays<=30,"1개월 이내",ifelse(bankpdays>30 & bankpdays<=90,"3개월 이내",ifelse(bankpdays>90 & bankpdays<=180,"6개월 이내",ifelse(bankpdays>180 & bankpdays<=365,"1년 이내","1년 이상")))))
three = ggplot(data = bank) + geom_bar(mapping = aes(x = bankpdays, fill = as.factor(bank[,16])),position = "dodge") + labs(x = "마지막 연락시기", title = "마지막 연락시기별 예금 가입 현황") + scale_fill_discrete(name = "예금가입현황")+scale_x_discrete(limits = c("연락한 적 없음","1개월 이내","3개월 이내","6개월 이내","1년 이내","1년 이상"))

# 종속변수와 직업의 관계
bankjob = bank[,7]
four = ggplot(data = bank) + geom_bar(mapping = aes(x = bankjob, fill = as.factor(bank[,16])),position = "dodge") + labs(x = "직업군", title = "직업군별 예금 가입 현황") + scale_fill_discrete(name = "예금가입현황")

# 종속변수와 혼인여부의 관계
bankmar = bank[,8]
five = ggplot(data = bank) + geom_bar(mapping = aes(x = bankmar, fill = as.factor(bank[,16])),position = "dodge") + labs(x = "혼인여부", title = "혼인여부별 예금 가입 현황") + scale_fill_discrete(name = "예금가입현황")

# 종속변수와 학력의 관계
bankstu = bank[,9]
six = ggplot(data = bank) + geom_bar(mapping = aes(x = bankstu, fill = as.factor(bank[,16])),position = "dodge") + labs(x = "학력", title = "학력별 예금 가입 현황") + scale_fill_discrete(name = "예금가입현황")

# 종속변수와 자가보유의 관계
bankhou = bank[,11]
seven = ggplot(data = bank) + geom_bar(mapping = aes(x = bankhou, fill = as.factor(bank[,16])),position = "dodge") + labs(x = "자가보유여부", title = "자가보유여부별 예금 가입 현황") + scale_fill_discrete(name = "예금가입현황")

# 종속변수와 대출의 관계
bankloan = bank[,12]
eight = ggplot(data = bank) + geom_bar(mapping = aes(x = bankloan, fill = as.factor(bank[,16])),position = "dodge") + labs(x = "대출여부", title = "대출여부별 예금 가입 현황") + scale_fill_discrete(name = "예금가입현황")

grid.arrange(one,two,three,ncol=3)
grid.arrange(four,five,six,ncol=3)
grid.arrange(seven,eight,ncol=2)

# 연속형 변수 상관관계 확인
corrplot(cor(bank[,1:6]),method='color') # pdays, previous가 0.5정도의 상관관계를 보임
cor(bank[,1:6])

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

# correlation
corrplot(cor(bank[,-34]),method='color')
cor(bank)

# pair plot
pairs(bank[,1:6])

# training set, test set 나누기 yes에서 4700개 no에서 5300개 추출
deposity = bank[bank[,34]==0,]
depositn = bank[bank[,34]==1,]

# training set
tdy = deposity[1:4700,]
tdn = depositn[1:5300,]
bank.df = rbind(tdy,tdn)
x.mat = as.matrix(bank.df[,-34])
y.vec = as.vector(bank.df[,34])
xy.df = data.frame(x.mat,y.vec)

# test set
testdy = deposity[4701:5289,]
testdn = depositn[5301:5873,]
tbank.df = rbind(testdy,testdn)
nx.mat = as.matrix(tbank.df[,-34])
ny.vec = as.vector(tbank.df[,34])
nxy.df = data.frame(nx.mat,ny.vec)

# ready
m.vec = c("logit","ridge","lasso")
b.mat = matrix(NA,nrow=1+ncol(x.mat),ncol=length(m.vec))
colnames(b.mat) = m.vec
rownames(b.mat) = c("intercept",colnames(x.mat))
cv.id = cv.index.fun(y.vec,k.val=10)

# logistic regression
log.fit = glm(y.vec~.,family=binomial,data=xy.df)
summary(log.fit)
b.mat[,"logit"] = coef(log.fit)

# ridge penalty
ridge.fit = cv.glmnet(x.mat,y.vec,family="binomial",alpha=0,foldid=cv.id,type.measure="class")
plot(ridge.fit$cvm)
opt = which.min(ridge.fit$cvm)
b.mat[,"ridge"] = coef(ridge.fit$glmnet.fit)[,opt]

# lasso penalty
lasso.fit = cv.glmnet(x.mat,y.vec,family="binomial",alpha=1,foldid=cv.id,type.measure="class")
plot(lasso.fit$cvm)
opt = which.min(lasso.fit$cvm)
b.mat[,"lasso"] = coef(lasso.fit$glmnet.fit)[,opt]

# assessment
ass = glm.ass.fun(ny.vec,nx.mat,b.mat,mod="binomial")$ass
ass

# decision tree
tree = rpart(y.vec~.,data=xy.df,method="class")
rpart.plot(tree)
pruned.tree = prune(tree,cp=0.03)
rpart.plot(pruned.tree)
prunedpred.tree = predict(pruned.tree,nxy.df,type="class")
auc.prtr = sum(prunedpred.tree==ny.vec)/nrow(nxy.df)*100 # 78%

pred.tree = predict(tree,nxy.df,type="class")
auc.tr = sum(pred.tree==ny.vec)/nrow(nxy.df)*100 # 79%

# generalized gradient boosting
set.seed(1234)
fit = gbm(y.vec~.,data=xy.df,distribution = "adaboost",n.trees=1000,cv.fold=10)
opt = gbm.perf(fit,method="cv",plot.it=FALSE)
pred.b = predict(fit,newdata=nxy.df[,-34],n.trees=opt)>0
auc.b = sum(pred.b==ny.vec)/nrow(nxy.df)*100 # 81%
summary(fit)
fit

# random forest
yy.vec = y.vec[y.vec==0]
nny.vec = y.vec[y.vec==1]
yy.vec[1:4700] = "yes"
nny.vec[1:5300] = "no"
y.vec = c(yy.vec,nny.vec)
y.vec = as.factor(y.vec)
xy.df = data.frame(x.mat,y.vec)
bank.rf = randomForest(y.vec~.,xy.df,mtry=sqrt(33),importance=T,ntree=300)
pred.rf = predict(bank.rf,newdata = nxy.df,typer="class")
ny.vec[ny.vec==0] = "yes"
ny.vec[ny.vec==1] = "no"
auc.rf = sum(pred.rf==ny.vec)/nrow(nxy.df)*100 # 83%
names(bank.rf)
bank.rf$importance[,4]


ny.vec = as.vector(tbank.df[,34]) # 변경한 test y vector 다시 돌려놓기

# acuracy 비교
auc = c(ass[,10],auc.tr,auc.prtr,auc.rf,auc.b)
names(auc)[4:7]=c("tree","prtree","random","boosting")
auc












