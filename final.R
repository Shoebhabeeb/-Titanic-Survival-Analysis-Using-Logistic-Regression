#loading data
titanic.train = read.csv('~/Desktop/Lin-Stat/project/train.csv',  header = T, na.strings = c(''))
titanic.test = read.csv('~/Desktop/Lin-Stat/project/test.csv', header = T, na.strings = c(""))
test.label = read.csv('~/Desktop/Lin-Stat/project/gender_submission.csv', header = T, na.string=c(""))
titanic.test = merge(titanic.test, test.label, by = "PassengerId")
titanic.train$Pclass = as.factor(titanic.train$Pclass)
titanic.test$Pclass = as.factor(titanic.test$Pclass)
#Visualization
str(titanic.train)
str(titanic.test)
library(Amelia)
missmap(titanic.train, main = "Missing Values vs. Observed")
missmap(titanic.test, main = "Missing Values vs. Observed")
titanic.train[which(titanic.train$Age < 1),'Age']
titanic.test[which(titanic.test$Age < 1),'Age']
sapply(titanic.train, function(x) sum(is.na(x)))
sapply(titanic.test, function(x) sum(is.na(x)))
sapply(titanic.train, function(x) length(unique(x)))
sapply(titanic.test, function(x) length(unique(x)))
titanic.train = subset(titanic.train, select = c(2,3,5,6,7,8,10,12))
titanic.test = subset(titanic.test, select = c(2,4,5,6,7,9,11,12))
age = c(titanic.train$Age, titanic.test$Age)
avg.age = mean(age, na.rm = T)
titanic.train$Age[is.na(titanic.train$Age)] = avg.age
titanic.test$Age[is.na(titanic.test$Age)] = avg.age
titanic.train = titanic.train[!is.na(titanic.train$Embarked),]
titanic.test = titanic.test[!is.na(titanic.test$Fare),]
missmap(titanic.train, main = "Missing Values vs. Observed")
missmap(titanic.test, main = "Missing Values vs. Observed")
#Model Training
reg.model = glm(Survived ~., family = binomial(link = 'logit'), data = titanic.train)
summary(reg.model)
confint(reg.model)
#Stepwise 
full<-glm(Survived~Pclass*Sex*Age*SibSp,data=titanic.train,family="binomial")
null<-glm(Survived~1,data=titanic.train,family="binomial")
step(null,scope=list(lower=null,upper=full),direction="both")
reg.model = glm(formula = Survived ~ Sex + Pclass + Age + SibSp + Sex:Pclass + Pclass:SibSp + Pclass:Age + Sex:Age, family = "binomial",data = titanic.train)
summary(reg.model)
confint(reg.model)
exp(coef(reg.model))
exp(cbind(OR=coef(reg.model),confint(reg.model)))
library(aod)
wald.test(b = coef(reg.model), Sigma = vcov(reg.model), Terms = 4:5)
wald.test(b = coef(reg.model), Sigma = vcov(reg.model), Terms = 2:3)
anova(reg.model, test = 'Chisq')
library(pscl)
library(ResourceSelection)
pR2(reg.model)
hoslem.test(reg.model$y,fitted(reg.model),g=10)
cooks.distance<-cooks.distance(reg.model)
which(cooks.distance>1)
library(car)
library(effects)
vif(reg.model)
plot(allEffects(reg.model))
#Evaluation
titanic.predict = predict(reg.model, newdata = subset(titanic.test, select = c(1:7)),
                          type = 'response')
head(titanic.predict)
titanic.predict = ifelse(titanic.predict >0.5, 1, 0)
head(titanic.predict)
misClassifiError = mean(titanic.predict != titanic.test$Survived)
print(paste('Accuracy', 1 - misClassifiError))
library(ROCR)
p = predict(reg.model, newdata = subset(titanic.test, select = c(1:7)),
            type = 'response')
pr = prediction(p, titanic.test$Survived)
prf = performance(pr, measure = 'tpr', x.measure = 'fpr')
plot(prf)
abline(0,1, lwd = 2, lty = 2)
auc = performance(pr, measure = 'auc')
str(auc) 
auc = auc@y.values[[1]]
auc

