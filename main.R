library(stringr)
library(dplyr)
library(tidyr)
library(randomForest)

train = read.csv("E:\\R\\train.csv")
test = read.csv("E:\\R\\test.csv")
test$Survived = 0

boxplot(split(train$Survived,train$Pclass))
Pclass3_prob = mean(train$Survived[train$Pclass ==3])

boxplot(split(train$Survived,train$Sex))

SexM_prob = mean(train$Survived[train$Sex == "male"])
#test$Survived[test$Sex == "female"] = 1

boxplot(split(train$Survived,train$Age))

boxplot(split(train$Survived,train$SibSp))

SibSpGreaterorEq4 = train[train$SibSp >= 4,]
SibSpGreater4_prob = mean(train$Survived[train$SibSp >= 4])
#test$Survived[test$SibSp >= 4] = 0

boxplot(split(train$Survived,train$Parch))

ParchGreaterorEq4 = train[train$Parch >= 4,]
ParchGreater4_prob = mean(train$Survived[train$Parch >= 4])
#test$Survived[test$Parch >= 4] = 0

boxplot(split(train$Survived,train$Fare))

FareGreaterorEq4 = train[train$Fare < 7,]
FareGreater4_prob = mean(train$Survived[train$Fare < 7])
#test$Survived[test$Fare < 7] = 0

boxplot(split(train$Survived,train$Embarked)) 
table(train$Embarked)
#test$Survived[test$Embarked==""] = 1


miss_in_name = train %>% filter(str_detect(Name, "Miss"))
miss_1Pclass = miss_in_name[miss_in_name$Pclass ==1,]
miss_1Pclass_prob = mean(miss_1Pclass$Survived)

train$miss_1Pclass = 0
train$miss_1Pclass[train$PassengerId %in% miss_1Pclass$PassengerId & train$Pclass ==1] = 1

test_miss_in_name = test %>% filter(str_detect(Name, "Miss"))
test_miss_1Pclass = test_miss_in_name[test_miss_in_name$Pclass ==1,]
test$miss_1Pclass = 0
test$miss_1Pclass[test$PassengerId %in% test_miss_in_name$PassengerId & test$Pclass ==1] = 1
#test$Survived[test$PassengerId %in% test_miss_in_name$PassengerId & test$Pclass ==1] = 1

young_prob = mean(train$Survived[drop_na(train)$Age<18])
train$young = 0
train$young[drop_na(train)$Age<18] = 1
test$young = 0
test$young[drop_na(test)$Age<18] = 1

retire_prob = mean(train$Survived[drop_na(train)$Age>=64])
train$retire = 0
train$retire[drop_na(train)$Age>=64] = 1
test$retire = 0
test$retire[drop_na(test)$Age>=64] = 1

duplicated_ticket_prob = mean(train$Survived[duplicated(train$Ticket)])
train$duplicated_ticket = 0
train$duplicated_ticket[duplicated(train$Ticket)] = 1
test$duplicated_ticket = 0
test$duplicated_ticket[duplicated(test$Ticket)] = 1

no_cabin_prob = mean(train$Survived[train$Cabin == ""])
train$no_cabin = 0
train$no_cabin[train$Cabin == ""] = 1
test$no_cabin = 0
test$no_cabin[test$Cabin == ""] = 1

test$Fare[is.na(test$Fare)] = 0

smp_size <- floor(0.7 * nrow(train))
train_ind <- sample(seq_len(nrow(train)), size = smp_size)
train2 <- train[train_ind, ]
test2 <- train[-train_ind, ]

fit <- rpart(Survived ~ Pclass + Sex + miss_1Pclass + SibSp + Parch + Fare +
               Embarked + duplicated_ticket + young + retire + no_cabin,
             data=train2,
             method="class")
plot(fit)
text(fit)
fancyRpartPlot(fit)

Prediction = predict(fit, test2, type = "class")
score_dt = sum(Prediction == test2$Survived)/length(Prediction)

fit2 <- randomForest(as.factor(Survived) ~ Pclass + Sex + miss_1Pclass + SibSp + Parch + Fare +
               Embarked + duplicated_ticket + young + retire + no_cabin,
             data=train2,
             importance=TRUE,
             ntree=2000)

Prediction = predict(fit2, test2, type = "class")
score_rF = sum(Prediction == test2$Survived)/length(Prediction)


fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + miss_1Pclass + SibSp + Parch + Fare +
                       Embarked + duplicated_ticket + young + retire + no_cabin,
                     data=train,
                     importance=TRUE,
                     ntree=2000)

Prediction = predict(fit, test, type = "class")

submit = data.frame(test$PassengerId, c(Prediction))
names(submit) = c("PassengerId","Survived")
write.csv(submit, file="submit.csv", row.names = F)
#prop.table(table(train$Pclass, train$Survived),1)
#aggregate(Survived ~ Child + Sex, train=train, FUN=function(x) {sum(x)/length(x)})