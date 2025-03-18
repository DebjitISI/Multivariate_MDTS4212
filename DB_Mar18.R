data(data = 'bank',package = 'gclus')
attach(bank)
library(klaR)
#library(tidyverse)
library(MASS)
View(bank)
??bank
train = sample(1:nrow(data),size = round(0.8*nrow(data)))
train_sample= bank[train,]
test_sample  = bank[-train,]
?lda
lda_bank = lda(as.factor(Status)~.,data = train_sample,prior = c(0.7,0.3))
lda_bank

?qda
qda_bank = qda(as.factor(Status)~.,data = train_sample,prior = c(0.7,0.3))
qda_bank

pred = predict(lda_bank,test_sample)
table(pred$class,test_sample$Status)

partimat(as.factor(Status) ~ ., data=train_sample, method="lda")

partimat(as.factor(Status) ~ ., data=train_sample, method="qda")
