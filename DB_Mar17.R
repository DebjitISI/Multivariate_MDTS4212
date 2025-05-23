library(readxl)
data = read_xlsx('/home/datascience/Downloads/otter-mandible-data (1).xlsx')
set.seed(1)
?sample
train = sample(1:nrow(data),size = round(0.8*nrow(data)))
train_sample= data[train,]
test_sample  = data[-train,]
attach(data)
library(klaR)
#library(tidyverse)
library(MASS)
library(partykit)
lda_otter = lda(species~.,train_sample)
pred = predict(lda_otter,test_sample)
startsWith(train_sample$species,'A')
plot(lda_otter, col = ifelse(startsWith(train_sample$species,'A'),'red',ifelse(startsWith(train_sample$species,'E'),'orange',ifelse(startsWith(train_sample$species,'L'),'green','black'))),main="LDA based on species")

View(test_sample)

table(pred$class,test_sample$species)
test_sample$spe
?plot

partimat(as.factor(species) ~ ., data=test_sample, method="lda")
?partimat
unique(train_sample$species)
cov(data[,-1])
cov(data[startsWith(data$species,'A'),-1])
cov(data[startsWith(data$species,'E'),-1])
cov(data[startsWith(data$species,'L'),-1])
cov(data[startsWith(data$species,'P'),-1])


