library(readxl)
data = read_xlsx('/home/datascience/Downloads/otter-mandible-data (1).xlsx')
#data = read_xlsx('C:/Users/DEBJIT/Downloads/otter-mandible-data (1).xlsx')
set.seed(1)
?sample
train = sample(1:nrow(data),size = round(0.8*nrow(data)))
train_sample= data[train,]
test_sample  = data[-train,]
attach(data)
library(klaR)
library(tidyverse)
library(MASS)
lda_otter = lda(species~.,train_sample)
pred = predict(lda_otter,test_sample)
startsWith(train_sample$species,'A')
plot(lda_otter, col = ifelse(startsWith(train_sample$species,'A'),'red',ifelse(startsWith(train_sample$species,'E'),'orange',ifelse(startsWith(train_sample$species,'L'),'green','black'))),main="LDA based on species")

View(test_sample)

table(pred$class,test_sample$species)
test_sample$spe
?plot

#partimat(species ~ ., data=train_sample, method="lda")
#?partimat
#unique(train_sample$species)
lda.data = data.frame(pred$x, species = test_sample$species)
library(ggplot2)

ggplot(lda.data, aes(x = LD1, y = LD2, color = species)) +
  geom_point(size = 3) + labs(title = "LDA Plot", x = "LD1", y = "LD2") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue","green","orange"))
