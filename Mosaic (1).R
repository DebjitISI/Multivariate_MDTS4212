# Load dataset
data(HairEyeColor)


attach(as.data.frame(HairEyeColor))

library(vcd)
mosaic(~Eye + Hair + Sex,data= HairEyeColor,main = "Mosaic of HairEyeColor Data",highlighting = "Sex",highlighting_fill = c("pink","orange"),direction=c('h','v','h'))
?mosaic()

View(HairEyeColor)
labs
labs = round(prop.table(HairEyeColor),3)


str(HairEyeColor)
mosaic(HairEyeColor,main = "Mosaic plot of Titanic Data", highlighting = 'Sex', highlighting_fill = c('pink','orange'),pop=F,direction = c('v','h','v','h'))
labeling_cells(text = labs,margin=0)(HairEyeColor)


data("Titanic")
attach(Titanic)
View(Titanic)

labs = round(prop.table(Titanic),3)

labs
str(Titanic)
mosaic(Titanic,main = "Mosaic plot of Titanic Data", highlighting = 'Sex', highlighting_fill = c('pink','orange','skyblue'))
labeling_cells(text = labs,margin=0)(Titanic)


View(iris)
data = iris[,-5]
plot(data)
?plot
pairs(data,col = iris$Species,oma = c(3,3,8,15),main =  'Pairwise Scatter plot of Iris Dataset')
par(xpd=T)
legend('bottomright',fill = unique(iris$Species),legend=c(levels(iris$Species)))


install.packages('GGally')
library(GGally)
ggpairs(iris,title = "Scatterplot Matrix",ggplot2::aes(colour=iris$Species,alpha=0.6))
??ggpairs
