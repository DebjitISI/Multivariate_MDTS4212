
#Q2

library(gclus)
data("bank")
attach(bank)
plot(bank[,-1],main="Pair Plot")
par(mfrow=c(3,2))
boxplot(Left[which(Status==0)],Left[which(Status==1)],names = c("Real","Fake"),ylab="Left",xlab="types of note")
boxplot(Right[which(Status==0)],Right[which(Status==1)],names = c("Real","Fake"),ylab="Right",xlab="types of note")
boxplot(Bottom[which(Status==0)],Bottom[which(Status==1)],names = c("Real","Fake"),ylab="Bottom",xlab="types of note")
boxplot(Top[which(Status==0)],Top[which(Status==1)],names = c("Real","Fake"),ylab="Top",xlab="types of note")
boxplot(Diagonal[which(Status==0)],Diagonal[which(Status==1)],names = c("Real","Fake"),ylab="Diagonal",xlab="types of note")

?boxplot
par(mfrow=c(1,1))
#boxplot(Left[which(Status==0)],Left[which(Status==1)],Right[which(Status==0)],Bottom[which(Status==0)],Top[which(Status==0)],Diagonal[which(Status==0)],names=c("Left Real","Left Fake","Right","Bottom","Top","Diagonal"))
data=bank
real_note= data[Status==0,-1]
fake_note= data[Status==1,-1]
real_note
par(mfrow= c(3,4))
for( i in 1:4){
  boxplot(real_note[,i],main=paste(colnames(real_note[i])))
  boxplot(fake_note[,i],main=paste(colnames(fake_note[i])))
}

plot(data[,-1])

#2

data = data[,-1]

var_cov_matrix= cov(data);var_cov_matrix
corr_matrix = cor(data);corr_matrix
x= as.matrix(var_cov_matrix)
e=eigen(x)
e$values[1]/sum(e$values)
e$values[2]/sum(e$values)
e$values[3]/sum(e$values)
pca_1 = prcomp(data);pca_1
summary(pca_1)
readings_1 = pca_1$rotation
std_dev_1 = pca_1$sdev
sqrt(e$values/std_dev_1)
corr_mat_pca_1= readings_1%*%diag(sqrt(e$values/std_dev_1));corr_mat_pca_1

#scaled data
scaled_data= scale(data);scaled_data
x_scaled= as.matrix(cov(scaled_data))
e_scaled= eigen(x_scaled)
e_scaled
e_scaled$values[1]/sum(e_scaled$values)
pca_2 = prcomp(scaled_data);pca_2
cov(scaled_data)
pca_2
e_scaled$values[1]/sum(e_scaled$values)
e_scaled$values[2]/sum(e_scaled$values)
e_scaled$values[3]/sum(e_scaled$values)
summary(pca_2)
readings = pca_2$rotation
std_dev = pca_2$sdev

corr_mat_pca_2= readings%*%diag(sqrt(e_scaled$values/std_dev));corr_mat_pca_2

par(mfrow=c(2,2))
#SCALED DATA
y_scaled = as.matrix(scaled_data)%*%e_scaled$vectors
y_scaled
tb=data.frame(y[,1],y[,2])
tb
theta=seq(0,2*pi,length.out=100)
x1=cos(theta)
y1=sin(theta)
plot(corr_mat_pca_2,ylim = c(1,-1),xlim = c(1,-1),main="Standardised importance of Xi's in Yj's")
lines(x1,y1)
text(corr_mat_pca_2,labels=rownames(corr_mat_pca_2),pos=4,cex=0.7,col='red')
plot(y_scaled,col=ifelse(Status==1,'red','blue'),main="Standardised Clustering based on Fake(Red) and Real(blue) Note")

#UNSCALED DATA
y = as.matrix(data)%*%e$vectors
y
tb=data.frame(y[,1],y[,2])
tb
theta=seq(0,2*pi,length.out=100)
x1=cos(theta)
y1=sin(theta)
plot(corr_mat_pca_1,ylim = c(1,-1),xlim = c(1,-1),main="Non_Standardised importance of Xi's in Yj's")
lines(x1,y1)
text(corr_mat_pca_1,labels=rownames(corr_mat_pca_1),pos=4,cex=0.7,col='red')
plot(y,col=ifelse(Status==1,'red','blue'),main="Non-Standardised Clustering based on Fake(Red) and Real(blue) Note")

