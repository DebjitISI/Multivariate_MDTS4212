library(readxl)
data= read_excel("/home/datascience/Desktop/Prac_438/Problem set 3_French Food Data.xlsx")
View(data)

x=data[,-1]
x_scaled = scale(x)

cov_mat=cov(x_scaled);cov_mat

e_scaled = eigen(cov_mat);e_scaled

prop_lamda12= (e_scaled$values[1]+e_scaled$values[2])/sum(e_scaled$values);prop_lamda12
prop_lamda1 = (e_scaled$values[1])/sum(e_scaled$values);prop_lamda1
pca_scaled = prcomp(x_scaled);pca_scaled

summary(pca_scaled)

corr_coeff = matrix(NA,nrow=2,ncol=ncol(e_scaled$vectors))

y_scaled = as.matrix(x_scaled)%*%e_scaled$vectors
y_scaled

par(mfrow=c(1,2))
plot(y_scaled)

for (i in 1:2){
  for (j in 1:ncol(e_scaled$vectors)){
    corr_coeff[i,j] = e_scaled$vectors[j,i]*sqrt(e_scaled$values[i])/sqrt(var((x_scaled))[j,j])
  }
}

y_scaled
corr_coeff=t(corr_coeff)
rownames(corr_coeff) = colnames(x_scaled)
plot(corr_coeff[,1],corr_coeff[,2],ylim = c(1,-1),xlim = c(1,-1))
theta=seq(0,2*pi,length.out=100)
x1=cos(theta)
y1=sin(theta)
lines(x1,y1)
text(corr_coeff,labels=rownames(corr_coeff),pos=4,cex=0.7,col='red')
corr_coeff
