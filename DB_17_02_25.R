library(readxl)
data = read_excel("C:\\Users\\DS-3\\Downloads\\Problem set 3_French Food Data.xlsx");data
matrix_data= (scale(data[,-1]));matrix_data
x=as.matrix(matrix_data)
main=x%*%t(x);main
print("The Variance-Covariance Matrix:")
var_cov_matrix = cov(main);var_cov_matrix

print("The Correlation matrix")
correlation_matrix = cor(main);correlation_matrix

eigen=eigen(main);eigen

eigen_val=eigen$values;eigen_val

eigen_vec=eigen$vectors;eigen_vec

numeric_matrix=as.matrix(main);numeric_matrix

eigen_vec_max= eigen_vec[,1:2];eigen_vec_max



z1=t(x)%*%eigen_vec[,1];z1

z2=t(x)%*%eigen_vec[,2];z2

plot(z1,z2,col="blue",main = "Representation of family expendeture based on food ")

abline(h=0)
abline(v=0)

text(z1,z2,label=c("bread","vegetables","fruits","meat","poultry","milk","wine"))






library(Matrix)
x= matrix(c(1,-2,0,-2,5,0,0,0,2),byrow = T,nrow=3)
x
#a
E=eigen(x)
eigen_vec=E$vectors;eigen_vec
eigen_val=E$values;eigen_val

#b
pca=eigen_vec;pca
#c
pca_var=var(pca);pca
#d
