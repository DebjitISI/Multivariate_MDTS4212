mu=c(2,4,3)
M=matrix(c(3,1,2,1,4,6,2,6,10),nrow = 3,byrow = T)
M
A=matrix(c(1,2,1,1,1,1,1,-2,-1),nrow = 3,byrow = T)
A

mu_y=A%*%mu;mu_y
D_y=A%*%M%*%t(A);D_y

B=matrix(c(1,1,1,1,-1,1),nrow = 2,byrow = T)
B
mu_z=B%*%mu;mu_z
D_z= B%*%M%*%t(B);D_z

cov_yz=A%*%M%*%t(B);cov_yz

library(matlib)
mu=c(2,4,5)

A=matrix(c(1,4,2,4,5,6,2,6,7),nrow = 3,byrow = T)
A
M=matrix(c(3,1,2,1,4,4,2,4,9),nrow = 3,byrow = T)
M
E=tr(A%*%M) +(t(mu)%*%A%*%mu);E