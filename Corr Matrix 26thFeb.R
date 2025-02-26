A = matrix(c(1,0,1,1,3,2,1,-2,1),nrow=3,byrow=T)
sigma = matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,byrow=T) # dispersion matrix of Y
disp = A %*% sigma %*% t(A);disp #dispersion matrix of X
corr = cov2cor(disp);corr #correlaton matrix of x




