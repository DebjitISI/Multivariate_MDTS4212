X_i=c(1,1,3,3,4,4,5,6,6,7);X_i
Y_i=c(4,7,9,12,11,12,17,13,18,17);Y_i
plot(X_i,Y_i,type = "b",col="red",xlim = c(0,20),ylim = c(0,20))
summary(X_i)
summary(Y_i)
sd(X_i)
sd(Y_i)

a=c(1,2,1);a#a is 3x1 vector
mu=c(2,4,3)
M=matrix(c(3,1,2,1,4,6,2,6,10),nrow = 3,byrow = T)
M
mu_y=sum(a*mu);mu_y
mu_y=t(a)%*%mu;mu_y
var_y=t(a)%*%M%*%a;var_y
