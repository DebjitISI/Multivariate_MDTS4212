n=10
Sum=0
Prob= c(0.3,0.4,0.3)

for(x1 in 0:n) {
  for(x2 in 0:(n-x1)){
    f_x=dmultinom(c(x1,x2,n-x1-x2),size=n,prob=Prob)
    print(f_x)
    Sum= Sum+f_x
  }
}

Sum

# Deriving Marginal probability
X1= c(0,0,0,0,0,0,1,1,1,2)
X2= c(0,0,0,1,1,2,0,0,1,0)
X3= c(0,1,2,0,1,0,0,1,0,0)
P= c(0.04,0.08,0.04,0.16,0.16,0.16,0.08,0.08,0.16,0.04)
Data= data.frame(X1,X2,X3,P)
aggregate(Data$P, by= list(X1,X2),sum)
aggregate(Data$P, by= list(X1),sum)
aggregate(Data$P, by= list(X2),sum)
