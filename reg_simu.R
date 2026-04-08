n<- 5
b1<- 5
b2<- 4
sigma<- 3
R<- 50000

x<- runif(n)

mu<- b1 + b2*x

m<- c()

for(i in 1:R)
{
  erro<- rnorm(n,0,sigma) #runif(n,-5,5) #
  
  y<- mu + erro
  
  fit<- lm(y~x)
  
  m<- rbind(m,fit$coefficients)
}
esp<- colMeans(m)
vies<- esp-c(b1,b2)
vr<- 100*vies/c(b1,b2)

print(rbind(c(b1,b2),esp,vies,vr))

