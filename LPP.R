LPP<-function(a,b,c,d,e,f,g,h){
  max(a+b+c+d+h,a+b+c+g+h,a+b+f+g+h,a+e+f+g+h)
}

#bottom row is abcd top row is efgh left to right

a<-rexp(n=100000,rate=1)
b<-rexp(n=100000,rate=1)
c<-rexp(n=100000,rate=1)
d<-rexp(n=100000,rate=1)
e<-rexp(n=100000,rate=1) #repeated one
f<-rexp(n=100000,rate=2)
g<-rexp(n=100000,rate=2)

test1=list()
test2=list()
test3=list()


for (i in 1:100000){
  test1[i]=LPP(a[i],b[i],f[i],e[i],c[i],d[i],e[i],g[i])
  test2[i]=LPP(a[i],f[i],e[i],b[i],c[i],e[i],g[i],d[i])
}
mean(unlist(test1))
mean(unlist(test2))


qqplot(unlist(test1),unlist(test2))


#non-homogeneous

u<-1.1
v<-2.7
x<-0.5
y<-0.9
z<-0.84

a<-rexp(n=100000,rate=u*x)
b<-rexp(n=100000,rate=u*y)
c<-rexp(n=100000,rate=v*x)
d<-rexp(n=100000,rate=v*y)
e<-rexp(n=100000,rate=u*v) #repeated one
f<-rexp(n=100000,rate=z*u)
g<-rexp(n=100000,rate=z*v)

test1=list()
test2=list()

for (i in 1:100000){
  test1[i]=LPP(a[i],b[i],f[i],e[i],c[i],d[i],e[i],g[i])
  test2[i]=LPP(a[i],f[i],e[i],b[i],c[i],e[i],g[i],d[i])
}
mean(unlist(test1))
mean(unlist(test2))
ks.test(unlist(test1),unlist(test2))


qqplot(unlist(test1),unlist(test2))


a<-runif(n=10000)
b<-runif(n=10000)
c<-runif(n=10000)
d<-runif(n=10000)
e<-runif(n=10000) #repeated one
f<-runif(n=10000, max=0.5)
g<-runif(n=10000, max=0.5)

test1=list()
test2=list()

for (i in 1:10000){
  test1[i]=LPP(a[i],b[i],f[i],e[i],c[i],d[i],e[i],g[i])
  test2[i]=LPP(a[i],f[i],e[i],b[i],c[i],e[i],g[i],d[i])
}
mean(unlist(test1))
mean(unlist(test2))


qqplot(unlist(test1),unlist(test2))



a<-runif(n=10000, max=100)
b<-runif(n=10000, max=100)
c<-runif(n=10000, max=100)
d<-runif(n=10000, max=100)
e<-runif(n=10000, max=100) #repeated one
f<-runif(n=10000, max=110)
g<-runif(n=10000, max=110)

test1=list()
test2=list()

for (i in 1:10000){
  test1[i]=LPP(a[i],b[i],f[i],e[i],c[i],d[i],e[i],g[i])
  test2[i]=LPP(a[i],f[i],e[i],b[i],c[i],e[i],g[i],d[i])
}
mean(unlist(test1))
mean(unlist(test2))


qqplot(unlist(test1),unlist(test2))



a<-rnorm(n=100000)
b<-rnorm(n=100000)
c<-rnorm(n=100000)
d<-rnorm(n=100000)
e<-rnorm(n=100000) #repeated one
f<-rnorm(n=100000, 2,0.5)
g<-rnorm(n=100000, 2,0.5)

test1=list()
test2=list()

for (i in 1:100000){
  test1[i]=LPP(a[i],b[i],f[i],e[i],c[i],d[i],e[i],g[i])
  test2[i]=LPP(a[i],f[i],e[i],b[i],c[i],e[i],g[i],d[i])
}
mean(unlist(test1))
mean(unlist(test2))


qqplot(unlist(test1),unlist(test2))


a<-rpois(n=100000, lambda=1)
b<-rpois(n=100000, lambda=1)
c<-rpois(n=100000, lambda=1)
d<-rpois(n=100000, lambda=1)
e<-rpois(n=100000, lambda=1) #repeated one
f<-rpois(n=100000, lambda=2)
g<-rpois(n=100000, lambda=2)

test1=list()
test2=list()

for (i in 1:10000){
  test1[i]=LPP(a[i],b[i],f[i],e[i],c[i],d[i],e[i],g[i])
  test2[i]=LPP(a[i],f[i],e[i],b[i],c[i],e[i],g[i],d[i])
}
mean(unlist(test1))
mean(unlist(test2))


qqplot(unlist(test1),unlist(test2))


#Testing for log gamma polymer?
#stupid test
library(invgamma)
Zbad<-function(a,b,c,d,e,f,g,h){
  log(a*b*c*d*h+a*b*c*g*h+a*b*f*g*h+a*e*f*g*h)
}
a<-rinvgamma(n=100000,shape=1,rate=1)
b<-rinvgamma(n=100000,shape=1,rate=1)
c<-rinvgamma(n=100000,shape=1,rate=1)
d<-rinvgamma(n=100000,shape=1,rate=1)
e<-rinvgamma(n=100000,shape=1,rate=1) #repeated one
f<-rinvgamma(n=100000,shape=2,rate=1)/2
g<-rinvgamma(n=100000,shape=2,rate=1)/2

test1=list()
test2=list()

for (i in 1:100000){
  test1[i]=Zbad(a[i],b[i],f[i],e[i],c[i],d[i],e[i],g[i])
  test2[i]=Zbad(a[i],f[i],e[i],b[i],c[i],e[i],g[i],d[i])
}
mean(unlist(test1))
mean(unlist(test2))


qqplot(unlist(test1),unlist(test2))
ks.test(unlist(test1),unlist(test2))




library(invgamma)

#generate all up right paths x->y
urpaths<-function(x,y){
  v<-y[1]-x[1]
  h<-y[2]-x[2]
  combn(1:(v+h),v, function(z) lapply(1:(v+h+1),function(a)x+c(length(z[z<a]),a-1-length(z[z<a]))))
}

#Partition function for x->y in terms of random environment M
Z<-function(M,S){
  log(sum(apply(S,2,function(p) prod(unlist(lapply(p,function(z)M[z[1],z[2]]))))))
}

#slow but easier
renv_one<-function(L,a,b){
  out<-matrix(0,L,L)
  for(i in 1:L){
    for(j in 1:L){
      if(i==j){
        out[i,j]<-rinvgamma(n=1,shape=b,rate=1)/2
      } else if(i<j){
        out[i,j]<-rinvgamma(n=1,shape=a,rate=1)
      } else{
        out[i,j]<-out[j,i]
      }
    }
  }
  out
}

renv<-function(L,n,a,b){
  lapply(1:n,function(x)renv_one(L,a,b))
}

rand_env<-renv(10,10000,1,2)

P1<-urpaths(c(1,1),c(4,4))
P2<-urpaths(c(1,2),c(8,3))
Q1<-urpaths(c(1,2),c(4,5))
P3<-urpaths(c(1,2),c(5,3))

test1<-lapply(rand_env,function(x)c(Z(x,P1),Z(x,P2),Z(x,Q1),Z(x,P1)*Z(x,P2),Z(x,P2)*Z(x,Q1)))

mean(unlist(lapply(test1,function(x)x[1])))
mean(unlist(lapply(test1,function(x)x[2])))
mean(unlist(lapply(test1,function(x)x[3])))
mean(unlist(lapply(test1,function(x)x[4])))
mean(unlist(lapply(test1,function(x)x[5])))



#Test two boundaries
#slow but easier
renv_two_bd_one<-function(L,a,b,c,m){
  out<-matrix(0,L,L)
  for(i in 1:L){
    for(j in 1:L){
      if((i-j)==0){
        out[i,j]<-rinvgamma(n=1,shape=b,rate=1)/2
      }
      else if((i-j)==m){
        out[i,j]<-rinvgamma(n=1,shape=c,rate=1)/2
      }
      else if(i<j&&i>j-m){
        out[i,j]<-rinvgamma(n=1,shape=a,rate=1)
      }
      else if(i>j&&j>i-m){
        out[i,j]<-out[j,i]
      }
    }
  }
  for(i in 1:L){
    for(j in 1:L){
      if((i-j)%%(2*m)<=m){
        k<-((i-j)-(i-j)%%(2*m))/2/m
        out[i,j]<-out[i-k*m,j+k*m]
      }
      else if((i-j)%%(2*m)>=m){
        k<-((i-j)-(i-j)%%(2*m))/2/m+1
        out[i,j]<-out[i-k*m,j+k*m]
      }
    }
  }
  
  out
}

renv_two_bd<-function(L,n,a,b,c,m){
  lapply(1:n,function(x)renv_two_bd_one(L,a,b,c,m))
}

rand_env<-renv_two_bd(10,100000,1,2,2,3)
P1<-urpaths(c(1,1),c(4,3))
P2<-urpaths(c(1,2),c(8,3))
Q1<-urpaths(c(1,2),c(4,4))
P3<-urpaths(c(1,2),c(5,3))


test1<-lapply(rand_env,function(x)c(Z(x,P1),Z(x,Q1),Z(x,P1)*Z(x,P2)*Z(x,P3),Z(x,P2)*Z(x,Q1)*Z(x,P3),Z(x,P1)*Z(x,P2),Z(x,P2)*Z(x,Q1)))

mean(unlist(lapply(test1,function(x)x[1])))
mean(unlist(lapply(test1,function(x)x[2])))
mean(unlist(lapply(test1,function(x)x[3])))
mean(unlist(lapply(test1,function(x)x[4])))
mean(unlist(lapply(test1,function(x)x[5])))
mean(unlist(lapply(test1,function(x)x[6])))

ks.test(unlist(lapply(test1,function(x)x[1])),unlist(lapply(test1,function(x)x[2])))
ks.test(unlist(lapply(test1,function(x)x[3])),unlist(lapply(test1,function(x)x[4])))
ks.test(unlist(lapply(test1,function(x)x[5])),unlist(lapply(test1,function(x)x[6])))



#test periodic environment
#slow but easier
renv_periodic_one<-function(L,a,m){
  out<-matrix(0,L,L)
  for(i in 1:L){
    for(j in 1:L){
      if(i<=j+m&&i>j-m){
        out[i,j]<-rinvgamma(n=1,shape=a,rate=1)
      }
    }
  }
  for(i in 1:L){
    for(j in 1:L){
      if((i-j)%%(2*m)<=m){
        k<-((i-j)-(i-j)%%(2*m))/2/m
        out[i,j]<-out[i-k*m,j+k*m]
      }
      else if((i-j)%%(2*m)>=m){
        k<-((i-j)-(i-j)%%(2*m))/2/m+1
        out[i,j]<-out[i-k*m,j+k*m]
      }
    }
  }
  out
}

renv_periodic<-function(L,n,a,m){
  lapply(1:n,function(x)renv_periodic_one(L,a,m))
}


N<-10000

#ONE BOUNDARY
rand_env_1<-renv(10,N,1,2)
rand_env_2<-renv(10,N,1,2)

#PERIODIC
rand_env_1<-renv_periodic(10,N,1,3)
rand_env_2<-renv_periodic(10,N,1,3)

#test periodic with two periods intersection (is this right? shouldn't this fail since P3 translate and P1/Q1
#only partially cross?) maybe need to test with more data
P1<-urpaths(c(1,1),c(5,7))
P2<-urpaths(c(1,2),c(8,3))
Q1<-urpaths(c(1,2),c(5,8))
P3<-urpaths(c(1,2),c(5,3))




test1<-lapply(rand_env_1,function(x)c(Z(x,P1),Z(x,Q1),Z(x,P1)*Z(x,P2)*Z(x,P3),Z(x,P2)*Z(x,Q1)*Z(x,P3),Z(x,P1)*Z(x,P2),Z(x,P2)*Z(x,Q1)))
test2<-lapply(rand_env_2,function(x)c(Z(x,P1),Z(x,Q1),Z(x,P1)*Z(x,P2)*Z(x,P3),Z(x,P2)*Z(x,Q1)*Z(x,P3),Z(x,P1)*Z(x,P2),Z(x,P2)*Z(x,Q1)))


mean(unlist(lapply(test1,function(x)x[1])))
sqrt(var(unlist(lapply(test1,function(x)x[1])))/N)
mean(unlist(lapply(test2,function(x)x[2])))
sqrt(var(unlist(lapply(test2,function(x)x[2])))/N)

mean(unlist(lapply(test2,function(x)x[1])))
sqrt(var(unlist(lapply(test2,function(x)x[1])))/N)
mean(unlist(lapply(test1,function(x)x[2])))
sqrt(var(unlist(lapply(test1,function(x)x[2])))/N)

mean(unlist(lapply(test1,function(x)x[3])))
sqrt(var(unlist(lapply(test1,function(x)x[3])))/N)
mean(unlist(lapply(test2,function(x)x[4])))
sqrt(var(unlist(lapply(test2,function(x)x[4])))/N)

mean(unlist(lapply(test2,function(x)x[3])))
sqrt(var(unlist(lapply(test2,function(x)x[3])))/N)
mean(unlist(lapply(test1,function(x)x[4])))
sqrt(var(unlist(lapply(test1,function(x)x[4])))/N)


mean(unlist(lapply(test1,function(x)x[5])))
sqrt(var(unlist(lapply(test1,function(x)x[3])))/N)
mean(unlist(lapply(test2,function(x)x[6])))
sqrt(var(unlist(lapply(test2,function(x)x[4])))/N)

ks.test(unlist(lapply(test1,function(x)x[1])),unlist(lapply(test2,function(x)x[2])))
ks.test(unlist(lapply(test1,function(x)x[3])),unlist(lapply(test2,function(x)x[4])))
ks.test(unlist(lapply(test1,function(x)x[5])),unlist(lapply(test2,function(x)x[6])))







#test diagonal shift
P1<-urpaths(c(2,3),c(4,5))
Q1<-urpaths(c(2,4),c(3,4))
Q2<-urpaths(c(3,5),c(4,5))

test1<-lapply(rand_env,function(x)c(Z(x,P1),Z(x,Q1),Z(x,Q2),Z(x,P1)*Z(x,Q1),Z(x,P1)*Z(x,Q2)))
mean(unlist(lapply(test1,function(x)x[1])))
mean(unlist(lapply(test1,function(x)x[2])))
mean(unlist(lapply(test1,function(x)x[3])))
mean(unlist(lapply(test1,function(x)x[4])))
mean(unlist(lapply(test1,function(x)x[5])))


#bad...
renv<-function(L,n,a,b){
  bulk<-rinvgamma(n=n*L*(L-1)/2,shape=a,rate=1)
  diag<-rinvgamma(n=n*L,shape=b,rate=1)/2
  lapply(1:n,function(x)outer(1:L,1:L,function(i,j)ifelse(i==j,diag[(x-1)*L+i],bulk[(x-1)*L*(L-1)/2+min(i,j)+max(i,j)])))
}

