options(repos = c(CRAN = "https://cloud.r-project.org"))
# install.packages("invgamma")
library(invgamma)

periodic_invgamma_renv <- function(L,M,N) {
  out<-matrix(-1,L,L)
  for (i in 1:L) {
    for (j in 1:L) {
      if (out[i,j] >= 0) {
        next
      }
      if (out[i,j] <0) {
        out[i,j]<-rinvgamma(n=1,shape=1,rate=1)
        temp <- min(floor((j-1)/N),floor((L-i)/M))
        if (temp > 0) {
          for (t in 1:temp) {
            out[i+t*M,j-t*N] <- out[i,j]
          }
        }
      }
    }
  }
  out
}

periodic_renv<-function(n,L,M,N){
  lapply(1:n,function(x)periodic_invgamma_renv(L,M,N))
}

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

test <- function(renv,p1,p2){
  P1 <- urpaths(c(p1[[1]][[1]],p1[[1]][[2]]),c(p1[[2]][[1]],p1[[2]][[2]]))
  P2 <- urpaths(c(p2[[1]][[1]],p2[[1]][[2]]),c(p2[[2]][[1]],p2[[2]][[2]]))
  Q1 <- urpaths(c(p1[[1]][[1]],p1[[1]][[2]]+1), c(p1[[2]][[1]],p1[[2]][[2]]+1))
  lapply(renv,function(x)c(Z(x,P1),Z(x,P2),Z(x,Q1),Z(x,P1)*Z(x,P2),Z(x,P2)*Z(x,Q1)))
}

env <- periodic_renv(100,10,3,2)

p1 <- list(
  list(0,0),
  list(7,1))
p2 <- list(
  list(2,2),
  list(8,2))

for (i in 1:5) {
  print(mean(unlist(lapply(test(env,p1,p2),function(x) x[i]))))
}