options(repos = c(CRAN = "https://cloud.r-project.org"))
#install.packages("invgamma")
library(invgamma)

periodic_invgamma_renv <- function(L,M,N) {
  out<-matrix(-1,L,L)
  for (i in 1:L) {
    for (j in 1:L) {
      if (out[i,j] >= 0) {
        next
      }
      if (out[i,j] <0) {
        out[i,j]<-rinvgamma(n=1,shape=j+i,rate=1)
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

rotate_right <- function(lst) {
  c(tail(lst, 1), head(lst, -1))
}

shift_rows <- function(m, j1, j2) {
  lst <- j1:j2
  m[lst, ] <- m[rotate_right(lst), ]
  m
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

Z_shift <- function(M,S,i,j){
  log(sum(apply(S,2,function(p) prod(unlist(lapply(p,function(z)shift_rows(M,i,j)[z[1],z[2]]))))))
}

test <- function(renv,p1,p2) {
  v <- max(max(-p1[[1]][[2]],0),max(-p2[[1]][[2]],0))
  P1 <- urpaths(c(p1[[1]][[1]],p1[[1]][[2]]+v),c(p1[[2]][[1]],p1[[2]][[2]]+v))
  P2 <- urpaths(c(p2[[1]][[1]],p2[[1]][[2]]+v),c(p2[[2]][[1]],p2[[2]][[2]]+v))
  Q1 <- urpaths(c(p1[[1]][[1]],p1[[1]][[2]]+v+1), c(p1[[2]][[1]],p1[[2]][[2]]+v+1))
  lapply(renv,function(x)c(Z(x,P1),Z(x,P2),Z(x,Q1),Z(x,P1)*Z(x,P2),Z(x,P2)*Z(x,Q1)))
}

test1 <- function(renv,p1,p2) {
  P1 <- urpaths(c(p1[[1]][[1]],p1[[1]][[2]]),c(p1[[2]][[1]],p1[[2]][[2]]))
  P2 <- urpaths(c(p2[[1]][[1]],p2[[1]][[2]]),c(p2[[2]][[1]],p2[[2]][[2]]))
  Q1 <- urpaths(c(p1[[1]][[1]],p1[[1]][[2]]+1), c(p1[[2]][[1]],p1[[2]][[2]]+1))
  i <- c(p1[[1]][[2]])
  j <- c(p1[[2]][[2]]+1)
  lapply(renv,function(x)c(Z(x,P1),Z_shift(x,Q1,i,j),Z(x,P2),Z_shift(x,P2,i,j),Z(x,P1)*Z(x,P2),Z_shift(x,P2,i,j)*Z_shift(x,Q1,i,j)))
}

env <- periodic_renv(500,8,5,4)

p1 <- list(
  list(1,3),
  list(6,5))
p2 <- list(
  list(4,1),
  list(4,6))

for (i in 1:6) {
  print(mean(unlist(lapply(test1(env,p1,p2),function(x)x[i]))))
}