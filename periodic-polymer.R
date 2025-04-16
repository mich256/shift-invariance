library(invgamma)

periodic_rand_env <- function(L,M,N) {
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

env <- periodic_rand_env(L = 5, M=3, N=2)
print(env)