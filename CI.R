#函数CI_normal计算正态总体的置信区间
CI_normal <- function(x, sigma=-1, alpha=0.05){
  n <- length(x)
  mu <- mean(x)
  if(sigma>=0){
    tmp <- sigma/sqrt(n)*qnorm(1 - alpha/2)
    df <- n
  }
  else{
    tmp <- sd(x)/sqrt(n)*qt(1 - alpha/2, n-1)
    df <- n - 1
  }
  c(mu-tmp, mu+tmp)
}