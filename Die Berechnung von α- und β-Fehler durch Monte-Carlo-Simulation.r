# Power
#Annahme1
n <- 20
m <- 10000
mu0 <- 0.5
sigma <- 0.1
mu <- c(seq(0.45, 0.65, 0.001))  
M <- length(mu)
power <- numeric(M)
for (i in 1:M) {
  mu1 <- mu[i]
  pvalues <- replicate(m, expr = {
    x <- rnorm(n, mean = mu1, sd = sigma)
    ttest <- t.test(x,
                    alternative = "greater", mu = mu0)
    ttest$p.value  } )
  power[i] <- mean(pvalues <= 0.05)
}

par(ask = TRUE)
library(Hmisc)  
plot(mu, power)
abline(v = mu0, lty = 1)
abline(h = .05, lty = 1)
se <- sqrt(power * (1-power) / m)
errbar(mu, power, yplus = power+se, yminus = power-se,
       xlab = bquote(theta))
lines(mu, power, lty=3)
detach(package:Hmisc)
par(ask = FALSE)

#Annahme2
n <- 20
m <- 10000
mu0 <- 0.5
sigma <- 0.2
mu <- c(seq(0.45, 0.65, 0.001))  
M <- length(mu)
power <- numeric(M)
for (i in 1:M) {
  mu1 <- mu[i]
  pvalues <- replicate(m, expr = {
    x <- rnorm(n, mean = mu1, sd = sigma)
    ttest <- t.test(x,
                    alternative = "greater", mu = mu0)
    ttest$p.value  } )
  power[i] <- mean(pvalues <= 0.05)
}

par(ask = TRUE)
library(Hmisc)  
plot(mu, power)
abline(v = mu0, lty = 1)
abline(h = .05, lty = 1)
se <- sqrt(power * (1-power) / m)
errbar(mu, power, yplus = power+se, yminus = power-se,
       xlab = bquote(theta))
lines(mu, power, lty=3)
detach(package:Hmisc)
par(ask = FALSE)


#Annahme3
n <- 40
m <- 10000
mu0 <- 0.5
sigma <- 0.1
mu <- c(seq(0.45, 0.65, 0.001))  
M <- length(mu)
power <- numeric(M)
for (i in 1:M) {
  mu1 <- mu[i]
  pvalues <- replicate(m, expr = {
    x <- rnorm(n, mean = mu1, sd = sigma)
    ttest <- t.test(x,
                    alternative = "greater", mu = mu0)
    ttest$p.value  } )
  power[i] <- mean(pvalues <= 0.05)
}

par(ask = TRUE)
library(Hmisc)  
plot(mu, power)
abline(v = mu0, lty = 1)
abline(h = .05, lty = 1)
se <- sqrt(power * (1-power) / m)
errbar(mu, power, yplus = power+se, yminus = power-se,
       xlab = bquote(theta))
lines(mu, power, lty=3)
detach(package:Hmisc)
par(ask = FALSE)

# Alpha
#Annahme1
n <- 20
alpha <- 0.05 
mu0 <- 0.5
sigma <- 0.1
m <- 10000          
p <- numeric(m)     
for (j in 1:m) {
  x <- rnorm(n, mu0, sigma)
  ttest <- t.test(x, alternative = "greater", mu = mu0)
  p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))
plot(1:m,cumsum(p<alpha)/1:m,type="l")
abline(h=0.05)

#Annahme2
n <- 20
alpha <- 0.05 
mu0 <- 0.5
sigma <- 0.2
m <- 10000         
p <- numeric(m)     
for (j in 1:m) {
  x <- rnorm(n, mu0, sigma)
  ttest <- t.test(x, alternative = "greater", mu = mu0)
  p[j] <- ttest$p.value
}
p.hat <- mean(p < alpha)
se.hat <- sqrt(p.hat * (1 - p.hat) / m)
print(c(p.hat, se.hat))
plot(1:m,cumsum(p<alpha)/1:m,type="l")
abline(h=0.05)

