library(tidyverse)

y <- sleep$extra[sleep$group ==1]
y

summary(y)
sd(y)
par(mfrow=c(2,2))
hist(y)
boxplot(y)
qqnorm(y); qqline(y)
hist(y, prob=T)
lines(density(y), lty=2)
par(mfrow=c(1,1))

t.test(y)
t.test(y, alternative="greater")

curve(dnorm(x, 0, 1.8), -4, 4)

options(digits=3)
set.seed(1606)
(y_star <- rnorm(10, 0, 1.8))
mean(y_star-0); sd(y_star)
(t_star <- mean(y_star-0) / (sd(y_star)/sqrt(length(y_star))))

(y_star <- rnorm(10, 0, 1.8))
mean(y_star-0); sd(y_star)
(t_star <- mean(y_star-0) / (sd(y_star)/sqrt(length(y_star))))

(y_star <- rnorm(10, 0, 1.8))
mean(y_star-0); sd(y_star)
(t_star <- mean(y_star-0) / (sd(y_star)/sqrt(length(y_star))))

#simulation 10,000 times

set.seed(1606)
B <- 1e4
n <- 10
xbars_star <- rep(NA, B)
sds_star <- rep(NA, B)
ts_star <- rep(NA, B)

for (b in 1:B) {
  y_star <- rnorm(n, 0, 1.789)
  m <- mean(y_star)
  s <- sd(y_star)
  xbars_star[b] <- m
  sds_star[b] <- s
  ts_star[b] <- m / (s/sqrt(n))
}

opar <- par(mfrow=c(2,2))
hist(xbars_star, nclass=100)
abline(v=0.75, col='red')
hist(sds_star, nclass=100)
abline(v=1.789, col='red')
hist(ts_star, nclass=100)
abline(v=1.3257, col='red')
qqnorm(ts_star); qqline(ts_star)
par(opar)

length(which(ts_star > 1.3257)) / B

# Student T-distribution and t-test

# 6.8 the meaning of confidence interval

set.seed(1606)
(y_star <- rnorm(10, 1, 1.8))
t.test(y_star)$conf.int
(y_star <- rnorm(10, 1, 1.8))
t.test(y_star)$conf.int
(y_star <- rnorm(10, 1, 1.8))
t.test(y_star)$conf.int

set.seed(1606)
B <- 1e2
conf_intervals <-
  data.frame(b=rep(NA, B),
             lower=rep(NA, B),
             xbar=rep(NA, B),
             upper=rep(NA, B))
true_mu <- 1.0
for(b in 1:B) {
  (y_star <- rnorm(10, true_mu, 1.8))
  conf_intervals[b, ] = c(b=b,
                          lower=t.test(y_star)$conf.int[1],
                          xbar=mean(y_star),
                          upper=t.test(y_star)$conf.int[2])
}

conf_intervals <- conf_intervals %>% 
  mutate(lucky = (lower <= true_mu & true_mu <= upper))

glimpse(conf_intervals)
table(conf_intervals$lucky)

conf_intervals %>% 
  ggplot(aes(b, xbar, color=lucky)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper)) +
  geom_hline(yintercept=true_mu, color='red')

# central limit theorem

hist(c(0, 1), nclass=100, prob=T, main='Individual sleep time increase')
set.seed(1606)

B <- 1e4
n <- 10
xbars_star <- rep(NA, B)
for(b in 1:B) {
  xbars_star[b] <- mean(sample(c(0,1), size=n, replace=T))
}
hist(xbars_star, nclass=100, main='Sample mean of 10 obs')
?hist

diff(t.test(y)$conf.int)
mean(y)
diff(t.test(y)$conf.int)/2

