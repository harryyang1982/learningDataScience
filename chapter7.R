library(tidyverse)

mpg <- tbl_df(mpg)
mpg
data(mpg)
mpg

glimpse(mpg)
head(mpg)
summary(mpg)

summary(mpg$hwy)
mean(mpg$hwy)
median(mpg$hwy)
range(mpg$hwy)
quantile(mpg$hwy)

opar <- par(mfrow=c(2,2))
hist(mpg$hwy)
boxplot(mpg$hwy)
qqnorm(mpg$hwy)
qqline(mpg$hwy)
par(opar)

# one variable t-test

hwy <- mpg$hwy
n <- length(hwy)
mu0 <- 22.9
t.test(hwy, mu=mu0, alternative="greater")

t.test(hwy)

boxplot(mpg$hwy)$out

c(mean(hwy), sd(hwy))
c(median(hwy), mad(hwy))

# success-failure categorial variable analysis

set.seed(1606)
n <- 100
p <- 0.5
x <- rbinom(n, 1, p)
x <- factor(x, levels = c(0,1), labels = c("no", "yes"))

x
table(x)
prop.table(table(x))
barplot(table(x))

ggplot(data=data.frame(x), aes(x)) +
  geom_bar()

binom.test(x=length(x[x=='yes']), n=length(x), p=0.5, alternative="two.sided")

binom.test(x=5400, n=10000)

n <- c(100, 1000, 2000, 10000, 1e6)
data.frame(n=n, moe=round(1.96 * sqrt(1/(4*n)), 4))

curve(1.96 * sqrt(1/(4*x)), 10, 10000, log='x')
grid()

# explanatory variable X and response variable Y

# 7.6 

ggplot(mpg, aes(cty, hwy)) + geom_jitter() + geom_smooth(method="lm")

cor(mpg$cty, mpg$hwy)
with(mpg, cor(cty, hwy))
with(mpg, cor(cty, hwy, method="kendall"))
with(mpg, cor(cty, hwy, method="spearman"))

# linear regression

(hwy_lm <- lm(hwy ~ cty, data=mpg))
summary(hwy_lm)

# model goodness of fit

# prediction

predict(hwy_lm)

resid(hwy_lm)

predict(hwy_lm, newdata=data.frame(cty=c(10, 20, 30)))
predict(hwy_lm, newdata=data.frame(cty=c(10, 20, 30)), se.fit=T)
