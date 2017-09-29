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

# assumption diagnosis

class(hwy_lm)
opar <- par(mfrow=c(2,2), oma=c(0, 0, 1.1, 0))
plot(hwy_lm, las=1)
par(opar)

# 7.6.7 Robust linear regression analysis

library(MASS)
set.seed(123)
lqs(stack.loss ~ ., data=stackloss)

lm(stack.loss ~., data=stackloss)

?stack.loss

# non linear

plot(hwy ~ displ, data=mpg)
mpg_lo <- loess(hwy ~ displ, data=mpg)
mpg_lo
summary(mpg_lo)

xs <- seq(2, 7, length.out = 100)
mpg_pre <- predict(mpg_lo, newdata=data.frame(displ=xs), se=T)
lines(xs, mpg_pre$fit)
lines(xs, mpg_pre$fit - 1.96*mpg_pre$se.fit, lty=2)
lines(xs, mpg_pre$fit + 1.96*mpg_pre$se.fit, lty=2)

## ggplot way

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth()

# 7.7 categorical x and continuous y

?model.matrix
?contrasts

mpg %>% 
  ggplot(aes(class, hwy)) +
  geom_boxplot()

(hwy_lm2 <- lm(hwy ~ class, data=mpg))
summary(hwy_lm2)

predict(hwy_lm2, newdata=data.frame(class="pickup"))

opar <- par(mfrow=c(2,2), oma=c(0, 0, 1.1, 0))
plot(hwy_lm2, las=1)
par(opar)

# 7.8 continous x, categorical y

chall <- read.csv("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/challenger.csv")
chall

chall <- tbl_df(chall)
glimpse(chall)

chall %>% 
  ggplot(aes(temperature, distress_ct)) +
  geom_point()

chall %>% 
  ggplot(aes(factor(distress_ct), temperature)) +
  geom_boxplot()

(chall_glm <-
    glm(cbind(distress_ct, o_ring_ct - distress_ct) ~
          temperature, data=chall, family='binomial'))

summary(chall_glm)

1 - pchisq(11.2, 1)

predict(chall_glm, data.frame(temperature=30))
predict(chall_glm)

exp(3.45) / (exp(3.45) + 1)
predict(chall_glm, data.frame(temperature=30), type="response")

# visualization of logistics model fitting consequence

logistic <- function(x) {exp(x) / (exp(x)+1)}
plot(c(20, 85), c(0,1), type="n", xlab="temperature",
     ylab = "prob")
tp <- seq(20, 85, 1)
chall_glm_pred <-
  predict(chall_glm,
          data.frame(temperature=tp),
          se.fit = T)

lines(tp, logistic(chall_glm_pred$fit))
lines(tp, logistic(chall_glm_pred$fit - 1.96 * chall_glm_pred$se.fit), lty=2)
lines(tp, logistic(chall_glm_pred$fit + 1.96 * chall_glm_pred$se.fit), lty=2)
abline(v=30, lty=2, col="blue")
