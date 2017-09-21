install.packages("gapminder")
library(gapminder)

head(gapminder)
tail(gapminder)

library(tidyverse)
glimpse(gapminder)

summary(gapminder$lifeExp)
summary(gapminder$gdpPercap)
cor(gapminder$lifeExp, gapminder$gdpPercap)

gapminder[gapminder$lifeExp == max(gapminder$lifeExp),]
filter(gapminder, lifeExp==max(lifeExp))

opar <- par(mfrow=c(2,2))
hist(gapminder$lifeExp)
hist(gapminder$gdpPercap, nclass=50)
hist(log10(gapminder$gdpPercap), nclass=50)
plot(log10(gapminder$gdpPercap), gapminder$lifeExp, cex=.5)
par(opar)

cor(gapminder$lifeExp, log10(gapminder$gdpPercap))

#4.2 ggplot graphics and base R graphics

gapminder %>% 
  ggplot(aes(x=lifeExp)) + geom_histogram()
gapminder %>% 
  ggplot(aes(x=gdpPercap)) + geom_histogram()
gapminder %>% 
  ggplot(aes(x=gdpPercap)) + geom_histogram() +
  scale_x_log10()
gapminder %>% 
  ggplot(aes(x=gdpPercap, y=lifeExp)) + geom_point() +
  scale_x_log10() + geom_smooth()

example(ggplot)

df <- data.frame(gp=factor(rep(letters[1:3], each=10)),
                  y=rnorm(30))
glimpse(df)

ds <- df %>% 
  group_by(gp) %>% 
  summarise(mean=mean(y), sd=sd(y))
ds

ggplot(df, aes(x=gp, y=y)) +
  geom_point() +
  geom_point(data=ds, aes(y=mean), color='red', size=3)

ggplot(df) +
  geom_point(aes(x=gp, y=y)) +
  geom_point(data=ds, aes(x=gp, y=mean), color='red', size=3)

ggplot() +
  geom_point(data=df, aes(x=gp, y=y)) +
  geom_point(data=ds, aes(x=gp, y=mean), color='red', size=3) +
  geom_errorbar(data=ds, aes(x=gp, y=mean, ymin=mean-sd, ymax=mean+sd), color='red', width=0.4)

#4.2.2 ggplot and dplyr %>%

gapminder %>% 
  ggplot(aes(lifeExp)) + geom_histogram()

#4.2.3 example data

glimpse(diamonds)
glimpse(mpg)

#4.3 Visualization techniques

gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram()
gapminder %>% ggplot(aes(x=gdpPercap)) + geom_histogram() + scale_x_log10()
gapminder %>% 
  ggplot(aes(x=gdpPercap)) + geom_freqpoly() +
  scale_x_log10()
gapminder %>% 
  ggplot(aes(x=gdpPercap)) + geom_density() +
  scale_x_log10()

summary(gapminder)

#4.3.2 one discrete variable

diamonds %>% 
  ggplot(aes(cut)) + geom_bar()

table(diamonds$cut)
prop.table(table(diamonds$cut))

diamonds %>% 
  group_by(cut) %>% 
  tally() %>% 
  mutate(pct = round(n/sum(n) * 100, 1))
?tally # tally is equal to count

# 4.3.3 two continuous varibles

diamonds %>% 
  ggplot(aes(carat, price)) +
  geom_point()
diamonds %>%
  ggplot(aes(carat, price)) +
  geom_point(alpha=0.1)
mpg %>% 
  ggplot(aes(cyl, hwy)) +
  geom_point()
mpg %>% 
  ggplot(aes(cyl, hwy)) +
  geom_jitter()

pairs(diamonds %>% sample_n(1000))
plot(diamonds)
plot(diamonds %>% sample_n(1000))

#4.3.4 numerical variable and categorical variable
mpg %>% 
  ggplot(aes(class, hwy)) + geom_boxplot()

mpg %>% 
  ggplot(aes(class, hwy, color=class)) +
  geom_boxplot()

mpg %>% 
  ggplot(aes(class, hwy)) + geom_jitter(color='grey') +
  geom_boxplot(alpha=0.5)
mpg %>% 
  mutate(class=reorder(class, hwy, median)) %>% 
  ggplot(aes(class, hwy)) + geom_jitter(color="grey") +
  geom_boxplot(alpha=0.5)
mpg %>%
  ggplot(aes(x=reorder(class, hwy, median), hwy)) + geom_jitter(color="grey") +
  geom_boxplot(alpha=0.5)

mpg %>% 
  mutate(class=factor(class, levels=c("2seater", "subcompact", "compact", "midsize", "minivan", "suv", "pickup"))) %>% 
  ggplot(aes(class, hwy)) + geom_jitter(color="grey") +
  geom_boxplot(alpha=0.5)
mpg %>% 
  mutate(class=factor(class, levels=c("2seater", "subcompact", "compact", "midsize",
                                      "minivan", "suv", "pickup"))) %>% 
  ggplot(aes(class, hwy)) + geom_jitter(color="grey") +
  geom_boxplot(alpha=0.5) + coord_flip()

# 2 categorical variables

glimpse(data.frame(Titanic))
xtabs(Freq ~ Class + Sex + Age + Survived, data.frame(Titanic))

Titanic
mosaicplot(Titanic, main = "Survival on the Titanic")
mosaicplot(Titanic, main = "Survival on the Titanic", color=TRUE)

apply(Titanic, c(3,4), sum)
Titanic

round(prop.table(apply(Titanic, c(3,4), sum), margin=1), 3)
glimpse(Titanic)

apply(Titanic, c(2,4), sum)
round(prop.table(apply(Titanic, c(2,4), sum), margin=1), 3)

t2 <- data.frame(Titanic)
t2

t2 %>% 
  group_by(Sex) %>% 
  summarise(n=sum(Freq),
            survivors=sum(ifelse(Survived=="Yes", Freq, 0))) %>% 
  mutate(rate_survival=survivors/n)

t2 %>% 
  group_by(Age) %>% 
  summarise(n=sum(Freq),
            survivors=sum(ifelse(Survived=="Yes", Freq, 0))) %>% 
  mutate(rate_survival=survivors/n)

# 4.3.6 Geom

gapminder %>% 
  filter(year==2007) %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point() + scale_x_log10() +
  ggtitle("Gapminder data for 2007")
gapminder %>% 
  filter(year==2007) %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point(aes(size=pop, col=continent)) +
  scale_x_log10() +
  ggtitle("Gapminder data for 2007")

# 4.3.7 Facet

gapminder %>% 
  ggplot(aes(year, lifeExp, group=country)) +
  geom_line()

gapminder %>% 
  ggplot(aes(year, lifeExp, group=country, color=continent)) +
  geom_line()

gapminder %>% 
  ggplot(aes(year, lifeExp, group=country, color=continent)) +
  geom_line() +
  facet_wrap(~ continent)

gapminder %>% 
  ggplot(aes(year, lifeExp, group=country)) +
  geom_line() +
  facet_wrap(~ continent)

gapminder %>% 
  ggplot(aes(year, lifeExp, group=country, color=country)) +
  geom_line() +
  facet_wrap(~ continent) +
  theme(legend.position = "none")

# 4.4 some principles for the visualization

