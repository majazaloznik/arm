# # 2.1
# x.mean <- 35
# x.sd <- 10
# 
# y = a + b *x
# 
# y.mean = 100
# y.sd = 15
# 
# E[y] = a + b *E[x]
# 100 = a  + b *35
# 
# var[y] = b^2 * var[x]
# 225 = b^2 *100
# b = sqrt(2.25)
# b = 1.5
# a = 100-35*b

# 2.2 
girls.1908 <- c(.4777, .4875, .4859, .4754, .4874, .4864, .4813, .4787, .4895, .4797, .4876, .4859)
girls.1909 <- c(.4857, .4907, .5010, .4903, .4860, .4911, .4871, .4725, .4822, .4870, .4823, .4973)

n <- 3900

girls <- c(girls.1908, girls.1909)

std.observed <- sd(girls)
 
p.observed <- mean(girls)

std.expected <- sqrt(p.observed*(1-p.observed)/n)

# http://www.milefoot.com/math/stat/ci-variances.htm
df <- 23
alpha <- 0.05

lower.chi <- qchisq(0.025, 23)
higher.chi <- qchisq(0.975, 23)

sqrt((df)*std.observed^2/lower.chi)
sqrt((df)*std.observed^2/higher.chi)


# 2.3
library(tidyr)
library(dplyr)
# irwin hall distribution - the variance is n/12!
n.sims <- 10000
exp.mean <- 20/2
exp.stdev <- 20/12

  cbind(var = runif(n.sims*20),group = rep(1:n.sims, each=20)) %>% 
   as.data.frame(.) %>% 
   group_by(group) %>% 
   summarise(sum = sum(var)) %>% 
   pull(sum) -> out

hist(out, prob = TRUE)
lines(seq(5, 15, length = 100),
      dnorm(seq(5, 15, length = 100),exp.mean, sqrt(exp.stdev)),
      lwd = 3, col = "orange")
lines(density(out), lwd=2)

#2.4 
nsims <- 1000
m.men <- 69.1
sd.men <- 2.9
m.women <- 63.7
sd.women <- 2.7
  
cbind(men = rnorm(100*nsims, m.men, sd.men),
      women = rnorm(100*nsims, m.women, sd.women),
      group = rep(1:nsims, each=100)) %>% 
  as.data.frame(.) %>% 
  group_by(group) %>% 
  summarise(mean.m = mean(men), mean.w =mean(women)) %>% 
  mutate(dif = mean.m - mean.w) %>% 
  pull(dif) -> out

hist(out, prob = TRUE)
lines(seq(3, 7, length = 100),
      dnorm(seq(3, 7, length = 100),mean(out), sd(out)),
      lwd = 3, col = "orange")
lines(density(out), lwd=2)


theor.mean <- m.men - m.women
theor.mean
mean(out)
theor.sd <- sqrt(sd.men^2/100+ sd.women^2/100)
theor.sd
sd(out)


#2.5 correlated random variables
# this is just 
(mean.x+mean.y)*0.5
sqrt(0.25*sigma.x^2 + 0.25*sigma.y^2+ 0.5*ro*sigma.x*sigma.y)