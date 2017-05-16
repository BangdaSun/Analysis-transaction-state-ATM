# In practice, we will always model the occurance of event as Poisson process:
# 
#   The number of event happens in [0, t] has distribution Poisson(lambda*t)
#   The interarrival time has iid distribution Exponential(lambda)
#   The occurance of nth event has distribution Gamma(n*lambda)
#
#

# DEMO of Poisson process

lambda = 20
# Fix time interval
t      = seq(100)
n      = rpois(length(t), lambda = lambda)
df     = data.frame(t = t,
                    n = n)

library(ggplot2)
p1 = ggplot(df) + 
  geom_point(mapping = aes(x = t, y = n), size = 3) +
  geom_line(mapping = aes(x = t, y = n)) +
  labs(title = "simulation from counting process")

# Fix number of events
num    = 20000
intarr = rexp(num, rate = lambda)
eventT = cumsum(intarr)

countEvent = function(time, eventT) {
  # @param time, stop time
  # @param eventT, occurance time
  # @return count, the number of event happens at [0, time]
  count = sum(eventT < time)
  return(count)
}

countEvent(1, eventT)
n2    = diff(sapply(seq(101), countEvent, eventT))
df2   = data.frame(t = t,
                   n = n2)
p2 = ggplot(df2) + 
  geom_point(mapping = aes(x = t, y = n), size = 3) +
  geom_line(mapping = aes(x = t, y = n)) +
  labs(title = "simulation from Exponential interarrival time")

library(grid)
library(gridExtra)
grid.arrange(p1, p2, ncol = 1)

# different lambda
lambda1 = 20;  t1 = seq(60)
lambda2 = 110;  t2 = seq(from = 61, to = 90)
lambda3 = 80;  t3 = seq(from = 91, to = 110)
lambda4 = 150; t4 = seq(from = 111, to = 160)
lambda5 = 20;  t5 = seq(from = 161, to = 300)

n1 = rpois(length(t1), lambda = lambda1)
n2 = rpois(length(t2), lambda = lambda2)
n3 = rpois(length(t3), lambda = lambda3)
n4 = rpois(length(t4), lambda = lambda4)
n5 = rpois(length(t5), lambda = lambda5)

df3 = data.frame(t = c(t1, t2, t3, t4, t5),
                 n = c(n1, n2, n3, n4, n5))
ggplot(df3) +
  geom_line(mapping = aes(x = t, y = n)) +
  labs(title = "simulation of different lambda")

# Time dependent
lambda = 20
t      = seq(100)
n      = vector(mode = "numeric", length = length(t))
for (i in seq(length(n))) {
  n[i] = rpois(1, lambda = lambda * t[i] * 1.3)
}
df     = data.frame(t = t,
                    n = n)

ggplot(df) + 
  geom_point(mapping = aes(x = t, y = n), size = 3) +
  geom_line(mapping = aes(x = t, y = n)) +
  labs(title = "simulation about time dependent parameter")

#####################################################################################
