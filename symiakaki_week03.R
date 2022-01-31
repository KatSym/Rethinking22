# sorry for being late

library(tidyverse)
library(rethinking)

data("foxes")
f <- foxes %>% 
  mutate(A = scale(area),
         F = scale(avgfood),
         W = scale(weight),
         G = scale(groupsize))

# 1 ---------

#model
m.AF <- quap(
  alist(
    F ~ dnorm(mi, sigma),  
    mi <- a + b*A,  
    a ~ dnorm(0, .2),
    b ~ dlnorm(0, .5),
    sigma ~ dexp(1)
  ), data=f)

# prior predictive check
prior <- extract.prior(m.AF)
xseq <- c(-2,2)
mi <- link(m.AF, post = prior, data=list(A=xseq))
plot(NULL, xlim=xseq, ylim=xseq)
for ( i in 1:100 ) lines(xseq, mi[i,], col=col.alpha("black",0.3))
# they seem ok


precis(m.AF)

post <- extract.samples(m.AF, n = 100)

ggplot() +
  geom_point(data =f, aes(x = A, y = F)) +
  geom_abline(intercept = post$a, slope = post$b, colour = "forestgreen", alpha = .1) +
  xlab("Area")+
  ylab("Food")+
  theme_minimal()

# the food available to the foxes would increase if they expanded their territory

# 2 ---------

# To estimate the total effect of F on W we don't need to adjust on any variable

ggplot(f) +
  geom_point(aes(x = F, y = W))
# that's a very difficult line to draw

#model
m.FW <- quap(
  alist(
    W ~ dnorm(mi, sigma),  
    mi <- a + b*F,  
    a ~ dnorm(0 , 2),
    b ~ dlnorm(0 , .5),
    sigma ~ dexp(1)
  ), data=f)


# prior predictive check
prior <- extract.prior(m.FW)
xseq <- c(-2,2)
mi <- link(m.FW, post = prior, data=list(A=xseq))
plot(NULL, xlim=xseq, ylim=xseq)
for ( i in 1:100 ) lines(xseq, mi[i,], col=col.alpha("black",0.3))
precis(m.FW)
# just about right
# the availability of food doesn't seem to affect the average fox weight


#To estimate the direct effect, we need to adjust on G

# model
m.FWG <- quap(
  alist(
    W ~ dnorm(mi, sigma),  
    mi <- a + bF*F + bG*G,  
    a ~ dnorm(0 , .2),
    bF ~ dnorm(0 , .5),
    bG ~ dnorm(0 , .5),
    sigma ~ dexp(1)
  ), data=f)

precis(m.FWG)
# now the effect of food availability on fox weight is positive 
# but the effect of group size on weight is negative

post <- extract.samples(m.FWG, 100)

ggplot() +
  geom_point(data =f, aes(x = F, y = W)) +
  geom_abline(intercept = post$a, slope = post$bF, colour = "forestgreen", alpha = .2) +
  xlab("Food")+
  ylab("Weight")+
  theme_minimal()

ggplot() +
  geom_point(data =f, aes(x = G, y = W)) +
  geom_abline(intercept = post$a, slope = post$bG, colour = "lightblue", alpha = .3) +
  xlab("Group size")+
  ylab("Weight")+
  theme_minimal()


# It makes sense that the group size has a negative effect on weight, because for a given amount 
# food in a given area, a larger group of foxes would "starve" compared to a smaller one. On the 
# other hand, the group size should also affect the territory, as a larger group could claim a 
# larger area. So the DAG should have one more arrow from G to A. 


# 3 ----------

# In this DAG there are two back doors to X, one from S and one from A. So to estimate the effect 
# of X to Y we should adjust on both those parameters.

# The coefficient of X is not affected by the unobserved variable. There is a collider path opening 
# when we adjust on S.

  





