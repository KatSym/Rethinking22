library(tidyverse)
library(rethinking)

data("Howell1")
d = Howell1


# 1 ----------

adults = d %>% 
  filter(age >= 18)

h.av = mean(adults$height)

adat = list(
  h = adults$height,
  w = adults$weight,
  h.av = mean(adults$height))

#model
m <- quap(
  alist(
    w ~ dnorm(mi, sigma) ,  # in Greek we call it mi, so I will stick with that
    mi <- a + b*(h - h.av) ,  
    a ~ dnorm(60 , 10) ,
    b ~ dlnorm(0 , 1) ,
    sigma ~ dunif(0, 50)
  ) , data=adat)

# compute mi for the given heights
new.h = c(140, 160, 175)
mi <- link(m, data = data.frame(h = new.h)) 

#let's summarize
mi.mean <- apply(mi, 2, mean)
mi.PI <- apply(mi, 2, PI, prob=.89)

# So

#   | h   |  w   | 89% interval
# -----------------------------------
# 1 | 140 | 35.8 | 35.08346-36.56135 
# 2 | 160 | 48.4 | 47.95932-48.81317 
# 3 | 175 | 57.8 | 56.83421-58.83126



# 2 --------

kids = d %>% 
  filter(age <= 13) %>% 
  mutate(sex = male + 1) %>% # note to self for later: 1=girl, 2=boy
  select(-male)

# let's see how they look like
ggplot(kids, aes(x = weight, y = height)) + 
  geom_point()

# explore priors
set.seed(13)
N <- 100 # 100 lines
a <- rnorm( N , 10 , 5 )
b <- rlnorm( N , 1 , .5 )
plot( NULL , xlim=range(kids$age) , ylim=c(-5,40) ,
      xlab="age" , ylab="weight" )
for ( i in 1:N ) curve( a[i] + b[i]*x,
                        from=min(kids$age) , to=max(kids$age) , add=TRUE ,
                        col=col.alpha("black",0.2) )
# I guess they are ok?


m_kids <- quap(
  alist(
    weight ~ dnorm(mi, sigma),
    mi <- a + b*age,
    a ~ dnorm(20, 5),
    b ~ dlnorm(1, .5),
    sigma ~ dexp(1)
  ), data = kids)

precis(m_kids)

# compute mi for each year of growth
years <- seq(0, 13, 1)
mi <- link(m_kids, data = data.frame(age = years))

plot(weight ~ age, kids, type="n")
for ( i in 1:100 ){
  points( years , mi[i,] , pch=16 , col=col.alpha(rangi2,0.1) )}
# bit of a heavy start and slow weight gain, but that's life


# let's summarize mi
mi.mean <- apply(mi, 2, mean)
mi.PI <- apply(mi, 2, PI, prob=0.89)

# plot raw data

plot(weight ~ age, data=kids, col=col.alpha(rangi2,0.5))
# draw MAP line
lines(years, mi.mean)
# plot a shaded region for PI
shade(mi.PI, years)

# 3 ----------

m_kidsS <- quap(
  alist(
    weight ~ dnorm(mi, sigma) ,
    mi <- a[sex] + b[sex]*age,
    a[sex] ~ dnorm(20, 5) ,
    b[sex] ~ dlnorm(1, .5) ,
    sigma ~ dexp(1)
  ), data=kids)

precis(m_kidsS, depth = 2)

# compute separate mis for each year of growth for girls and boys
miG = link(m_kidsS, data = list(sex=rep(1,14), age=years))
miB = link(m_kidsS, data = list(sex=rep(2,14), age=years))

mi.contr = miB-miG

#plot the difference
plot(NULL, xlim=range(years), ylim=c(-1,5), xlab = "Age", ylab = "Weight contrast of B-G")
for (p in c(.5, .6, .7, .8, .9)) {
  shade(apply(mi.contr, 2, PI, prob=p), years)
}
abline(h=0,lty=2)
# as it turns out boys are always heavier than girls
