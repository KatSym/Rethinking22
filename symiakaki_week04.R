library(tidyverse)
library(rethinking)

# 1 --------

d <- sim_happiness( seed=1977 , N_years=1000 )

d2 <- d[ d$age>17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )

d2$mid <- d2$married + 1
m6.9 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a[mid] + bA*A,
    a[mid] ~ dnorm( 0 , 1 ),
    bA ~ dnorm( 0 , 2 ),
    sigma ~ dexp(1)
  ) , data=d2 )
precis(m6.9,depth=2)


m6.10 <- quap(
  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a + bA*A,
    a ~ dnorm( 0 , 1 ),
    bA ~ dnorm( 0 , 2 ),
    sigma ~ dexp(1)
  ) , data=d2 )
precis(m6.10)

compare(m6.9, m6.10, func = WAIC)

compare(m6.9, m6.10, func = PSIS)


# in both cases the metric is huge, but in both cases the model that considers the married status is
# preferred (m6.9)




# 2 ---------

data("foxes")
f <- foxes %>% 
  mutate(A = scale(area),
         F = scale(avgfood),
         W = scale(weight),
         G = scale(groupsize))

m.FW <- quap(
  alist(
    W ~ dnorm(mi, sigma),  
    mi <- a + b*F,  
    a ~ dnorm(0 , 2),
    b ~ dlnorm(0 , .5),
    sigma ~ dexp(1)
  ), data=f)

m.FWG <- quap(
  alist(
    W ~ dnorm(mi, sigma),  
    mi <- a + bF*F + bG*G,  
    a ~ dnorm(0 , .2),
    bF ~ dnorm(0 , .5),
    bG ~ dnorm(0 , .5),
    sigma ~ dexp(1)
  ), data=f)



compare(m.FW, m.FWG, func = WAIC)


# m.FWG is slightly better at prediction. a is the expected mean fox weigth, bF is the direct 
# effect of food on weight and bG is the effect of groupsize on weight.


# 3 --------

data("cherry_blossoms")

b <- cherry_blossoms %>% 
  drop_na() 

m <- quap(
  alist(
    doy ~ dnorm( mu , sigma ) ,
    mu <- aT + bT*temp,
    aT ~ dnorm( 0 , 5 ) ,
    bT ~ dnorm( 0 , 5 ) ,
    sigma ~ dexp( 1 )
  ) , data=b)

m2 <- quap(
  alist(
    doy ~ dnorm( mu , sigma ) ,
    mu <- aT + b[1]*temp + b[2]*temp^2, 
    aT ~ dnorm( 0 , 5 ) ,
    b ~ dnorm( 0 , 5) ,
    sigma ~ dexp( 1 )
  ) , data=b, start=list(b=rep(0,2)))


compare(m, m2, func = WAIC)
# the polynomial model is better

D <- sim( m2 , data=list(temp=9) )
dens( D , lwd=3 , col=2 , xlab="1st bloom @ 9oC")
abline(v = mean(D))

# The 1st bloom will happen on day 81 which is Tuesday March 22nd in 2050
