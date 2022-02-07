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
  drop_na() %>% 
  mutate(T = scale(temp),
         D = scale(doy))
  
num_knots <- 10
knot_list <- quantile( b$T , probs=seq(0,1,length.out=num_knots) )

library(splines)
B <- bs(b$T,
        knots=knot_list[-c(1,num_knots)] ,
        degree=3 , intercept=TRUE )



m <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- aT + bT*T,
    aT ~ dnorm( 0 , .1 ) ,
    bT ~ dnorm( 0 , .3 ) ,
    sigma ~ dexp( 1 )
  ) , data=b)

# prior predictive check
prior <- extract.prior( m )
xseq <- c(-2,2)
mu <- link( m , post = prior , data=list(T=xseq))
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:100 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )
precis(m)
plot( b$T , b$D , lwd=1, col=8 )
post <- extract.samples(m)
for ( i in 1:100 ) abline( post$a[i] , post$b[i] , lwd=3 , col=alpha(4, 0.1) )

m2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- aT + b[1]*T + b[2]*T^2, 
    aT ~ dnorm( 0 , .1 ) ,
    b ~ dnorm( 0 , .3 ) ,
    # bY ~ dnorm( 0 , .3 ) ,
    sigma ~ dexp( 1 )
  ) , data=b, start=list(b=rep(0,2)))

m3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + B %*% w ,
    a ~ dnorm(0, .1),
    w ~ dnorm(0 , 3),
    sigma ~ dexp(1)
  ), data=list( D=b$doy , B=B ) ,
  start=list( w=rep( 0 , ncol(B) ) ) )



compare(m, m3, func = WAIC)
