library(tidyverse)
library(rethinking)
library(dagitty)
set_cmdstan_path('C:\\Users\\symiakaki\\Documents\\R\\R-4.1.2\\library\\cmdstan')

data(reedfrogs)

d <- reedfrogs %>% 
  mutate(S = surv,
         n = density,
         t = 1:nrow(.),
         P = ifelse(pred == "pred", 1, 2),
         sz = ifelse(size == "small", 1, 2),
         D = scale(density))

d$P <- as.factor(d$P)
d$sz <- as.factor(d$sz)

# 1 --------

# we do a prior predictive check 
N <- 1000
a.av <- rnorm( N , 0, 1 )

sigma <- rexp(N, .1)
a <- rnorm( N , a.av , sigma )
dens(inv_logit(a), lwd = 2, col= 2)

sigma <- rexp(N, 1)
a <- rnorm( N , a.av , sigma )
dens(inv_logit(a), lwd = 2, col= 2)

sigma <- rexp(N, 10)
a <- rnorm( N , a.av , sigma )
dens(inv_logit(a), lwd = 2, col= 2)



# 2 ------

m <- ulam(
  alist(
    S ~ dbinom( n , p ) ,
    logit(p) <- a[t] + b[P, sz],
    a[t] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1 ) ,
    matrix[P, sz]:b ~ dnorm(0, 1),
    sigma ~ dexp( 1 )
  ), data=d , chains=4 , cores = 4, log_lik=TRUE )

precis(m, depth = 3)


dag <- dagitty("dag{
  T -> S;
  D -> S;
  D -> P;
  P -> S;
  Sz -> S}")

plot(dag)

# If we look at the b parameter, it's clear that predation has a negative effect on 
# survival, and actually its strongest effect is in a large size tank. In the case of no
# predation, tank size seems doesn't seems to affect the outcome. 

# 3 -----

# predation affects the density

mD <- ulam(
  alist(
    S ~ dbinom( n , p ) ,
    logit(p) <- a[t] + b[P, sz] + bD[P]*D,
    a[t] ~ dnorm( a_bar , sigma ) ,
    a_bar ~ dnorm( 0 , 1 ) ,
    matrix[P, sz]:b ~ dnorm(0, 1),
    bD[P] ~ dnorm(0, 1),
    sigma ~ dexp( 1 )
  ), data=d , chains=4 , cores = 4, log_lik=TRUE )

precis(mD, depth = 3)

# Again we see that with the presence of a predator the intercept for density is 
# negative - expected. 

s.m <- extract.samples(m)
s.mD <- extract.samples(mD)

dens(inv_logit(s.m$sigma))
dens(inv_logit(s.mD$sigma))

# Mean sigma in this case (0.67) is lower than in the model without density (0.75) and
# it has a wider destributions at high densities
