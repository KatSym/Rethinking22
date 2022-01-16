library(tidyverse)
library(rethinking)
data("Howell1")
d = Howell1


# 1 ----------
adults = d %>% 
  filter(age >= 18)

adat = list(
  h = adults$height,
  w = adults$weight,
  h.av = mean(adults$height))

#model
m <- quap(
  alist(
    w ~ dnorm( mi , sigma ) ,  # the correct greek pronunciation is mi (sorry)
    mi <- a + b*(h - h.av ) ,  
    a ~ dnorm( 170 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=adat )


new.h = c(140, 160, 175)
mi = link(m, data = data.frame(height = new.h))

mi.mean <- apply( mi , 2 , mean )
mi.PI <- apply( mi , 2 , PI , prob=0.89 )
