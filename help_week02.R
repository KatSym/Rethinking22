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
mi = link(m, data = data.frame(height = new.h)) # Error in eval(parse(text = lm), envir = e) : object 'h' not found

mi.mean <- apply( mi , 2 , mean )
mi.PI <- apply( mi , 2 , PI , prob=0.89 )

#don't know if that correct

# 2-3 --------

kids = d %>% 
  filter(age < 13)

kdat <- list(
  H = kids$height,
  W = kids$weight,
  H.av = mean(kids$height),
  s = kids$male + 1
)


m_full = quap(
  alist(
    
    #weight
    W ~ dnorm(mu, sigma),
    mu = a[s] + b[s]*(H - H.av),
    a[s] ~ dnorm(30,10),
    b[s] ~ dlnorm(0,1),
    sigma ~ dunif(0,10),
    
    # height
    H ~ dnorm(nu, tau),
    nu = k[s],
    k[s] ~ dnorm(110, 10),
    tau ~ dunif(0, 10)
  ), data = kids
)

# Error in eval(flist[[i]]) : object 'a' not found
# In addition: Warning message:
#   In flist_untag(flist) :
#   Named entry 'mu' detected. Make sure you didn't use '=' where you meant '~' or '<-'.
  
m_kids <-  quap(
  alist(
    W ~ dnorm(mi, sigma),
    mi <- a[s] + b[s]*(H - H.av),
    a[s] ~ dnorm(30,10),
    b[s] ~ dlnorm(0,1),
    sigma ~ dunif(0,10),
  ), data = kdat
)

# Error in eval(flist[[i]]) : argument is missing, with no default



