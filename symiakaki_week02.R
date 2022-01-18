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
    w ~ dnorm(mi, sigma) ,  # the correct greek pronunciation is mi (sorry)
    mi <- a + b*(h - h.av) ,  
    a ~ dnorm(60 , 10) ,
    b ~ dlnorm(0 , 1) ,
    sigma ~ dunif(0, 50)
  ) , data=adat)


new.h = c(140, 160, 175)
mi <- link(m, data = data.frame(h = new.h)) 

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
  filter(age < 13)

ggplot(kids, aes(x = weight, y = height)) + 
  geom_point()

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
    mo = a[s] + b[s]*(H - H.av),
    a[s] ~ dnorm(30,10),
    b[s] ~ dlnorm(0,1),
    sigma ~ dunif(0,10),
    
    # height
    H ~ dnorm(ni, tau),
    ni = k[s],
    k[s] ~ dnorm(130, 10),
    tau ~ dunif(0, 10)
  ), data = kids)
  
m_kids <-  quap(
  alist(
    W ~ dnorm(mi, sigma),
    mi <- a[s] + b[s]*(H - H.av),
    a[s] ~ dnorm(30,10),
    b[s] ~ dlnorm(0,1),
    sigma ~ dunif(0,10),
  ), data = kdat
)

# from the slides
xseq <- seq(130, 190, len= 50)

miF <- link(
  m_adults2, 
  data = list(
    S = rep(1,50), 
    H=xseq, 
    Hbar = mean(d$height)
    )
)
lines(xseq, apply(miF, 2, mean), lwd = 3, col= 2)

miM <- link(
  m_adults2, 
  data = list(
    S = rep(2,50), 
    H=xseq, 
    Hbar = mean(d$height)
  )
)
lines(xseq, apply(miM, 2, mean), lwd = 3, col= 4)

mi_contrast <- miF - miM



)

HWsim <- sim(m_full,
             data = list(S = c(1,2)),
             vars = c("H","W"))
W-Sauto <- HWsim[,2] - HWsim[,1]






