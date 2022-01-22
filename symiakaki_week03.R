library(tidyverse)
library(rethinking)

data("foxes")
d <- foxes

# 1 ---------

ggplot(d) +
  geom_point(aes(x = area, y = avgfood))


set.seed(13)
N <- 100 # 100 lines
a <- rnorm( N , .1 , .2 )
b <- rlnorm( N , .2 , 1 )
plot( NULL , xlim=range(d$area) , ylim=c(0,1.25) ,
      xlab="area" , ylab="weight" )
for ( i in 1:N ) curve( a[i] + b[i]*x,
                        from=min(d$area) , to=max(d$area) , add=TRUE ,
                        col=col.alpha("black",0.2) )


area.av <- mean(d$area)

m.AF <- quap(
  alist(
    avgfood ~ dnorm(mi, sigma) ,  
    mi <- a + b*(area - area.av) ,  
    a ~ dnorm(.1 , .2) ,
    b ~ dlnorm(.2 , 11) ,
    sigma ~ dexp(1)
  ) , data=d)
precis(m.AF)


# compute mi for each year of growth
teritory <- seq(1, 5, .2)
mi <- link(m.AF, data = data.frame(area = teritory))

plot(avgfood ~ area, d, type="n")
for ( i in 1:100 ){
  points( teritory , mi[i,] , pch=16 , col=col.alpha(rangi2,0.1) )}
# bit of a heavy start and slow weight gain, but that's life


# let's summarize mi
mi.mean <- apply(mi, 2, mean)
mi.PI <- apply(mi, 2, PI, prob=0.89)

# plot raw data

plot(avgfood ~ area, d, col=col.alpha(rangi2,0.5))
# draw MAP line
lines(teritory, mi.mean)
# plot a shaded region for PI
shade(mi.PI, teritory)

# the food availeable to the foxed would increase if they expanded their territory

# 2 ---------

# to estimate the total effect of F on W we don't need to adjust on any variable
#to estimate the direct effect, we need to adjust on G



