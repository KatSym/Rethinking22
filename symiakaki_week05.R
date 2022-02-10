library(tidyverse)
library(rethinking)

# 1 --------
library(dagitty)

award.dag <- dagitty("dag{
  gender -> discipline -> awards;
  gender -> awards}")

plot(award.dag)

data("NWOGrants")
d <- NWOGrants %>% 
  mutate(G = ifelse(gender=="f", 1, 2),
         D = as.integer(discipline))

# I have to set this otherwise the models don't run
set_cmdstan_path('C:\\Users\\symiakaki\\Documents\\R\\R-4.1.2\\library\\cmdstan')

# total effect of gender
m <- ulam(
  alist(
    awards ~ dbinom(applications, p),
    logit(p) <- a[G],
    a[G] ~ dnorm( 0 , 1 )
  ), data=d, chains = 4, cores = 4 )

precis( m , depth=2 )
trankplot(m, n_cols = 2)
dev.off()

post <- extract.samples(m)
pra_1 <- inv_logit(post$a[,1]) # f
pra_2 <- inv_logit(post$a[,2]) # m
diff <- pra_2 - pra_1 # m-f
dens(diff, lwd = 2, col= 2, xlab = "Gender contrast")
abline(v= 0)

# the highest density is in positive values so men seem to be favored over women - once again

# 2 --------

# direct effect of gender
m2 <- ulam(
  alist(
    awards ~ dbinom(applications, p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ dnorm( 0 , 1 )
  ), data=d, chains = 4, cores = 4 )
beepr::beep()

precis( m2 , depth=3 )
trankplot(m2)
dev.off()

# app_dep <-sapply(1:9, function(i) sum(d$applications[d$D==i]))
# p1 <- link(m2, data = )
# p2

post <- extract.samples(m2)
pra <- inv_logit(post$a)
diff_D <- sapply(1:9, function(i) pra[,2, i] - pra[,1,i])
w <- xtabs(d$applications ~ d$D) / sum(d$applications)

plot(NULL, xlim = c(-.25, .25), ylim = c(0,25), ylab = "Density", xlab = "Gender contrast")
for (i in 1:9) dens(diff_D[,i], lwd = 1+ 20*w[i], col = 1+i, add = T)

# 3 -----

# As we see from the weighted effects of the different departments, two departments 
# are mostly driving the mean posterior distribution of the total effect. 
# 