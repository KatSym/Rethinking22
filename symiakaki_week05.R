library(tidyverse)
library(rethinking)

# 1 --------
library(dagitty)
data("NWOGrants")

award.dag <- dagitty("dag{
  gender -> discipline -> awards;
  gender -> awards;
  gender -> applications -> awards;
  gender -> discipline -> applications -> awards}")

plot(award.dag)
adjustmentSets(award.dag, "F", "W")
impliedConditionalIndependencies( fx_dag )

d <- NWOGrants %>% 
  mutate(G = ifelse(gender=="f", 1, 2),
         D = as.integer(discipline))
# 
# # generative example
# n <- 100
# G <- sample(1:2, size = n, replace = T)
# D <- rbern(n, ifelse(G==1, 0.3, 0.8)) +1
# aw.rate <- matrix(c(0.1, 0.3, 0.1, 0.3), nrow = 2)
# A <- rbern(n, aw.rate[D, G])


set_cmdstan_path('C:\\Users\\symiakaki\\Documents\\R\\R-4.1.2\\library\\cmdstan')

# total effect of gender
m <- ulam(
  alist(
    awards ~ dbinom(applications, p),
    logit(p) <- a[G],
    a[G] ~ dnorm( 0 , 1 )
  ), data=d, chains = 3, cores = 3 )

precis( m , depth=2 )
trankplot(m)
dev.off()

post <- extract.samples(m)
pra_1 <- inv_logit(post$a[,1]) # f
pra_2 <- inv_logit(post$a[,2]) # m
diff <- pra_2 - pra_1
dens(diff, lwd = 2, col= 2, xlab = "Gender contrast")

# 

# 2 --------

# direct effect of gender
m2 <- ulam(
  alist(
    awards ~ dbinom(applications, p),
    logit(p) <- a[G, D],
    matrix[G, D]:a ~ dnorm( 0 , 1 )
  ), data=d, chains = 3, cores = 3 )

precis( m2 , depth=3 )
trankplot(m2)
dev.off()

post <- extract.samples(m2)
pra <- inv_logit(post$a)
diff_D <- sapply(1:9, function(i) pra[,1, i] - pra[,2,i])
w <- xtabs(d$applications ~ d$D) / sum(d$applications)
plot(NULL, xlim = c(-.2, .3), ylim = c(0,25), ylab = "Density", xlab = "Gender contrast")
for (i in 1:9) dens(diff_D[,i], lwd = 1+ 20*w[i], col = 1+i, add = T)
