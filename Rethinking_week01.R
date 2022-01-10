library(rethinking)

# 1 ------

p_grid1 <- seq(0 ,1 , length.out = 1e5)

# prior
prior1 <- rep(1 , 1e5)

# likelihood
likelihood1 <- dbinom(4, size=15, prob=p_grid1)

# compute product of likelihood and prior
unstd.posterior1 <- likelihood1 * prior1

# standardize the posterior
posterior1 <- unstd.posterior1 / sum(unstd.posterior1)

# see what it looks like
plot(p_grid1, posterior1, type="b",
      xlab="probability of water", ylab="posterior probability")


# 2 ------
# another grid
p_grid2 <- seq(0, 1 ,length.out=1000)

# prior
prior2 <- ifelse(p_grid2 < .5, 0, .87)

# compute likelihood at each value in grid
likelihood2 <- dbinom(4, size = 6, prob = p_grid2)

# compute product of likelihood and prior
unstd.posterior2 <- likelihood2 * prior2

# standardize the posterior,
posterior2 <- unstd.posterior2 / sum(unstd.posterior2)

# see what it looks like
plot(p_grid2, posterior2, type = "b" ,
      xlab="probability of water", ylab="posterior probability")




# 3 ------

# sampling from the posterior distribution
samples <- sample(p_grid2 ,size = 1e4 ,replace = T,prob = posterior2 )

# 89% percentile
PI(samples, prob = 0.89)

# HPDI 
HPDI(samples, prob = 0.89)


# see what it looks like, bit fancier this time
dat <- data.frame(p_grid2, posterior2)

ggplot(dat, aes(x=p_grid2, y=posterior2)) +
  geom_line(size = 1) +
  geom_area(data = subset(dat, 
                          # PI limits
                          p_grid2 > 0.5271527 & p_grid2 < 0.8784933),
            fill="blue", 
            alpha = .3) + 
  geom_area(data = subset(dat, 
                          #HPDI limits
                          p_grid2 > 0.5020502 & p_grid2 < 0.8423842),
            fill="red", 
            alpha = .3) +
  scale_fill_manual(" ", labels = c("PI", "HPDI")) +
  theme_minimal()
 
# Pi is a bit wider than hpdi ~ 0.01