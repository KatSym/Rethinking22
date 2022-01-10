
# 1 
# define grid
p_grid1 <- seq( from=0 , to=1 , length.out=1e5 )
# define prior
prior1 <- rep( 1 , 1e5 )
# compute likelihood at each value in grid
likelihood1 <- dbinom( 4 , size=15 , prob=p_grid1)
# compute product of likelihood and prior
unstd.posterior1 <- likelihood1 * prior1
# standardize the posterior, so it sums to 1
posterior1 <- unstd.posterior1 / sum(unstd.posterior1)

plot( p_grid1 , posterior1 , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )


# 2 
# define grid
p_grid2 <- seq(0, 1 ,length.out=1000)
# define prior
prior2 <- ifelse(p_grid2 < .5, 0, .87)
# compute likelihood at each value in grid
likelihood2 <- dbinom(4 , size = 6 , prob = p_grid2 )
# compute product of likelihood and prior
unstd.posterior2 <- likelihood2 * prior2
# standardize the posterior, so it sums to 1
posterior2 <- unstd.posterior2 / sum(unstd.posterior2)

plot( p_grid2 , posterior2 , type = "b" ,
      xlab="probability of water" , ylab="posterior probability" )
# abline(v = PI(samples, prob=.89 ), col = "red")
# abline(v = HPDI(samples, prob=.89), col = "blue")

dat <- data.frame(p_grid2, posterior2)
# 3
samples <- sample( p_grid2 , size = 1e4 , replace = T, prob = posterior2 )

library(rethinking)
pi <- PI(samples, prob = 0.89)

HPDI(samples, prob = 0.89)

ggplot(dat, aes(x=p_grid2, y=posterior2)) +
  geom_line() +
  geom_area(data = subset(dat, p_grid2 = c(0.5271527 : 0.8784933))
              , aes(fill="lightblue"), alpha = .7) +
  geom_area(data = subset(dat, p_grid2 = c(0.5020502 : 0.8423842))
            , aes(fill="green"), alpha = .7)
 