N <- 100
N_EXIST <- 10
BETA <- 1
SD <- 10
set.seed(1)

x <- 1:N
y <- x + rnorm(N)
lm.regular <- lm(y~x)

#mcar
x <- 1:N
y <- x + rnorm(N)
exist <- sample(1:N,N_EXIST)
x[-exist] <- 0
missing <- ifelse(x > 0, 0, 1)
lm.mcar.1 <- lm(y~x)
lm.mcar.2 <- lm(y~x+missing)

#non-mcar
x <- 1:N
y <- x + rnorm(N)
exist <- 1:10
x[-exist] <- 0
missing <- ifelse(x > 0, 0, 1)
lm.nonmcar.1 <- lm(y~x)
lm.nonmcar.2 <- lm(y~x+missing)
