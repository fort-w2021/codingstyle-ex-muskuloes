# compute confidence interval
# param: estimate overwhich to compute
#       the confidence interval
# value: returns confidence_interval
confidence_interval <- function(estimate) {
  m <- mean(estimate)
  s <- sd(estimate)
  n <- length(estimate)
  c(m - 1.96 * s / sqrt(n), m + 1.96 * s / sqrt(n))
}

x <- rnorm(100)
y <- rnorm(100)
z <- rnorm(100)
v <- z^2

confidence_interval(x)
confidence_interval(y)
confidence_interval(z)
confidence_interval(v)
