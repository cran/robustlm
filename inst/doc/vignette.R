## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>", 
  warning = FALSE
)

## -----------------------------------------------------------------------------
set.seed(1)
library(MASS)
N <- 1000
p <- 8
rho <- 0.5
beta_true <- c(1, 1.5, 2, 1, 0, 0, 0, 0)
H <- abs(outer(1:p, 1:p, "-"))
V <- rho^H
X <- mvrnorm(N, rep(0, p), V)

# generate error term from a mixture normal distribution
components <- sample(1:2, prob=c(0.8, 0.2), size=N, replace=TRUE)
mus <- c(0, 10)
sds <- c(1, 6)
err <- rnorm(n=N,mean=mus[components],sd=sds[components])

Y <- X %*% beta_true + err

## -----------------------------------------------------------------------------
plot(lm(Y ~ X))

## -----------------------------------------------------------------------------
library(robustlm)
robustlm1 <- robustlm(X, Y)
robustlm1

## -----------------------------------------------------------------------------
coef(robustlm1)
Y_pred <- predict(robustlm1, X)
head(Y_pred)

## -----------------------------------------------------------------------------
data(Boston, package = "MASS")
head(Boston)

## -----------------------------------------------------------------------------
# scaling and centering
Boston[, -14] <- scale(Boston[, -14])
Boston[, 14] <- scale(Boston[, 14], scale = FALSE)

# diagnostic
set.seed(1)
x <- as.matrix(Boston[, -14])
y <- Boston[, 14]
lm_OLS <- lm(y ~ x - 1)
plot(lm_OLS)

## -----------------------------------------------------------------------------
# robustlm
robustlm2 <- robustlm(x, y)
coef(robustlm2)

## -----------------------------------------------------------------------------
cor(x, y)

## ---- fig.align='center', fig.keep='all', echo=FALSE, eval=FALSE--------------
#  beta <- robustlm2$beta
#  fun6 <- function(z) beta[6]*z
#  library(ggplot2)
#  data6 <- data.frame(medv=y, rm=x[,6])
#  p <- ggplot(data = data6, aes(x=rm, y=medv)) +
#    geom_function(fun = fun6) +
#    geom_point() + theme_bw()
#  p
#  ggsave(p, filename = "boston.jpg", height = 3.6, width = 3.6)

## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  for (i in 1:13) {
#    beta <- robustlm2$beta
#  fun <- function(z) beta[i]*z
#  library(ggplot2)
#  data <- data.frame(y=y, x=x[,i])
#  pl <- ggplot(data = data, aes(x=x, y=y)) +
#    geom_function(fun = fun) +
#    geom_point()
#  print(pl)
#  }

