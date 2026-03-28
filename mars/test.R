# =============================================================================
# test.R - Three worked examples for the MARS R package
# STAT 360 Final Project
# =============================================================================

library(mars)

# =============================================================================
# Example 1: marstestdata (provided dataset)
# =============================================================================
cat("=== Example 1: marstestdata ===\n\n")

data("marstestdata", package = "mars")
mc <- mars.control(Mmax = 10, d = 3, trace = FALSE)
fit1 <- mars(y ~ ., data = marstestdata, control = mc)

print(fit1)
summary(fit1)
plot(fit1)
head(predict(fit1))
predict(fit1, newdata = head(marstestdata, 5))
anova(fit1)

# =============================================================================
# Example 2: Boston Housing Data
# =============================================================================
cat("\n=== Example 2: Boston Housing Data ===\n\n")

library(MASS)
data("Boston")

mc2  <- mars.control(Mmax = 20, d = 3, trace = FALSE)
fit2 <- mars(medv ~ ., data = Boston, control = mc2)

print(fit2)
summary(fit2)
plot(fit2)

# Predictions on first 5 rows
preds2 <- predict(fit2, newdata = head(Boston, 5))
print(data.frame(actual    = head(Boston$medv, 5),
                 predicted = round(preds2, 3)))

# In-sample R-squared
ss_res <- sum((Boston$medv - predict(fit2))^2)
ss_tot <- sum((Boston$medv - mean(Boston$medv))^2)
cat(sprintf("In-sample R-squared: %.4f\n", 1 - ss_res/ss_tot))

anova(fit2)

# =============================================================================
# Example 3: Simulated Nonlinear Data with Train/Test Split
# =============================================================================
cat("\n=== Example 3: Simulated Nonlinear Data ===\n\n")

set.seed(42)
n  <- 200
x1 <- runif(n, -2, 2)
x2 <- runif(n, -2, 2)
x3 <- runif(n, -2, 2)
y  <- sin(pi*x1) + (x2 - 0.5)^2 + abs(x3) + rnorm(n, sd = 0.3)
dat3 <- data.frame(y, x1, x2, x3)

# Train/test split
set.seed(7)
idx   <- sample(n, 160)
train <- dat3[idx, ]
test  <- dat3[-idx, ]

mc3  <- mars.control(Mmax = 12, d = 3, trace = FALSE)
fit3 <- mars(y ~ x1 + x2 + x3, data = train, control = mc3)

print(fit3)
summary(fit3)
plot(fit3)

preds3 <- predict(fit3, newdata = test)
rmse   <- sqrt(mean((test$y - preds3)^2))
cat(sprintf("Test RMSE: %.4f\n", rmse))

anova(fit3)

cat("\n=== All examples completed successfully ===\n")
