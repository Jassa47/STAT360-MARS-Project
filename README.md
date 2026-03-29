# MARS R Package

## Multivariate Adaptive Regression Splines

An R package implementing the Multivariate Adaptive Regression Splines (MARS)
algorithm of Friedman (1991) for flexible nonlinear regression modelling.

**GitHub Repository:** https://github.com/Jassa47/STAT360-MARS-Project

---

## Group Members

- Akaaljot Mathoda — [Your SFU ID]
- Mayank Mayank — 301563549

---

## Files

### Core Algorithm

| File | Description |
|------|-------------|
| `R/mars.R` | Main fitting function `mars()` and all supporting functions: `fwd_stepwise()`, `bwd_stepwise()`, `LOF()`, `h()`, `split_points()`, `init_B()`, `mars.control()` |

### Methods

| File | Description |
|------|-------------|
| `R/print_mars.R` | `print()` method — displays call and coefficients |
| `R/summary_mars.R` | `summary()` method — displays basis function descriptions and lm summary |
| `R/plot_mars.R` | `plot()` method — 2D plots for 1-variable basis functions, 3D persp plots for 2-variable |
| `R/predict_mars.R` | `predict()` method and `make_B()` helper |
| `R/anova_mars.R` | `anova()` method — analysis of variance table |

### Tests

| File | Description |
|------|-------------|
| `tests/testthat/testmars_control.R` | Tests `mars.control()` |
| `tests/testthat/testfwd_stepwise.R` | Tests `fwd_stepwise()` |
| `tests/testthat/testbwd_stepwise.R` | Tests `bwd_stepwise()` |
| `tests/testthat/testLOF.R` | Tests `LOF()` |
| `tests/testthat/testmars.R` | Tests `mars()` |
| `tests/testthat/testpredict.R` | Tests `predict.mars()` |
| `test.R` | Three worked examples demonstrating all functions and methods |

---

## Installation
```r
devtools::install_github("Jassa47/STAT360-MARS-Project/mars")
library(mars)
```

---

## Quick Start
```r
library(mars)

# Fit a MARS model
fit <- mars(y ~ ., data = mydata, control = mars.control(Mmax = 10))

print(fit)
summary(fit)
plot(fit)
predict(fit, newdata = newdata)
anova(fit)
```

---

## References

Friedman, J.H. (1991). Multivariate Adaptive Regression Splines.
*The Annals of Statistics*, 19(1), 1--141.
