# iBART: **I**terative **B**ART‑guided **A**lgorithm for **R**epresentative **T**ransforms  
Operator‑induced Structural Variable Selection in R

---

## ✨ Overview
**iBART** is an R implementation of the methodology described in *Ye, Senftle & Li (2023), “Operator‑induced structural variable selection for identifying materials genes”*.  
The pipeline combines:

* **Descriptor generation** via algebraic, transcendental and trigonometric operators (unary/binary)
* **BART‑based screening** for non‑linear relevance
* **Constant transformation optimisation** (grid + BFGS, optional)
* **Sparse linear modelling** by LASSO (`glmnet`) or best‑subset `L₀` regression
* **Iterative refinement** with automatic unit checking to ensure physical consistency

The goal is to turn raw primary features \(X\) into a compact, human‑readable formula \(\hat y = f(X)\) that retains predictive accuracy.

![](docs/figures/ibart_workflow.svg)

---

## 📦 Repository Structure
| Path | Purpose |
|------|---------|
| `iBART.R` | Main orchestration function |
| `BART_iter.R` | One BART‑guided selection step |
| `descriptorGenerator.R` | Builds descriptors from `operations.R` |
| `operations.R` | Unary / binary operator library with unit logic |
| `LASSO.R` | Post‑selection refit with cross‑validated elastic net |
| `L_zero_regression.R` | Optional best‑subset (`l0`) search |
| `generate_unit.R` | Helper for unit dimension matrices |
| `utilis.R` | Train/test split, scaling & logging helpers |
| `data.R` | Example catalysis dataset + saved vignette results |
| `test_constant_transform.R` | Minimal reproducible example |

---

## 🔧 Installation
```r
# 1. Clone the repo
> git clone https://github.com/YunkeW/iBART.git
> setwd("iBART")

# 2. Install dependencies (Java ≥8 required for bartMachine)
> install.packages(c("glmnet", "bartMachine", "foreach"))

# 3. Source or install as a package
> source("iBART.R")               # quick use in a script
```
*On Windows, set Java heap size before using `bartMachine`:*
```r
options(java.parameters = "-Xmx4g")
```

---

## 🚀 Quick Start
```r
library(iBART)
library(bartMachine)

set.seed(123)
# Simulate toy data
n <- 200; p <- 6
X <- matrix(runif(n*p, -1, 1), n, p)
colnames(X) <- paste0("x", 1:p)
y <- 10*exp(X[,1]) - 5*X[,2]^2 + rnorm(n)

# Run iBART with default settings
res <- iBART(X = X, y = y,
             opt = c("binary", "unary", "binary"),
             Lzero = TRUE, K = 5,
             constant_transformation = TRUE,
             verbose = TRUE)

print(res$descriptor_names)   # selected descriptors
print(res$coefficients)       # linear model coefficients
```
---

## 📝 Citing
If you use this code, please cite:
> Ye S, Senftle TP, Li M. Operator‑induced structural variable selection for identifying materials genes. *arXiv* 2110.10195 (2023).

---


