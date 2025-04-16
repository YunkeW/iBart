# Install required packages if not already installed
if (!require("bartMachine")) {
  install.packages("bartMachine")
}
if (!require("glmnet")) {
  install.packages("glmnet")
}

# Source required files
source("iBART.R")
source("operations.R")
source("BART_iter.R")
source("LASSO.R")
source("L_zero_regression.R")
source("descriptorGenerator.R")
source("utilis.R")

# Set up test environment
set.seed(123)
options(java.parameters = "-Xmx2g") # Allocate 2GB of memory for Java

# Generate test data (smaller dataset)
n <- 100  # Reduced from 250
p <- 5    # Reduced from 10
X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
colnames(X) <- paste("x.", seq(from = 1, to = p, by = 1), sep = "")
y <- 15*(exp(X[,1])-exp(X[,2]))^2 + 20*sin(pi*X[,3]*X[,4]) + rnorm(n, mean = 0, sd = 0.5)

# Test iBART with constant transformations (simpler parameters)
iBART_results <- iBART(X = X, y = y,
                       name = colnames(X),
                       unit = NULL,                         # no unit information for simulation
                       constant_transformation = TRUE,      # enable constant transformations
                       const_transform_threshold = 0.3,     # lower threshold to test more features
                       const_grid_search = TRUE,           # use grid search
                       const_grid_values = c(0.5, 1, 2),   # fewer grid values
                       const_transform_types = c("mult"),   # only multiplicative transformations
                       opt = c("unary", "binary"),         # simpler operation sequence
                       sin_cos = TRUE,                      # add sin and cos to operator set
                       apply_pos_opt_on_neg_x = FALSE,      # e.g. do not apply log() on negative x
                       Lzero = TRUE,                        # best subset selection
                       K = 3,                               # fewer predictors
                       standardize = FALSE,                 # don't standardize input matrix X
                       num_trees = 10,                      # fewer trees
                       num_burn_in = 1000,                  # fewer burn-in iterations
                       num_iterations_after_burn_in = 1000, # fewer MCMC iterations
                       seed = 99)

# Print results
print("Selected descriptors:")
print(iBART_results$descriptor_names)
print("Coefficients:")
print(iBART_results$coefficients)
print("In-sample RMSE:")
print(iBART_results$iBART_in_sample_RMSE)

# Print the full formula
print("Full formula:")
formula_parts <- character(length(iBART_results$descriptor_names) + 1)
formula_parts[1] <- sprintf("%.4f", iBART_results$coefficients[1])  # Intercept
for (i in 1:length(iBART_results$descriptor_names)) {
  coef <- iBART_results$coefficients[i + 1]
  if (coef >= 0) {
    formula_parts[i + 1] <- sprintf("+ %.4f * %s", coef, iBART_results$descriptor_names[i])
  } else {
    formula_parts[i + 1] <- sprintf("- %.4f * %s", abs(coef), iBART_results$descriptor_names[i])
  }
}
full_formula <- paste(formula_parts, collapse = " ")
print(full_formula)

# Print true formula for comparison
print("True formula:")
print("15*(exp(x.1)-exp(x.2))^2 + 20*sin(pi*x.3*x.4)") 