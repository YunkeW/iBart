descriptorGenerator <- function(data = NULL,
                                opt = "binary",
                                sin_cos = FALSE,
                                apply_pos_opt_on_neg_x = TRUE, 
                                allowed_ops = c("multiply", "divide", "sqrt", "inverse", "square", "log", "sin", "cos"), # Allowed Operators list
                                verbose = TRUE) {
  if (opt == "binary") {
    if (verbose) cat("Constructing descriptors using binary operators... \n")
    data <- binary(data, sin_cos, allowed_ops)
  } else if (opt == "unary") {
    if (verbose) cat("Constructing descriptors using unary operators... \n")
    data <- unary(data, sin_cos, apply_pos_opt_on_neg_x, allowed_ops)
  } else {
    if (verbose) cat("Constructing descriptors using all operators... \n")
    data_unary <- unary(data, sin_cos, apply_pos_opt_on_neg_x, allowed_ops)
    data_binary <- binary(data, sin_cos, allowed_ops)
    data$X <- cbind(data_unary$X, data_binary$X)
    data$name <- c(data_unary$name, data_binary$name)
    data$unit <- cbind(data_unary$unit, data_binary$unit)
    data <- dataprocessing(data)
  }
  return(data)
}
