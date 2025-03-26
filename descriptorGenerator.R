descriptorGenerator <- function(data = NULL,
                                y,
                                opt = "binary",
                                sin_cos = FALSE,
                                apply_pos_opt_on_neg_x = TRUE, 
                                allowed_ops = c("multiply", "divide", "sqrt", "inverse", "square", "log", "sin", "cos"),
                                verbose = TRUE,
                                cor_threshold = 0.2,
                                grid_seq = seq(-1, 1, by = 0.5)) {
  if (opt == "binary") {
    if (verbose) cat("Constructing descriptors using binary operators... \n")
    data <- binary(data, y, allowed_ops, sin_cos)
  } else if (opt == "unary") {
    if (verbose) cat("Constructing descriptors using unary operators... \n")
    data <- unary(data, y, sin_cos, apply_pos_opt_on_neg_x, cor_threshold, grid_seq)
  } else {
    if (verbose) cat("Constructing descriptors using all operators... \n")
    data_unary <- unary(data, y, sin_cos, apply_pos_opt_on_neg_x, cor_threshold, grid_seq)
    data_binary <- binary(data, y, allowed_ops, sin_cos)
  
    data$X <- cbind(data_unary$X, data_binary$X)
    data$name <- c(data_unary$name, data_binary$name)
    data$unit <- cbind(data_unary$unit, data_binary$unit)
    data <- dataprocessing(data)
  }

  return(data)
}
