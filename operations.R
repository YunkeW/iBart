##### Binary Operations #####
ADD <- function(dat) {
  if (is.null(dat$unit)) {
    p <- ncol(dat$X)
    df_out <- list(X = matrix(0, nrow = nrow(dat$X), ncol = choose(p, 2)),
                   unit = NULL,
                   name = c())
    count <- 1

    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        df_out$X[, count] <- dat$X[, i] + dat$X[, j]
        df_out$name[count] <- paste0("(", dat$name[i], "+", dat$name[j], ")")
        count <- count + 1
      }
    }
    # colnames(df_out$X) <- df_out$name
    return(df_out)

  } else {
    # 1. Only perform + on variables with the same unit: X_unit + X_unit
    # 2. Can perform X_unit + X_const
    # 3. Can perform X_unitless + X_unitless

    # Find X_unitless
    X_unitless <- NULL
    name_unitless <- c()
    unitless_col <- apply(dat$unit, 2, function(x) all(x == 0))
    p_unitless <- sum(unitless_col)

    if (p_unitless > 0) {
      X_unitless <- as.matrix(dat$X[, unitless_col])
      name_unitless <- dat$name[unitless_col]

      # Find X_const
      const_col <- apply(X_unitless, 2, function(x) length(unique(x)) == 1)
      p_const <- sum(const_col)
      if (p_const > 0) {
        X_const <- as.matrix(X_unitless[, const_col])
        name_const <- dat$name[const_col]
      }
    } else {
      X_const <- NULL
      p_const <- 0
    }

    # Find unique units
    unit_var <- as.matrix(dat$unit[, !unitless_col])
    unit_unique <- as.matrix(unique(unit_var, MARGIN = 2))

    # Count number of X_unit + X_unit
    unit_var_str <- apply(unit_var, 2, paste, collapse = ",")
    p_var <- as.vector(table(unit_var_str))
    p_1 <- sapply(p_var, function(x) if (x > 1) choose(x, 2) else 0)
    p_1 <- sum(p_1)

    # Count number of X_unit + X_const
    p_2 <- sum(p_var * p_const)

    # Count number of X_unitless + X_unitless
    p_3 <- if (p_unitless > 1) choose(p_unitless, 2) else 0

    p_total <- p_1 + p_2 + p_3

    # Initialize df_out to store results
    df_out <- list(X = matrix(nrow = nrow(dat$X), ncol = p_total),
                   unit = NULL,
                   name = c())
    count <- 1

    # Iterative over unique units
    for (i in 1:ncol(unit_unique)) {
      unit_tmp <- unit_unique[, i]
      unit_id <- apply(dat$unit, 2, function(x) all(x == unit_tmp))
      X_tmp <- as.matrix(dat$X[, unit_id])
      name_tmp <- dat$name[unit_id]
      p_tmp <- ncol(X_tmp)

      #################
      # X_unit + X_unit
      #################
      # Only + if there are at least 2 columns
      if (p_tmp > 1) {
        for (j in 1:(p_tmp-1)) {
          for (k in (j+1):p_tmp) {
            df_out$X[, count] <- X_tmp[, j] + X_tmp[, k]
            df_out$name[count] <- paste0("(", name_tmp[j], "+", name_tmp[k], ")")
            count <- count + 1
          }
        }
        p_tmp2 <- choose(p_tmp, 2)
        df_out$unit <- cbind(df_out$unit,
                             matrix(rep(unit_tmp, p_tmp2), ncol = p_tmp2))
      }
      ##################
      # X_unit + X_const
      ##################
      # Only + if there are at least 1 column in both X_tmp and X_const
      if ((p_const > 0) & (p_tmp > 0)) {
        for (j in 1:p_tmp) {
          for (k in 1:p_const){
            df_out$X[, count] <- X_tmp[, j] + X_const[, k]
            df_out$name[count] <- paste0("(", name_tmp[j], "+", name_const[k], ")")
            count <- count + 1
          }
        }
        p_tmp3 <- p_tmp * p_const
        df_out$unit <- cbind(df_out$unit,
                             matrix(rep(unit_tmp, p_tmp3), ncol = p_tmp3))
      }
    }

    #########################
    # X_unitless + X_unitless
    #########################
    if (p_unitless > 1) {
      for (i in 1:(p_unitless-1)) {
        for (j in (i+1):p_unitless) {
          df_out$X[, count] <- X_unitless[, i] + X_unitless[, j]
          df_out$name[count] <- paste0("(", name_unitless[i], "+", name_unitless[j], ")")
          count <- count + 1
        }
      }
      df_out$unit <- cbind(df_out$unit,
                           matrix(0, nrow = nrow(df_out$unit), ncol = choose(p_unitless, 2)))
    }
    # colnames(df_out$X) <- df_out$name
    return(df_out)
  }
}

MINUS <- function(dat) {
  if (is.null(dat$unit)) {
    p <- ncol(dat$X)
    df_out <- list(X = matrix(0, nrow = nrow(dat$X), ncol = choose(p, 2)),
                   unit = NULL,
                   name = c())
    count <- 1

    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        df_out$X[, count] <- dat$X[, i] - dat$X[, j]
        df_out$name[count] <- paste0("(", dat$name[i], "-", dat$name[j], ")")
        count <- count + 1
      }
    }
    # colnames(df_out$X) <- df_out$name
    return(df_out)

  } else {
    # 1. Only perform - on variables with the same unit: X_unit - X_unit
    # 2. Can perform X_unit - X_const
    # 3. Can perform X_unitless - X_unitless

    # Find X_unitless
    X_unitless <- NULL
    name_unitless <- c()
    unitless_col <- apply(dat$unit, 2, function(x) all(x == 0))
    p_unitless <- sum(unitless_col)

    if (p_unitless > 0) {
      X_unitless <- as.matrix(dat$X[, unitless_col])
      name_unitless <- dat$name[unitless_col]

      # Find X_const
      const_col <- apply(X_unitless, 2, function(x) length(unique(x)) == 1)
      p_const <- sum(const_col)
      if (p_const > 0) {
        X_const <- as.matrix(X_unitless[, const_col])
        name_const <- dat$name[const_col]
      }
    } else {
      X_const <- NULL
      p_const <- 0
    }

    # Find unique units
    unit_var <- as.matrix(dat$unit[, !unitless_col])
    unit_unique <- as.matrix(unique(unit_var, MARGIN = 2))

    # Count number of X_unit - X_unit
    unit_var_str <- apply(unit_var, 2, paste, collapse = ",")
    p_var <- as.vector(table(unit_var_str))
    p_1 <- sapply(p_var, function(x) if (x > 1) choose(x, 2) else 0)
    p_1 <- sum(p_1)

    # Count number of X_unit - X_const
    p_2 <- sum(p_var * p_const)

    # Count number of X_unitless - X_unitless
    p_3 <- if (p_unitless > 1) choose(p_unitless, 2) else 0

    p_total <- p_1 + p_2 + p_3

    # Initialize df_out to store results
    df_out <- list(X = matrix(nrow = nrow(dat$X), ncol = p_total),
                   unit = NULL,
                   name = c())
    count <- 1

    # Iterative over unique units
    for (i in 1:ncol(unit_unique)) {
      unit_tmp <- unit_unique[, i]
      unit_id <- apply(dat$unit, 2, function(x) all(x == unit_tmp))
      X_tmp <- as.matrix(dat$X[, unit_id])
      name_tmp <- dat$name[unit_id]
      p_tmp <- ncol(X_tmp)

      #################
      # X_unit - X_unit
      #################
      # Only - if there are at least 2 columns
      if (p_tmp > 1) {
        for (j in 1:(p_tmp-1)) {
          for (k in (j+1):p_tmp) {
            df_out$X[, count] <- X_tmp[, j] - X_tmp[, k]
            df_out$name[count] <- paste0("(", name_tmp[j], "-", name_tmp[k], ")")
            count <- count + 1
          }
        }
        p_tmp2 <- choose(p_tmp, 2)
        df_out$unit <- cbind(df_out$unit,
                             matrix(rep(unit_tmp, p_tmp2), ncol = p_tmp2))
      }
      ##################
      # X_unit - X_const
      ##################
      # Only - if there are at least 1 column in both X_tmp and X_const
      if ((p_const > 0) & (p_tmp > 0)) {
        for (j in 1:p_tmp) {
          for (k in 1:p_const){
            df_out$X[, count] <- X_tmp[, j] - X_const[, k]
            df_out$name[count] <- paste0("(", name_tmp[j], "-", name_const[k], ")")
            count <- count + 1
          }
        }
        p_tmp3 <- p_tmp * p_const
        df_out$unit <- cbind(df_out$unit,
                             matrix(rep(unit_tmp, p_tmp3), ncol = p_tmp3))
      }
    }

    #########################
    # X_unitless - X_unitless
    #########################
    if (p_unitless > 1) {
      for (i in 1:(p_unitless-1)) {
        for (j in (i+1):p_unitless) {
          df_out$X[, count] <- X_unitless[, i] - X_unitless[, j]
          df_out$name[count] <- paste0("(", name_unitless[i], "-", name_unitless[j], ")")
          count <- count + 1
        }
      }
      df_out$unit <- cbind(df_out$unit,
                           matrix(0, nrow = nrow(df_out$unit), ncol = choose(p_unitless, 2)))
    }
    # colnames(df_out$X) <- df_out$name
    return(df_out)
  }
}

MULTI <- function(dat) {
  p <- ncol(dat$X)
  df_out <- list(X = matrix(0, nrow = nrow(dat$X), ncol = choose(p, 2)),
                 unit = if (is.null(dat$unit)) NULL else matrix(0, nrow = nrow(dat$unit), ncol = choose(p, 2)),
                 name = c())
  count <- 1
  if (is.null(dat$unit)) {
    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        df_out$X[, count] <- dat$X[, i] * dat$X[, j]
        df_out$name[count] <- paste0("(", dat$name[i], "*", dat$name[j], ")")
        count <- count + 1
      }
    }
  } else {
    for (i in 1:(p-1)) {
      for (j in (i+1):p) {
        df_out$X[, count] <- dat$X[, i] * dat$X[, j]
        df_out$unit[, count] <- dat$unit[, i] + dat$unit[, j]
        df_out$name[count] <- paste0("(", dat$name[i], "*", dat$name[j], ")")
        count <- count + 1
      }
    }
  }
  # colnames(df_out$X) <- df_out$name
  return(df_out)
}

MULTI_const_opt <- function(data, y, candidate_constants = seq(-1, 1, by = 0.5), heuristic_threshold = 0.3) {
  n <- nrow(data$X)
  p <- ncol(data$X)
  
  df_out <- list(X = NULL, name = c(), unit = NULL)

  count <- 1

  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      best_corr <- -Inf
      best_feature <- NULL
      best_name <- NULL
      best_unit <- NULL
      best_c <- 0
      
      # Variant (x1+c)*x2
      for (c in candidate_constants) {
        feature_temp <- (data$X[, i] + c) * data$X[, j]
        corr_temp <- abs(cor(feature_temp, y))

        if (corr_temp > best_corr) {
          best_corr <- corr_temp
          best_feature <- feature_temp
          best_name <- paste0("(", data$name[i], "+", c, ")*", data$name[j])
          best_unit <- if (!is.null(data$unit)) data$unit[, i] + data$unit[, j] else NULL
          best_c <- c
        }
      }

      # Check if heuristic passes
      if (best_corr >= heuristic_threshold) {
        # Optimize constant with BFGS
        bfgs_result <- optim(par = best_c, fn = function(c) {
          feature <- (data$X[, i] + c) * data$X[, j]
          model <- lm(y ~ feature)
          sqrt(mean(residuals(model)^2))
        }, method = "BFGS")

        final_c <- bfgs_result$par
        final_feature <- (data$X[, i] + final_c) * data$X[, j]
        final_name <- paste0("(", data$name[i], "+", round(final_c, 4), ")*", data$name[j])

        df_out$X <- cbind(df_out$X, final_feature)
        df_out$name <- c(df_out$name, final_name)
        if (!is.null(data$unit)) {
          df_out$unit <- cbind(df_out$unit, best_unit)
        }
        count <- count + 1
      }

    }
  }

  return(df_out)
}


DIVD <- function(dat) {
  # Find columns that have 0
  # Put nonzero columns first, then zero columns
  p <- ncol(dat$X)
  col_nonzero <- which(apply(dat$X, 2, function(x) all(x != 0)))
  col_zero <- setdiff(1:p, col_nonzero)
  dat$X <- dat$X[, c(col_nonzero, col_zero)]
  dat$unit <- dat$unit[, c(col_nonzero, col_zero)]
  dat$name <- dat$name[c(col_nonzero, col_zero)]

  # Record which are nonzero and which are zero
  p_nonzero <- length(col_nonzero)
  p_zero <- ncol(dat$X) - p_nonzero
  p_total <- p_nonzero * (p_nonzero - 1) + p_nonzero * p_zero
  if (p_total == 0) {
    cat("There is no column that is strictly nonzero. Cannot perform division.")
    return(list(X = NULL, unit = NULL, name = c()))
  }

  df_out <- list(X = matrix(nrow = nrow(dat$X), ncol = p_total),
                 unit = if (is.null(dat$unit)) NULL else matrix(nrow = nrow(dat$unit), ncol = p_total),
                 name = c())
  count <- 1

  if (is.null(dat$unit)) {
    #### Operation 1: on X.no.zero ###
    if (p_nonzero > 1) {
      for (i in 1:(p_nonzero - 1)) {
        for (j in (i + 1):p_nonzero) {
          df_out$X[, count] <- dat$X[, i] / dat$X[, j]
          df_out$name[count] <- paste0("(", dat$name[i], "/", dat$name[j], ")")
          count <- count + 1
          df_out$X[, count] <- dat$X[, j] / dat$X[, i]
          df_out$name[count] <- paste0("(", dat$name[j], "/", dat$name[i], ")")
          count <- count + 1
        }
      }
    }

    #### Operation 2: on X.some.zero ####
    if ((p_nonzero > 0) & (p_zero > 0)) {
      for (i in (p_nonzero + 1):p) {
        for (j in 1:p_nonzero) {
          df_out$X[, count] <- dat$X[, i] / dat$X[, j]
          df_out$name[count] <- paste0("(", dat$name[i], "/", dat$name[j], ")")
          count <- count + 1
        }
      }
    }
  } else {
    #### Operation 1: on X.no.zero ###
    if (p_nonzero > 1) {
      for (i in 1:(p_nonzero - 1)) {
        for (j in (i + 1):p_nonzero) {
          df_out$X[, count] <- dat$X[, i] / dat$X[, j]
          df_out$unit[, count] <- dat$unit[, i] - dat$unit[, j]
          df_out$name[count] <- paste0("(", dat$name[i], "/", dat$name[j], ")")
          count <- count + 1
          df_out$X[, count] <- dat$X[, j] / dat$X[, i]
          df_out$unit[, count] <- dat$unit[, j] - dat$unit[, i]
          df_out$name[count] <- paste0("(", dat$name[j], "/", dat$name[i], ")")
          count <- count + 1
        }
      }
    }

    #### Operation 2: on X.some.zero ####
    if ((p_nonzero > 0) & (p_zero > 0)) {
      for (i in (p_nonzero + 1):p) {
        for (j in 1:p_nonzero) {
          df_out$X[, count] <- dat$X[, i] / dat$X[, j]
          df_out$unit[, count] <- dat$unit[, i] - dat$unit[, j]
          df_out$name[count] <- paste0("(", dat$name[i], "/", dat$name[j], ")")
          count <- count + 1
        }
      }
    }
  }

  # colnames(df_out$X) <- df_out$name
  return(df_out)
}

DIVD_const_opt <- function(dat, y, candidate_constants = seq(-1, 1, by=0.5), corr_threshold = 0.1){

  X <- dat$X
  p <- ncol(X)
  
  # Store results
  optimized_features <- list(X = NULL, name = c(), unit = NULL)

  for(i in 1:(p-1)){
    for(j in (i+1):p){

      # Original descriptors
      original_feature_1 <- X[,i] / X[,j]
      original_feature_2 <- X[,j] / X[,i]

      best_corr1 <- abs(cor(original_feature_1, y, use="complete.obs"))
      best_corr2 <- abs(cor(original_feature_2, y, use="complete.obs"))
      best_feature1 <- original_feature_1
      best_feature2 <- original_feature_2
      best_c1 <- 0
      best_c2 <- 0

      # Constant optimization
      for(c in candidate_constants){
        candidate_feature_1 <- (X[,i] + c) / X[,j]
        candidate_feature_2 <- X[,i] / (X[,j] + c)

        corr1 <- abs(cor(candidate_feature_1, y, use="complete.obs"))
        corr2 <- abs(cor(candidate_feature_2, y, use="complete.obs"))

        if(corr1 > best_corr1 + corr_threshold){
          best_corr1 <- corr1
          best_feature1 <- candidate_feature_1
          best_c1 <- c
        }

        if(corr2 > best_corr2 + corr_threshold){
          best_corr2 <- corr2
          best_feature2 <- candidate_feature_2
          best_c2 <- c
        }
      }

      # Append optimized features
      optimized_features$X <- cbind(optimized_features$X, best_feature1, best_feature2)
      optimized_features$name <- c(optimized_features$name,
                                   paste0("(",dat$name[i],"+",best_c1,")/",dat$name[j]),
                                   paste0(dat$name[i],"/(",dat$name[j],"+",best_c2,")"))
      if(!is.null(dat$unit)){
        optimized_features$unit <- cbind(optimized_features$unit,
                                         dat$unit[,i]-dat$unit[,j],
                                         dat$unit[,i]-dat$unit[,j])
      }

    }
  }

  return(optimized_features)
}

binary <- function(data, y, allowed_ops, sin_cos = FALSE) {
  p <- ncol(data$X)
  if (p < 2) {
    message("X has less than 2 columns. Need at least 2 columns to perform binary operations!")
    return(data)
  } else {

    data_binary <- data

    # ADD
    if ("add" %in% allowed_ops) {
      data_tmp <- ADD(data)
      data_binary$X <- data_tmp$X
      data_binary$unit <- data_tmp$unit
      data_binary$name <- data_tmp$name
    }

    # MINUS
    if ("minus" %in% allowed_ops) {
      data_minus <- MINUS(data)
      data_binary$X <- cbind(data_binary$X, data_minus$X)
      data_binary$unit <- cbind(data_binary$unit, data_minus$unit)
      data_binary$name <- c(data_binary$name, data_minus$name)
    }

    # MULTI with constant optimization
    if ("multiply" %in% allowed_ops) {
      data_tmp <- MULTI(data)
      data_binary$X <- cbind(data_binary$X, data_tmp$X)
      data_binary$unit <- cbind(data_binary$unit, data_tmp$unit)
      data_binary$name <- c(data_binary$name, data_tmp$name)

      # Constant Optimization for MULTI
      data_tmp_const <- MULTI_const_opt(data, y)
      data_binary$X <- cbind(data_binary$X, data_tmp_const$X)
      data_binary$name <- c(data_binary$name, data_tmp_const$name)
    }

    # DIVD with constant optimization
    if ("divide" %in% allowed_ops) {
      data_tmp <- DIVD(data)
      data_binary$X <- cbind(data_binary$X, data_tmp$X)
      data_binary$unit <- cbind(data_binary$unit, data_tmp$unit)
      data_binary$name <- c(data_binary$name, data_tmp$name)

      # Constant Optimization for DIVD
      data_tmp_const <- DIVD_const_opt(data, y)
      data_binary$X <- cbind(data_binary$X, data_tmp_const$X)
      data_binary$name <- c(data_binary$name, data_tmp_const$name)
    }

    # MINUS_ABS
    if ("abs" %in% allowed_ops) {
      data_tmp <- ABS(data_minus)
      data_binary$X <- cbind(data_binary$X, data_tmp$X)
      data_binary$unit <- cbind(data_binary$unit, data_tmp$unit)
      data_binary$name <- c(data_binary$name, data_tmp$name)
    }

    # Remove redundant descriptors
    data_binary$name <- unname(data_binary$name)
    colnames(data_binary$X) <- data_binary$name
    data_binary <- dataprocessing(data_binary)

    cat(c(paste0("Finished building X.binary... Initial p = ", p, "; New p = ", ncol(data_binary$X)), "\n"))
    return(data_binary)
  }
}

##### Unary Operations #####
ABS <- function(dat) {
  dat$X <- abs(dat$X)
  dat$name <- unname(sapply(dat$name, function(x) paste0("abs(", x, ")")))
  # colnames(dat$X) <- dat$name
  return(dat)
}

SQRT <- function(dat, y, apply_pos_opt_on_neg_x = TRUE, heuristic_threshold = 0.1, 
                 candidate_constants = seq(-1, 1, by = 0.5)) {
  
  optimize_constant_sqrt <- function(x, y, candidate_constants, heuristic_threshold) {
    best_corr <- -Inf
    best_c <- 0
    
    for (c in candidate_constants) {
      new_feature <- sqrt(abs(x + c))
      if (any(is.na(new_feature) | is.infinite(new_feature))) next
      
      corr <- abs(cor(new_feature, y, use = "complete.obs"))
      if (!is.na(corr) && corr > best_corr) {
        best_corr <- corr
        best_c <- c
      }
    }
    
    if (best_corr < heuristic_threshold) {
      return(list(feature = NULL, c = NULL))
    }
    
    # Define optimization function
    opt_func <- function(c) {
      new_x <- sqrt(abs(x + c))
      -abs(cor(new_x, y, use = "complete.obs"))
    }
    
    # Optimize using BFGS
    optim_result <- optim(par = best_c, fn = opt_func, method = "BFGS")
    optimal_c <- optim_result$par
    final_feature <- sqrt(abs(x + optimal_c))
    
    list(feature = final_feature, c = optimal_c)
  }
  
  X_new <- list()
  names_new <- c()
  
  for (i in 1:ncol(dat$X)) {
    x <- dat$X[, i]
    result <- optimize_constant_sqrt(x, y, candidate_constants, heuristic_threshold)
    
    if (!is.null(result$feature)) {
      X_new[[length(X_new) + 1]] <- result$feature
      names_new <- c(names_new, paste0("sqrt(", dat$name[i], "+", round(result$c, 3), ")"))
    } else if (apply_pos_opt_on_neg_x) {
      new_x <- sqrt(abs(x))
      X_new[[length(X_new) + 1]] <- new_x
      names_new <- c(names_new, paste0("sqrt(abs(", dat$name[i], "))"))
    }
  }
  
  if (length(X_new) == 0) return(list(X = NULL, unit = NULL, name = NULL))
  
  dat$X <- do.call(cbind, X_new)
  dat$name <- names_new
  
  if (!is.null(dat$unit)) {
    dat$unit <- matrix(0, nrow = nrow(dat$unit), ncol = length(names_new))
  }
  
  return(dat)
}


INV <- function(dat, y, cor_threshold = 0.2, grid_seq = seq(-1, 1, by = 0.5)) {
  p <- ncol(dat$X)
  
  X_new <- list()
  name_new <- c()
  unit_new <- if (is.null(dat$unit)) NULL else list()
  
  count <- 1
  
  for (i in 1:p) {
    
    x_current <- dat$X[, i]
    if (any(x_current == 0)) next  # Skip zero-containing columns
    
    # Original inverse descriptor without constant
    plain_feature <- 1 / x_current
    if (any(is.nan(plain_feature) | is.infinite(plain_feature))) next
    
    cor_plain <- abs(cor(plain_feature, y))
    
    if (cor_plain >= cor_threshold) {
      
      best_cor <- cor_plain
      best_feature <- plain_feature
      best_c <- 0  # No constant initially
      
      # Coarse grid search for constant optimization
      for (c in grid_seq) {
        candidate_feature <- 1 / (x_current + c)
        if (any(is.nan(candidate_feature) | is.infinite(candidate_feature))) next
        
        candidate_cor <- abs(cor(candidate_feature, y))
        
        if (candidate_cor > best_cor) {
          best_cor <- candidate_cor
          best_feature <- candidate_feature
          best_c <- c
        }
      }
      
      # BFGS refinement if improvement found
      if (best_c != 0) {
        optim_result <- optim(par = best_c,
                              fn = function(c) {
                                candidate_feature <- 1 / (x_current + c)
                                if (any(is.nan(candidate_feature) | is.infinite(candidate_feature))) return(Inf)
                                -abs(cor(candidate_feature, y))
                              },
                              method = "BFGS")
        
        final_c <- optim_result$par
        final_feature <- 1 / (x_current + final_c)
        
        if (!any(is.nan(final_feature) | is.infinite(final_feature))) {
          best_feature <- final_feature
          best_c <- final_c
        }
      }
      
      # Store optimized feature
      X_new[[count]] <- best_feature
      descriptor_name <- paste0("1/(", dat$name[i], ifelse(best_c != 0, paste0(" + ", round(best_c, 4)), ""), ")")
      name_new[count] <- descriptor_name
      if (!is.null(dat$unit)) unit_new[[count]] <- -dat$unit[, i]
      count <- count + 1
    }
  }
  
  # Final checks
  if (length(X_new) == 0) {
    message("No features passed the heuristic threshold for INV operation.")
    return(list(X = NULL, name = NULL, unit = NULL))
  }
  
  X_matrix <- do.call(cbind, X_new)
  unit_matrix <- if (!is.null(dat$unit)) do.call(cbind, unit_new) else NULL
  colnames(X_matrix) <- name_new
  
  return(list(X = X_matrix, name = name_new, unit = unit_matrix))
}

SQRE <- function(dat, y, heuristic_threshold = 0.1, 
                 candidate_constants = seq(-1, 1, by = 0.5)) {
  
  optimize_constant_sqre <- function(x, y, candidate_constants, heuristic_threshold) {
    best_corr <- -Inf
    best_c <- 0
    
    # Coarse grid search
    for (c in candidate_constants) {
      new_feature <- (x + c)^2
      if (any(is.na(new_feature) | is.infinite(new_feature))) next
      
      corr <- abs(cor(new_feature, y, use = "complete.obs"))
      if (!is.na(corr) && corr > best_corr) {
        best_corr <- corr
        best_c <- c
      }
    }
    
    # Check if heuristic threshold is met
    if (best_corr < heuristic_threshold) {
      return(list(feature = NULL, c = NULL))
    }
    
    # Define optimization function for BFGS
    opt_func <- function(c) {
      new_x <- (x + c)^2
      -abs(cor(new_x, y, use = "complete.obs"))
    }
    
    # BFGS optimization
    optim_result <- optim(par = best_c, fn = opt_func, method = "BFGS")
    optimal_c <- optim_result$par
    final_feature <- (x + optimal_c)^2
    
    list(feature = final_feature, c = optimal_c)
  }
  
  X_new <- list()
  names_new <- c()
  
  for (i in 1:ncol(dat$X)) {
    x <- dat$X[, i]
    result <- optimize_constant_sqre(x, y, candidate_constants, heuristic_threshold)
    
    if (!is.null(result$feature)) {
      X_new[[length(X_new) + 1]] <- result$feature
      names_new <- c(names_new, paste0("(", dat$name[i], "+", round(result$c, 3), ")^2"))
    } else {
      # Fallback to original squared feature if heuristic not met
      X_new[[length(X_new) + 1]] <- x^2
      names_new <- c(names_new, paste0("(", dat$name[i], ")^2"))
    }
  }
  
  dat$X <- do.call(cbind, X_new)
  dat$name <- names_new
  
  if (!is.null(dat$unit)) {
    dat$unit <- 2 * dat$unit
  }
  
  return(dat)
}


LOG <- function(dat, y, heuristic_threshold = 0.1, 
                apply_pos_opt_on_neg_x = TRUE,
                candidate_constants = seq(-1, 1, by = 0.5)) {

  optimize_constant_log <- function(x, y, candidate_constants, heuristic_threshold) {
    best_corr <- -Inf
    best_c <- 0

    # Coarse grid search
    for (c in candidate_constants) {
      new_feature <- log(abs(x + c))
      if (any(is.na(new_feature) | is.infinite(new_feature))) next

      corr <- abs(cor(new_feature, y, use = "complete.obs"))
      if (!is.na(corr) && corr > best_corr) {
        best_corr <- corr
        best_c <- c
      }
    }

    # Check heuristic threshold
    if (best_corr < heuristic_threshold) {
      return(list(feature = NULL, c = NULL))
    }

    # Define optimization function for BFGS
    opt_func <- function(c) {
      new_x <- log(abs(x + c))
      if (any(is.na(new_x) | is.infinite(new_x))) return(Inf)
      -abs(cor(new_x, y, use = "complete.obs"))
    }

    # BFGS optimization
    optim_result <- optim(par = best_c, fn = opt_func, method = "BFGS")
    optimal_c <- optim_result$par
    final_feature <- log(abs(x + optimal_c))

    list(feature = final_feature, c = optimal_c)
  }

  X_new <- list()
  names_new <- c()

  for (i in 1:ncol(dat$X)) {
    x <- dat$X[, i]
    
    # Handle zeros and negatives explicitly
    if (any(x + candidate_constants <= 0) && !apply_pos_opt_on_neg_x) next

    result <- optimize_constant_log(x, y, candidate_constants, heuristic_threshold)

    if (!is.null(result$feature)) {
      X_new[[length(X_new) + 1]] <- result$feature
      names_new <- c(names_new, paste0("log(abs(", dat$name[i], "+", round(result$c, 3), "))"))
    } else {
      # Original log feature if heuristic not passed
      safe_x <- ifelse(x <= 0 & apply_pos_opt_on_neg_x, abs(x), x)
      feature_plain <- log(safe_x)
      if (all(!is.na(feature_plain) & !is.infinite(feature_plain))) {
        X_new[[length(X_new) + 1]] <- feature_plain
        prefix <- ifelse(any(x <= 0 & apply_pos_opt_on_neg_x), "log(abs(", "log(")
        names_new <- c(names_new, paste0(prefix, dat$name[i], "))"))
      }
    }
  }

  if (length(X_new) == 0) return(list(X = NULL, unit = NULL, name = NULL))

  dat$X <- do.call(cbind, X_new)
  dat$name <- names_new

  if (!is.null(dat$unit)) {
    dat$unit[,] <- 0
  }

  return(dat)
}


EXP <- function(dat) {
  dat$X <- exp(dat$X)
  dat$name <- unname(sapply(dat$name, function(x) paste0("exp(", x, ")")))
  if (!is.null(dat$unit)) {
    dat$unit[,] <- 0
  }
  # colnames(dat$X) <- dat$name
  return(dat)
}

SIN <- function(dat, y, candidate_constants = seq(-1, 1, by = 0.5), heuristic_threshold = 0.2) {
  library(stats)

  n <- nrow(dat$X)
  p <- ncol(dat$X)
  
  X_new <- list()
  name_new <- c()
  unit_new <- NULL

  for (i in 1:p) {
    feature <- dat$X[, i]
    original_name <- dat$name[i]

    # Original transformation (no constant)
    plain_feature <- sin(pi * feature)
    X_new <- c(X_new, list(plain_feature))
    name_new <- c(name_new, paste0("sin(pi*", original_name, ")"))

    # Heuristic Check for sin(pi*(x + c))
    best_cor <- 0
    best_c <- NULL
    for (c in candidate_constants) {
      candidate_feature <- sin(pi * (feature + c))
      cor_val <- abs(cor(candidate_feature, y))
      if (!is.na(cor_val) && cor_val > best_cor) {
        best_cor <- cor_val
        best_c <- c
      }
    }

    if (!is.null(best_c) && best_cor >= heuristic_threshold) {
      # BFGS optimization
      obj_func <- function(c) {
        feature_opt <- sin(pi * (feature + c))
        -abs(cor(feature_opt, y))
      }

      opt_result <- optim(best_c, obj_func, method = "BFGS")
      optimal_c <- opt_result$par

      # Store optimized feature
      optimized_feature <- sin(pi * (feature + optimal_c))
      X_new <- c(X_new, list(optimized_feature))
      name_new <- c(name_new, paste0("sin(pi*(", original_name, sprintf("%+.3f", optimal_c), "))"))
    }

    # Heuristic Check for sin(c*x)
    best_cor2 <- 0
    best_c2 <- NULL
    for (c in candidate_constants) {
      candidate_feature <- sin(pi * (c * feature))
      cor_val <- abs(cor(candidate_feature, y))
      if (!is.na(cor_val) && cor_val > best_cor2) {
        best_cor2 <- cor_val
        best_c2 <- c
      }
    }

    if (!is.null(best_c2) && best_cor2 >= heuristic_threshold) {
      # BFGS optimization for sin(pi * c * x)
      obj_func2 <- function(c) {
        feature_opt <- sin(pi * (c * feature))
        -abs(cor(feature_opt, y))
      }

      opt_result2 <- optim(best_c2, obj_func2, method = "BFGS")
      optimal_c2 <- opt_result2$par

      # Store optimized feature
      optimized_feature2 <- sin(pi * (optimal_c2 * feature))
      X_new <- c(X_new, list(optimized_feature2))
      name_new <- c(name_new, paste0("sin(pi*", sprintf("%.3f", optimal_c2), "*", original_name, ")"))
    }
  }

  X_new_mat <- do.call(cbind, X_new)

  if (!is.null(dat$unit)) {
    unit_new <- matrix(0, nrow = nrow(dat$unit), ncol = ncol(X_new_mat))
  }

  return(list(X = X_new_mat, name = name_new, unit = unit_new))
}


COS <- function(dat, y, candidate_constants = seq(-1, 1, by = 0.5), heuristic_threshold = 0.2) {
  library(stats)

  n <- nrow(dat$X)
  p <- ncol(dat$X)
  
  X_new <- list()
  name_new <- c()
  unit_new <- NULL

  for (i in 1:p) {
    feature <- dat$X[, i]
    original_name <- dat$name[i]

    # Original transformation (no constant)
    plain_feature <- cos(pi * feature)
    X_new <- c(X_new, list(plain_feature))
    name_new <- c(name_new, paste0("cos(pi*", original_name, ")"))

    # Heuristic Check for cos(pi*(x + c))
    best_cor <- 0
    best_c <- NULL
    for (c in candidate_constants) {
      candidate_feature <- cos(pi * (feature + c))
      cor_val <- abs(cor(candidate_feature, y))
      if (!is.na(cor_val) && cor_val > best_cor) {
        best_cor <- cor_val
        best_c <- c
      }
    }

    if (!is.null(best_c) && best_cor >= heuristic_threshold) {
      # BFGS optimization
      obj_func <- function(c) {
        feature_opt <- cos(pi * (feature + c))
        -abs(cor(feature_opt, y))
      }

      opt_result <- optim(best_c, obj_func, method = "BFGS")
      optimal_c <- opt_result$par

      # Store optimized feature
      optimized_feature <- cos(pi * (feature + optimal_c))
      X_new <- c(X_new, list(optimized_feature))
      name_new <- c(name_new, paste0("cos(pi*(", original_name, sprintf("%+.3f", optimal_c), "))"))
    }

    # Heuristic Check for cos(c*x)
    best_cor2 <- 0
    best_c2 <- NULL
    for (c in candidate_constants) {
      candidate_feature <- cos(pi * (c * feature))
      cor_val <- abs(cor(candidate_feature, y))
      if (!is.na(cor_val) && cor_val > best_cor2) {
        best_cor2 <- cor_val
        best_c2 <- c
      }
    }

    if (!is.null(best_c2) && best_cor2 >= heuristic_threshold) {
      # BFGS optimization for cos(pi * c * x)
      obj_func2 <- function(c) {
        feature_opt <- cos(pi * (c * feature))
        -abs(cor(feature_opt, y))
      }

      opt_result2 <- optim(best_c2, obj_func2, method = "BFGS")
      optimal_c2 <- opt_result2$par

      # Store optimized feature
      optimized_feature2 <- cos(pi * (optimal_c2 * feature))
      X_new <- c(X_new, list(optimized_feature2))
      name_new <- c(name_new, paste0("cos(pi*", sprintf("%.3f", optimal_c2), "*", original_name, ")"))
    }
  }

  X_new_mat <- do.call(cbind, X_new)

  if (!is.null(dat$unit)) {
    unit_new <- matrix(0, nrow = nrow(dat$unit), ncol = ncol(X_new_mat))
  }

  return(list(X = X_new_mat, name = name_new, unit = unit_new))
}


unary <- function(data, y, sin_cos, apply_pos_opt_on_neg_x, cor_threshold = 0.2, grid_seq = seq(-1, 1, by=0.5)) {
  p <- ncol(data$X)

  if (p < 1) {
    stop("X has zero column. Need at least 1 column to perform unary operations!")
  } else {
    # Original unary operations without optimization
    data_abs <- ABS(data)
    data_exp <- EXP(data)

    # Unary operations with selective constant optimization
    data_sqrt <- SQRT(data, y, apply_pos_opt_on_neg_x, cor_threshold, grid_seq)
    data_sqre <- SQRE(data, y, cor_threshold, grid_seq)
    data_log <- LOG(data, y, apply_pos_opt_on_neg_x, cor_threshold, grid_seq)
    data_inv <- INV(data, y, cor_threshold, grid_seq)

    if (sin_cos) {
      data_sin <- SIN(data, y, cor_threshold, grid_seq)
      data_cos <- COS(data, y, cor_threshold, grid_seq)

      # Combine datasets
      data$X <- cbind(data$X, data_sqrt$X, data_sqre$X,
                      data_log$X, data_exp$X, data_sin$X,
                      data_cos$X, data_inv$X, data_abs$X)
      data$unit <- cbind(data$unit, data_sqrt$unit, data_sqre$unit,
                         data_log$unit, data_exp$unit, data_sin$unit,
                         data_cos$unit, data_inv$unit, data_abs$unit)
      data$name <- unname(c(data$name, data_sqrt$name, data_sqre$name,
                            data_log$name, data_exp$name, data_sin$name,
                            data_cos$name, data_inv$name, data_abs$name))
    } else {
      # Combine datasets without sin/cos
      data$X <- cbind(data$X, data_sqrt$X, data_sqre$X,
                      data_log$X, data_exp$X,
                      data_inv$X, data_abs$X)
      data$unit <- cbind(data$unit, data_sqrt$unit, data_sqre$unit,
                         data_log$unit, data_exp$unit,
                         data_inv$unit, data_abs$unit)
      data$name <- unname(c(data$name, data_sqrt$name, data_sqre$name,
                            data_log$name, data_exp$name,
                            data_inv$name, data_abs$name))
    }

    colnames(data$X) <- data$name

    # Remove redundant descriptors
    data <- dataprocessing(data)

    cat(c(paste0("Building X.unary... Initial p = ", p, "; New p = ", ncol(data$X)), "\n"))
    return(data)
  }
}

