# missing function documentation: please add
simulate_fast <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  set.seed(seed)
  design <- model.matrix(~ ., data = data)
  expected <- design %*% true_coef
  n <- nrow(data)
  y_large <- rep(expected, reps) + rt(n*reps, df = df)
  coefs_mat <- matrix(nrow = ncol(data)+1, ncol = reps)
  for (rep in seq(reps)) {
    start_index <- 1 + (rep-1)*n
    y <- y_large[start_index:(start_index+n-1)]
    coefs <- lm.fit(design,y)$coefficients
    coefs_mat[,rep] <- unname(coefs)
  }
  return(structure(coefs_mat, seed = seed))
}




