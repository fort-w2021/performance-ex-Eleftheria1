# function simulates coefficients from linear regression model given 
# - number of sampled coefficients
# - data
# - seed
# - true coefficients
# - and degrees of freedom of the t distribution
# returns matrix of coefficient vectors as columns
simulate_fast <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  set.seed(seed)
  design <- model.matrix(~ ., data = data)
  expected <- design %*% true_coef
  n <- nrow(data)
  y_mat <- matrix(rep(expected, reps) + rt(n*reps, df = df),
                     nrow = n, ncol = reps, byrow = FALSE)
  coefs_mat <- apply(y_mat, 2, function(y){
    .lm.fit(design, y)$coefficients
  })
  return(structure(coefs_mat, seed = seed))
}


simulate_fast_future <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  set.seed(seed)
  design <- model.matrix(~ ., data = data)
  expected <- design %*% true_coef
  n <- nrow(data)
  y_mat <- matrix(rep(expected, reps) + rt(n*reps, df = df),
                  nrow = n, ncol = reps, byrow = FALSE)
  # add future::plan("multiprocess", workers = 2) before call
  coefs_mat <- future.apply::future_apply(y_mat, 2, function(y){
    .lm.fit(design, y)$coefficients
  })
  return(structure(coefs_mat, seed = seed))
}

