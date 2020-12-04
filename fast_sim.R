# function simulates coefficients from linear regression model given
# - number of sampled coefficients
# - data
# - seed
# - true coefficients
# - and degrees of freedom of the t distribution
# returns matrix of coefficient vectors as columns
simulate_fast <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  # input checks
  checkmate::assert_integerish(reps, lower = 1, any.missing = FALSE, len = 1)
  checkmate::assert_integerish(seed, lower = 0)
  checkmate::assert_data_frame(data, types = "numeric")
  # types set because of the default action of true_coef argument
  checkmate::assert_numeric(true_coef,
    finite = TRUE, any.missing = FALSE,
    len = ncol(data) + 1
  )
  checkmate::assert_integerish(df, lower = 0, len = 1, any.missing = FALSE)


  set.seed(seed)
  design <- model.matrix(~., data = data)
  expected <- design %*% true_coef
  prediction_matrix <- solve(crossprod(design)) %*% t(design)
  n <- nrow(data)
  # matrix with a column for every rep in reps and every column is a simulated y
  # vector
  y_matrix <- matrix(rep(expected, reps) + rt(n * reps, df = df),
    nrow = n, ncol = reps, byrow = FALSE
  )
  # loop over the simulated y's in the y_matrix (over the columns) and get the
  # predicted betas and store them in the desired matrix output format
  coefs_matrix <- apply(y_matrix, 2, function(y) {
    prediction_matrix %*% y
  })
  return(structure(coefs_matrix, seed = seed))
}



# paralellized with future package.
# same as above but in this case there seems to be to much overhead especially
# for multisession
# so it is much slower than the above solution!
simulate_fast_future <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  set.seed(seed)
  design <- model.matrix(~., data = data)
  expected <- design %*% true_coef
  n <- nrow(data)
  y_mat <- matrix(rep(expected, reps) + rt(n * reps, df = df),
    nrow = n, ncol = reps, byrow = FALSE
  )
  # add future::plan("multiprocess", workers = 2) before call
  coefs_mat <- future.apply::future_apply(y_mat, 2, function(y) {
    .lm.fit(design, y)$coefficients
  })
  return(structure(coefs_mat, seed = seed))
}
