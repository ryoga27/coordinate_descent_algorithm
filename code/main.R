# setwd("YOUR_PATH")
source("generate_data.R")
source("penalties.R")
source("coordinate_descent_algorithm.R")
source("leave_one_out.R")

data_set = generate_data(n = 100)
Y = data_set$Y
X = data_set$X
lambdas = seq(0.0, 1.0, length = 10)
fit_lasso = leave_one_out(Y, X, lambdas = lambdas, penalty = "lasso")
fit_scad  = leave_one_out(Y, X, lambdas = lambdas, penalty = "scad")
fit_lasso$opt_result
fit_scad$opt_result
