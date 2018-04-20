generate_data = function(
    n = 1000,
    beta = c(1.0, 2.0, -1.0, -2.0, 0.0, 0.0, 0.0, 0.0),
    epsilon_sd = 2,
    X_min = -1.0,
    X_max =  1.0,
    intercept = TRUE
){
    d = length(beta)
    epsilon = rnorm(n = n, mean = 0, sd = epsilon_sd)
    X = matrix(runif(n = n*d, min = -1.0, max = 1.0), nrow = n, ncol = d)
    if(intercept == TRUE){
        X[, 1] = 1
    }
    Y = X%*%beta + epsilon
    out = list(
        Y = Y,
        X = X[, -1]
    )
    return(out)
}

# data_set = generate_data()
# Y = data_set$Y
# X = data_set$X
