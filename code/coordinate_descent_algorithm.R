coordinate_descent_algolithm = function(
    Y,
    X,
    penalty = "lasso",
    lambda,
    gamma,
    beta_init = NA,
    iter_max = 1000,
    eps = 1e-3
){
    n = length(Y)
    d = ncol(X)

    y_mean = mean(y)
    y_sd   = sd(y)
    y      = Y - mean(Y)

    x = scale(X)
    X_mean = attr(x, "scaled:center")
    X_sd   = attr(x, "scaled:scale")
    x_colnames = colnames(X)
    if(is.null(x_colnames)){
        x_colnames = rep(0, d)
        for(j in 1:d){
            x_colnames[j] = paste("X", j, sep = "")
        }
    }

    list_init = list(beta_init = beta_init, lambda = lambda, iter_max = iter_max, intercept = intercept)
    beta_list = list()
    obj_list  = list()
    args_list = list()

    if(all(is.na(beta_init))){
        beta = rep(0, d)
    }
    if(all(!is.na(beta_init))){
        beta = beta_init[-1]*X_sd
    }

    beta_list[[1]] = beta

    if(penalty == "lasso"){
        penalty_function = lasso_penalty_function
        penalty_solution = lasso_penalty_solution
    }
    if(penalty == "scad"){
        penalty_function = scad_penalty_function
        penalty_solution = scad_penalty_solution
    }

    obj = rep(0, iter_max)
    obj[1] = (1/n)*sum((y - x%*%beta)^2) + penalty_function(beta, lambda)
    for(s in 1:iter_max){
        for(j in 1:d){
            r = y - x[, -j]%*%beta[-j]
            z = (1/n)*x[, j]%*%r
            beta[j] = penalty_solution(z, lambda = lambda, gamma = gamma)
        }
        beta_list[[s + 1]] = beta
        obj[s + 1] = (1/n)*sum((y - x%*%beta)^2) + penalty_function(beta, lambda)
        convergence = (abs(obj[s + 1] - obj[s]) < eps)
        if(convergence == TRUE){
            beta = beta_list[[s]]
            obj = obj[1:s+1]
            break
        }
    }

    beta_hat = beta/X_sd
    const = mean(Y - X%*%beta_hat)

    coefficients = matrix(c(const, beta_hat), nrow = d + 1, ncol = 1)
    rownames(coefficients) = c("(Intercept)", x_colnames)
    colnames(coefficients) = "coefficients"

    args_list = list(
        coefficients = coefficients,
        lambda = lambda
    )

    return(args_list)
}
