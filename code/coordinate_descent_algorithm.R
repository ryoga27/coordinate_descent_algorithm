coordinate_descent_algolithm = function(
    Y,
    X,
    penalty = "lasso",
    lambda,
    gamma = 3.7,
    beta_init = NA,
    iter_max = 10000,
    eps = 1e-3
){
    n = length(Y)
    d = ncol(X)

    y_mean = mean(Y)
    y_sd   = sd(Y)
    y      = Y - y_mean

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

    beta_0_list = rep(0, iter_max)
    beta_list = list()
    obj_list  = list()
    args_list = list()

    if(all(is.na(beta_init))){
        beta = rep(0, d)
    }
    if(all(!is.na(beta_init))){
        beta = beta_init*X_sd
    }
    beta_list[[1]] = beta

    beta_0 = (1/n)*sum(y - x%*%beta)
    beta_0_list[1] = beta_0

    list_init = list(
        penalty = penalty,
        beta_init = beta_init,
        lambda = lambda,
        iter_max = iter_max
    )

    obj = rep(0, iter_max)
    obj[1] = (1/n)*sum((y - x%*%beta)^2) + penalty_function(beta, lambda, penalty = penalty)

    for(s in 1:iter_max){
        for(j in 1:d){
            r = y - beta_0 - x[, -j]%*%beta[-j]
            z = (1/n)*x[, j]%*%r
            beta[j] = penalty_solution(z, lambda = lambda, gamma = gamma, penalty = penalty)
        }
        beta_list[[s + 1]] = beta

        beta_0 = (1/n)*sum(y - x%*%beta)
        beta_0_list[s + 1] = beta_0

        obj[s + 1] = (1/n)*sum((y - beta_0 - x%*%beta)^2) + penalty_function(beta, lambda, penalty = penalty)
        convergence = (abs(obj[s + 1] - obj[s]) < eps)
        if(convergence == TRUE){
            times_iter = s
            break
        }
    }

    beta_0_hat = beta_0 + y_mean
    beta_hat = beta/X_sd

    coefficients = matrix(c(beta_0_hat, beta_hat), nrow = d + 1, ncol = 1)
    rownames(coefficients) = c("(Intercept)", x_colnames)
    colnames(coefficients) = "coefficients"

    args_list = list(
        coefficients = coefficients,
        lambda = lambda,
        times_iter = times_iter,
        init = list_init,
        process = list(
            beta_0 = beta_0_list,
            beta = beta_list,
            obj = obj_list
        )
    )
    return(args_list)
}
