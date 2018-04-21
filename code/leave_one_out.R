leave_one_out = function(Y, X, lambdas, penalty = "lasso"){

    n = length(Y)
    n_lambdas = length(lambdas)

    scores = rep(0, n_lambdas)
    result_list = list()

    for(l in 1:n_lambdas){
        lambda = lambdas[l]
        score = 0
        for(i in 1:n){
            Y_test  = Y[ i, ]
            Y_train = Y[-i, ]
            X_test  = X[ i, ]
            X_train = X[-i, ]
            result = coordinate_descent_algolithm(Y = Y_train, X = X_train, lambda = lambda, penalty = penalty)
            Y_pred = predict(result, X_test)
            score = score + (Y_pred - Y_test)^2
        }
        result_list[[l]] = coordinate_descent_algolithm(Y = Y, X = X, lambda = lambda, penalty = penalty)
        scores[l] = (1/n)*sqrt(score)
        cat("lambda: ", lambda, "\t", " score: ", scores[l], "\n")
    }
    opt_score = min(scores)
    opt_index = (1:length(scores))[scores == opt_score]
    opt_lambda = lambdas[opt_index]
    opt_result = result_list[[opt_index]]

    out = list(
        opt_lambda = opt_lambda,
        opt_score = opt_score,
        opt_result = opt_result,
        lambdas = lambdas,
        scores = scores,
        result = result_list
    )
}
