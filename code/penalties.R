lasso_penalty_function = function(x, lambda){
    out = 0
    d = length(x)
    for(j in 1:d){
        out = out + lambda*abs(x[j])
    }
    return(out)
}

# x = seq(from = -2, to = 2, by = 0.1)
# d = length(x)
# out = rep(0, d)
# for(j in 1:d){
#     out[j] = lasso_penalty_function(x[j], lambda = 0.5)
# }
# plot(x, out, type = "n")
# grid()
# points(x, out, pch = 20)

lasso_penalty_solution = function(x, lambda){
    d = length(x)
    out = rep(NA, d)
    for(j in 1:d){
        if(abs(x[j]) <= lambda){
            out[j] = 0
        }
        if(abs(x[j]) >  lambda){
            out[j] = sign(x[j])*(abs(x[j]) - lambda)
        }
    }
    return(out)
}

# x = seq(from = -2, to = 2, by = 0.1)
# out = lasso_penalty_solution(x = x, lambda = 0.5)
# print(out)
# plot(x, out, type = "n")
# grid()
# points(x, x, type = "l", lty = 3)
# points(x, out, type = "l", lwd = 2)

scad_penalty_function = function(x, lambda, gamma = 3.7){
    out = 0
    d = length(x)
    for(j in 1:d){
        if(abs(x[j]) <= lambda){
            out = out + lambda*abs(x[j])
        }
        if(lambda < abs(x[j]) & abs(x[j]) <= gamma*lambda){
            out = out - (abs(x[j])^2 - 2*gamma*lambda*abs(x[j]) + lambda^2)/(2*(gamma - 1))
        }
        if(gamma*lambda < abs(x[j])){
            out = out + (gamma + 1)*lambda^2/2
        }
    }
    return(out)
}

# x = seq(from = -2, to = 2, by = 0.1)
# d = length(x)
# out = rep(0, d)
# for(j in 1:d){
#     out[j] = scad_penalty_function(x[j], lambda = 0.5, gamma = 3.7)
# }
# plot(x, out, type = "n")
# grid()
# points(x, out, pch = 20)

scad_penalty_solution = function(x, lambda, gamma = 3.7){
    d = length(x)
    out = rep(NA, d)
    for(j in 1:d){
        if(abs(x[j]) <= 2*lambda){
            if(abs(x[j]) <= lambda){
                out[j] = 0
            }
            if(abs(x[j]) >  lambda){
                out[j] = sign(x[j])*(abs(x[j]) - lambda)
            }
        }
        if(2*lambda <= abs(x[j]) & abs(x[j]) <= gamma*lambda){
            out[j] = ((gamma - 1)*x[j] - sign(x[j])*gamma*lambda)/(gamma - 2)
        }
        if(gamma*lambda  < abs(x[j])){
            out[j] = x[j]
        }
    }
    return(out)
}

# x = seq(from = -2, to = 2, by = 0.1)
# out = scad_penalty_solution(x = x, lambda = 0.5, gamma = 3.7)
# print(out)
# plot(x, out, type = "n")
# grid()
# points(x, x, type = "l", lty = 3)
# points(x, out, type = "l", lwd = 2)

penalty_function = function(x, lambda, gamma = 3.7, penalty = "lasso"){
    if(penalty == "lasso"){
        out = lasso_penalty_function(x = x, lambda = lambda)
    }
    if(penalty == "scad"){
        out = scad_penalty_function(x = x, lambda = lambda, gamma = gamma)
    }
    return(out)
}

penalty_solution = function(x, lambda, gamma = 3.7, penalty = "lasso"){
    if(penalty == "lasso"){
        out = lasso_penalty_solution(x = x, lambda = lambda)
    }
    if(penalty == "scad"){
        out = scad_penalty_solution(x = x, lambda = lambda, gamma = gamma)
    }
    return(out)
}
