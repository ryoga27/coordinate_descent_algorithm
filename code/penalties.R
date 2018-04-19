soft_threshold_function = function(x, lambda){
    d = length(x)
    out = rep(NA, d)
    for(j in 1:d){
        if(abs(x[j]) <= lambda){
            out[j] = 0
        }
        if(abs(x[j]) >  lambda)
            out[j] = x[j]
        }
    }
    return(out)
}

# x = seq(from = -2, to = 2, by = 0.1)
# out = soft_threshold_function(x = x, lambda = 0.5)
# print(out)
# plot(x, out, type = "n")
# grid()
# points(x, x, type = "l", lty = 3)
# points(x, out, type = "l", lwd = 2)

lasso_penalty_function = function(x, lambda){
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
# out = lasso_penalty_function(x = x, lambda = 0.5)
# print(out)
# plot(x, out, type = "n")
# grid()
# points(x, x, type = "l", lty = 3)
# points(x, out, type = "l", lwd = 2)

scad_penalty_function = function(x, lambda, gamma = 3.7){
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
# out = scad_penalty_function(x = x, lambda = 0.5)
# print(out)
# plot(x, out, type = "n")
# grid()
# points(x, x, type = "l", lty = 3)
# points(x, out, type = "l", lwd = 2)
# grid()
