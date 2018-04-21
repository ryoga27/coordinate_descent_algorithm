predict = function(result, X_new){
    out = result$coefficients[1] + result$coefficients[-1]%*%X_new
    out = as.numeric(out)
    return(out)
}
