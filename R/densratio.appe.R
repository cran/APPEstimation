densratio.appe <-
function(xtrain, xtest, method="uLSIF", sigma=NULL, lambda=NULL, kernel_num=NULL, fold=5, stabilize=TRUE, qstb=0.025) {

    xtrain = as.matrix(xtrain)
    xtest  = as.matrix(xtest)

    if (is.null(kernel_num)) kernel_num = 100
    
    if (is.null(sigma)) {
        center = matrix(xtest[sample(1:nrow(xtest), kernel_num),], kernel_num, ncol(xtest))
        sigma  = as.array(quantile((dist(center))))
        sigma  = unique(sigma[ sigma>0.001 ])
    }

    if (is.null(lambda)) lambda = "auto"

    if (method == "uLSIF" || method == "KLIEP") {
        wgt = densratio(xtrain, xtest, method, sigma, lambda, kernel_num, fold, verbose=FALSE)$compute_density_ratio(xtest)
#    } else if (method == "gam") {
#        wgt = densratio.gam(xtrain, xtest, stabilize)
    } else {
#        stop("\n\nmethod should be either in ('uLSIF', 'KLIEP', 'gam').\n\n")
        stop("\n\nmethod should be either in ('uLSIF', 'KLIEP').\n\n")
    }

    ## tail-weight stabilization
    if (stabilize) {
        vl = quantile(wgt, qstb)
        wgt[ wgt < vl ] = vl
        vl = quantile(wgt, 1-qstb)
        wgt[ wgt > vl ] = vl
    }

    return(wgt)
}
