cvalest.bin <-
function (Y, scr, wgt=NULL) {

    if (is.null(wgt)) wgt = rep(1, length(Y))

    obj  = data.frame(Y, scr, wgt)
    obj1 = obj[obj$Y==1,]
    obj0 = obj[obj$Y==0,]

    cnt = 0
    for (i in 1:nrow(obj1)) {
        flg = obj1$scr[i] > obj0$scr
        cnt = cnt + obj1$wgt[i] * sum(obj0[flg,]$wgt)

        flg = obj1$scr[i] == obj0$scr
        cnt = cnt + obj1$wgt[i] * sum(obj0[flg,]$wgt) / 2
    }

    n   = nrow(obj)
    nrm = sum(obj1$wgt) * sum(obj0$wgt)
    return(cnt / nrm)
}
