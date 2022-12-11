library(Deriv)
secant = function(f, x0, x1, err, iterasi) {
    arrx0 = NULL
    arrx1 = NULL
    arrc = NULL
    arry0 = NULL
    arry1 = NULL

    for (i in 1:iterasi) {
        y0 = f(x0)
        y1 = f(x1)
        c = x1 - ((y1 * (x1 - x0)) / (y1 - y0))
        if (abs(c - x1) < err) {
            break
        }
        arrx0[i] = x0
        arrx1[i] = x1
        arrc[i] = c
        arry0[i] = y0
        arry1[i] = y1
        x0 = x1
        x1 = c
    }
    tabel = matrix(c(arrx0, arrx1, arry0, arry1, arrc), ncol = 5, dimnames=list(NULL, c("x0", "x1", "f(x0)", "f(x1)", "xn+1")))
    print(paste("akar = ", c))
    return (tabel)
}