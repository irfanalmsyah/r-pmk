library(Deriv)
newtonraphson = function(f, x0, err, iterasi) {
    arrx0 = NULL
    arry0 = NULL
    arry1 = NULL

    for (i in 1:iterasi) {
        y0 = f(x0)
        df = Deriv(f)
        y1 = df(x0)
        c = x0 - (y0 / y1)
        
        arrx0[i] = x0
        arry0[i] = y0
        arry1[i] = y1

        if (abs((c - x0)/c) < err) {
            break
        }
        x0 = c
    }
    tabel = matrix(c(arrx0, arry0, arry1), ncol = 3, dimnames=list(NULL, c("x0", "f(x0)", "f'(x0)")))
    print(paste("akar = ", c))
    return (tabel)
}