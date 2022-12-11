regula_falsi = function(a,b,error,f,n){
    i = 1
    matrixa = NULL
    matrixb = NULL
    matrixc = NULL
    matrixf = NULL
    while ((abs(b - a) > error) && (i <= n)) {
        c = b - ((f(b) * (b - a)) / (f(b) - f(a)))
        matrixa[i] = a
        matrixb[i] = b
        matrixc[i] = c
        matrixf[i] = f(c)
        if (f(c) == 0) {
            break
        } else if (f(a) *f(c) < 0) {
            b = c
        } else {
            a = c
        }
        abs(b - a)
        i = i + 1
    }
    matrix = matrix(c(matrixa, matrixb, matrixc, matrixf), ncol = 4, dimnames =
    list(NULL, c("a", "b", "c", "f(c)")))
    return(matrix)
}