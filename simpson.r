simpson = function(f, a, b, n) {
    arri = NULL
    arrxi = NULL
    arrfi = NULL
    h = (b - a) / n

    for (i in 0:n) {
        arri[i + 1] = i
        arrxi[i + 1] = a + i * h
        arrfi[i + 1] = f(arrxi[i + 1])
    }
    tabel = matrix(c(arri, arrxi, arrfi), ncol = 3, dimnames = list(NULL, c("i", "xi", "fi")))
    sumgenap = 0
    sumganjil = 0
    for (i in 2:n) {
        if (i %% 2 == 0) {
            sumganjil = sumganjil + arrfi[i]
        } else {
            sumgenap = sumgenap + arrfi[i]
        }
    } 
    output = paste("solusi numerik dengan aturan simpson adalah", (h/3) * (arrfi[1] + arrfi[n + 1] + 2 * sumgenap + 4 * sumganjil))
    return(list(tabel, output))
}

# lipat 2
simpson = function(f, a, b, c, d, h1, h2){
  hasil=0
  for (i in seq(a, b, by=h1)){
    sum=0
    for(j in seq(c, d, by=h2)){
      if(j==c||j==d){
        sum<-sum+ f(i,j)
      }
      else if(((j-c)/h2)%%2==0){
        sum<-sum+2*f(i,j)
      }
      else if(((j-c)/h2)%%2==1){
        sum<-sum+4*f(i,j)
      }
    }
    if(i==a||i==b){
      hasil<-hasil+(h2/3)*sum
    }
    else if(((i-a)/h2)%%2==1){
      hasil<-hasil+4*((h2/3)*sum)
    }
    else if(((i-a)/h2)%%2==0){
      hasil<-hasil+2*((h2/3)*sum)
    }
  }
  return((h1/3)*hasil)
}