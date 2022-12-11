trapesium = function(f, a, b, n) {
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
    output = paste("solusi numerik dengan aturan trapezoid adalah", (h/2) * (arrfi[1] + arrfi[n + 1] + 2 * sum(arrfi[2:n])))
    return(list(tabel, output))
}


#lipat 2
trapesium = function(f, a, b, c, d, h1, h2){
  hasil=0
  for(i in seq(a, b, by=h1)){
    sum=0
    for(j in seq(c, d, by=h2)){
      if(j==c||j==d){
        sum<-sum+f(i,j)
      }
      else{
        sum<-sum+2*f(i,j)
      }
    }
    if(i==a||i==b){
      hasil<-hasil+(h2/2)*sum
    }
    else{
      hasil<-hasil+2*((h2/2)*sum)
    }
  }
  return((h1/2)*hasil)
}