trapsim <- function(f, a, b, c, d, h1, h2){
  hasil=0
  for(i in seq(a, b, by=h1)){
    sum=0
    for(j in seq(c, d, by=h2)){
      if(j==c||j==d){
        sum<-sum+f(i,j)
      }
      else if(((j-c)/h2)%%2==1){
        sum<-sum+4*f(i,j)
      }
      else if(((j-c)/h2)%%2==0){
        sum<-sum+2*f(i,j)
      }
    }
    if(i==a||i==b){
      hasil<-hasil+(h2/3)*sum
    }
    else{
      hasil<-hasil+2*((h2/3)*sum)
    }
  }
  return((h1/2)*hasil)
}