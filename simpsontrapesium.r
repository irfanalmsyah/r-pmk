simpsontrapesium<-function(f, a, b, c, d, h1, h2){
  hasil=0
  for(i in seq(a, b, by=h1)){
    sum=0
    for(j in seq(c, d, by=h2)){
      if(j==c||j==d){
        sum<-sum+f(i,j)
      }else{
        sum<-sum+2*f(i,j)
      }
    }
    if(i==a||i==b){
      hasil<-hasil+(h2/2)*sum
    }
    else if(((i-a)/h2)%%2==1){
      hasil<-hasil+4*((h2/2)*sum)
    }
    else if(((i-a)/h2)%%2==0){
      hasil<-hasil+2*((h2/2)*sum)
    }
  }
  return((h1/3)*hasil)
}