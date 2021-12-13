run <- function(){
  a <- 3
  b <- 10
  #Numero de integrales
  n <- 5
  #Diferencia
  h <-(b-a)

  print("<-----------------  Trapecios  ----------------->")
  print(IntegracionTrapecio(h, n, a, b))

  print("<-----------------  Simpson  ----------------->")
  print(integracionSimpson(h, n, a, b))
  
}
run()
#
Fx <- function(x){
  resultado <- 6+3*cos(x)
  return (resultado)
}
#
IntegracionTrapecio <- function(h, n, a, b){
  h <- h/n
  if (n == 1) {
    return (Trapecio_1(h, a, b))
  }else {
    return (Trapecio_2(h, n, a, b))
  }
}
#
integracionSimpson <- function(h, n, a, b){
  h <- h/(2*n)
  if (n == 1) {
    return (Simpson_1(h, a, b))
  }else {
    return (Simpson_2(h, n, a, b))
  }
}
#
Trapecio_1 <- function(h, a, b){
  i <- h*(Fx(a)+Fx(b))/2
  return (i)
}
#
Trapecio_2 <- function(h, n, a, b){
  x <- a
  suma <- Fx(x)
  for (i in 1:(n-1)) {
    x <- x + h
    suma <- suma + 2*Fx(x)
  }
  suma <- suma + Fx(b)
  return(h*suma/2)
}
#
Simpson_1 <- function(h, a, b){
  x <- (b-a)/2
  i <- h*(Fx(a)+4*Fx(x)+Fx(b))/3
  return(i)
}
#
Simpson_2 <- function(h, n, a, b){
  suma <- Fx(a) + Fx(b)
  for (i in 0:(n-1)) {
    x <- a + h*(2*i+1)
    suma <- suma + 4*Fx(x)
  }
  for (i in 1:(n-1)) {
    x <- a + h*(2*i)
    suma <- suma + 2*Fx(x)
  }
  return(h*suma/3)
}