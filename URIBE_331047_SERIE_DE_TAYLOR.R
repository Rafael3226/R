#getValorNDerivada() Funcion para calcular 
#el valor de la n derivada de cos(1+2x)
#evaluada en x.
#@param n El numero de la derivada
#@param x El valor para evaluar la derivada
#@return el valor la n derivada evaluada en x.
getValorNDerivada <- function ( n , x ) {
    case <- (n%%4)+1 # +1 porque el switch empieza en 1 
    trigoPart <- switch(case,
        {cos(1+2*x)},
        {-sin(1+2*x)},
        {-cos(1+2*x)},
        {sin(1+2*x)})
    value <- (2^n)*trigoPart
    return(value)
}
#getError() Funcion para calcular 
#el valor Error a
#@param vActual El valor actual
#@param vAnterior El valor anterior 
#@return procentaje de error calculado para los valores 
getError <- function ( vActual , vAnterior ) {
    error <- ((vActual-vAnterior)/vActual)*100  
    if( error < 0 ){
        error <- error * (-1)
    }
    return(error)
}

#Ejecuta las series de taylor para los valores de xi y xi+1
#la funcion utilizada es  cos(1+2x).
#@param xi El valor a utilizar como xi
#@param xf El valor a utilizar como xi+1
#imprime cada una de las iteraciones con los valores de numero de iteracion, valor actual y error
taylor <- function (xi ,xf) {
    dif <- xf - xi
    vReal <- getValorNDerivada(0,xf)
    vActual <- getValorNDerivada(0,xi)
    i <- 1
    seguir <- TRUE
    while(seguir) {
        print("---------------------------------------------------")
        print(paste("Iteracion #",i))
        print(paste("Valor Actual: ",vActual))
        error <- getError(vReal,vActual)
        print(paste("Error: ",error))
        if( error < 0.5 || i > 100){
            seguir <-FALSE
        }else{
            vActual <- vActual + ((getValorNDerivada(i,xi)*(dif^i))/factorial(i)) #serie de taylor
            i <- i + 1
        }  
    }
    print("---------------------------------------------------")
}
 
taylor( 2.3 , 2.9 )