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
#evaluarDerivada()
#@param x
#@param n
#@param func (0-1)  trigo o poli
#@return
evaluarDerivada <- function( x , n , func){
    if( func == 0){
    value <- switch(n ,
        {2*(x^3)-11.7*(x^2)+17.7*x-5},
        {6*(x^2)-23.4*x+17.7})
    }
    else{
    value <- switch(n ,
        {0.95*x^3-5.9*x^2+10.9*x-6},
        {2.85*x^2-11.8*x+10.9})
    }
    return (value)

}
#puntoFijo() calcula el xi segun el metodo de punto fijo
#@param xu el valor con el cual se evalua la funcion
#@param xl el valor con el cual se evalua la funcion
#@param func (0-1)  trigo o poli
#@return el siguiente valor de xi
puntoFijo <- function(xu , xl , func){
    fxu <- evaluarDerivada(xu , 1 , func )
    fxl <- evaluarDerivada(xl , 1 , func )
    result <- xu- ( (fxu*(xl-xu)) / (fxl - fxu) )
    return (result)

}
#newtonRaphson()
#@param x
#@param func
#@return
newtonRaphson <- function(x , func){
    x1 <- evaluarDerivada(x , 1 , func )
    x1p <- evaluarDerivada(x , 2 , func )
    print(paste("F(",x,")=",x1))
    print(paste("F'(",x,")=",x1p))
    result <- x - (x1/x1p)
    print(paste("X",x," = ",result))

    return(result)
}
#secante()
#@param x1
#@param x1
#@return
secante <- function(x1 , x2 ){
    fx1 <- evaluarDerivada(x1 , 1 , 1)
    fx2 <- evaluarDerivada(x2 , 1 , 1)
    result <- x2 - ((fx2*(fx1-x1))/(fx1-fx2))
    return(result)
}
#runPuntoFijo()
runPuntoFijo <- function(){
    seguir <- TRUE
    #punto fijo
    x1 <- 1.5
    x2 <- 2
    x3 <- 0
    print("Punto fijo <------------------------------------")
    while(seguir){
        x3 <- puntoFijo(x1 , x2 , 1)
        print(paset("Xi= ", x3))
        error <- getError(x1,x3)
        print(paste("ea= ",error))
        x2 <- x3
        if (error <= 0.01){
            seguir = FALSE
        }
    }
}
#runNewtonRaphson()
runNewtonRaphson <- function(x1, func, ea){
    seguir <- TRUE
    #Newton Raphson
    print("newton Raphson <------------------------------------")
    count <- 0
    while(seguir){
        x2 <- newtonRaphson(x1 , func )
        error <- getError(x2,x1)
        print(paste("Ea= ",error))
        x1 <- x2
        count <- count + 1
        if (error <=  ea || count == 100){
            seguir = FALSE
        }
    }
}
#runSecante()
runSecante <- function(){
    seguir <- TRUE
    #Newton Raphson
    x1 <- 0.5
    x2 <- 1
    print("Secante <------------------------------------")
    while(seguir){
        x3 <- secante(x1 , x2 )
        print(paste("Xi= ",x3))
        error <- getError(x3,x2)
        print(paste("ea= ",error))
        x1 <- x2
        x2 <- x3
        if (error <= 0.01){
            seguir = FALSE
        }
    }
}

#runPuntoFijo()
runNewtonRaphson(1,1,1)
#runNewtonRaphson(1.2,1,0.1)
runSecante()
#print(secante(3,1))
