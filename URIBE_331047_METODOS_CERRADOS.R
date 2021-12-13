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
#evaluarFuncion() Funcion para calcular
#el valor de la funcion en el punto x
#@param x El valor para evaluar la funcion
#@return el valor de la funcion en el punto x
evaluarFuncion <- function ( x ) {
    valor <- -2*x^6-1.5*x^4+10*x+2
    return(valor)
}

#getRaices() Funcion para encontrar los cambios de signo en el rango dado
#@param inicio El valor para evaluar la funcion
#@param fin
#@param incremento
#@return un vector con los puntos iniciales de x donde se encontraron raices
getRaices <- function ( inicio, fin, incremento ) {
    list <- NULL
    x <- inicio
    seguir <- TRUE
    while(seguir) {
        if( x <= fin ){
            x2 <- x + incremento
            v1 <- evaluarFuncion(x)
            v2 <- evaluarFuncion(x2)
            if( ( v1 > 0 && v2 < 0) || ( v1 < 0 && v2 > 0 ) ){
                list <- c(list , x)
            }
            x <- x2
        }else{
            seguir <-FALSE
        }
    }
    return(list)
}

#metodoBiseccion() Ejecuta el metodo de la biseccion
#@param error El error final deseado
#imprime dos vectores por cada raiz encontrada.
#uno con el valor de la aproximacion
#y otro con el error relativo porcentual
metodoBiseccion <- function ( error ) {
    inicio <- -4
    fin <- 4
    incremeto <- 1
    vRaicesIniciales <- getRaices(inicio,fin,incremeto) # Raices de -6 a 6 de a 0.3
    nRaices <- length(vRaicesIniciales)
    print("<-------- METODO BISECCION -------->")
    print(paste("Raices:",vRaicesIniciales))

    for(xini in vRaicesIniciales){
        vError <- NULL
        vRaiz <- NULL
        seguir <- TRUE
        xi <- xini
        xf <- xini + incremeto



        while(seguir){
            print("---------------------------------------------")
            print(paste("xi = ", xi))
            print(paste("xf = ", xf))

            xr <- (xi + xf)/2
            print(paste("Xr=",xr))

            errorActual <- getError(xr,xf)
            vError <- c(vError, errorActual)

            vRaiz <- c(vRaiz, xr)
            fxi <- evaluarFuncion(xi)
            fxf <- evaluarFuncion(xf)
            signo <- fxi*fxf # evaluar el signo

            print(paste("F(xi)=",fxi))
            print(paste("F(xf)=",fxf))
            print(paste("F(xi)*F(xf)=",signo))
            print(paste("Ea=",errorActual))
            if( signo < 0 ){
                xf <- xr
            }else{
                xi <- xr
            }
            if( errorActual <= error ){ # parar
                seguir <-FALSE
            }

        }
        print("---------------------------------------------")
    }
}

#metodoFalsaPos() Ejecuta el metodo de la biseccion
#@param error El error final deseado
#imprime dos vectores por cada raiz encontrada.
#uno con el valor de la aproximacion
#y otro con el error relativo porcentual
metodoFalsaPos <- function ( error ) {
    inicio <- -6
    fin <- 6
    incremeto <- 0.3
    vRaicesIniciales <- getRaices(inicio,fin,incremeto) # Raices de -6 a 6 de a 0.3
    nRaices <- length(vRaicesIniciales)
    print("<-------- METODO FALSA POSICION -------->")

    for(xini in vRaicesIniciales){
        vError <- NULL
        vRaiz <- NULL
        seguir <- TRUE
        xi <- xini
        xf <- xini + incremeto

        print("---------------------------------------------")
        print(paste("xi = ", xi))
        print(paste("xf = ", xf))
        print("----------------------")

        while(seguir){
            fxi <- evaluarFuncion(xi)
            fxf <- evaluarFuncion(xf)
            xr <- xi - ((fxi*(xf-xi))/(fxf-fxi)) # Falsa Pos
            fxr <- evaluarFuncion(xr)
            errorActual <- NULL

            if((fxi > 0 && fxr > 0) || (fxi < 0 && fxr < 0)){
                errorActual <- getError(xr,xi)
                xi <- xr

            }else{
                errorActual <- getError(xr,xf)
                xf <- xr

            }
            vError <- c(vError, errorActual)
            vRaiz <- c(vRaiz, xr)
            if( errorActual <= error ){ # parar
                seguir <-FALSE
            }
        }
        print("Raices")
        print(vRaiz)
        print("Errores")
        print(vError)
        print("---------------------------------------------")
    }
}
if(interactive()){
    input <- readline(prompt="Enter error deseado: ")
    e <- as.integer(input)
}else{
    e <- 0.1

}
metodoBiseccion(e)
#metodoFalsaPos(e)
