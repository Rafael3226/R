#getRandonVector() Funcion para generar el vector a evaluar de manera aleatoria
#@param seed Numero a utilizar como semilla
#@return vector  generado aleatoriamente con longitud 8-16 con numeros del 1-10
getRandonVector <- function (seed) {
    set.seed(seed)
    longitud <- round(runif(1,8,16))
    vector <- round(runif(longitud,1,10))
    return (vector)
}

#getMedia() Funcion para calcular la media del vector
#@param vector El vector a evaluar
#@return el valor de la media
getMedia <- function ( vector ) {
    suma <- 0
    for (actual in vector)
        suma <- suma + actual
    media <- suma / length(vector)
    return (media)
}

#ordenarVector() Funcion para ordenar un vector
#@param vector El vector a ordenar
#@return el vector ordenado acendientemenete 
ordenarVector <- function ( vector ) {
    ordenado <- vector
    long <- length(vector)
    for (i in 1:long)
        for (j in 1:long)
            if ( ordenado[i] < ordenado[j] ){
                temp <- ordenado[i]
                ordenado[i] <- ordenado[j]
                ordenado[j] <- temp
            }
    return (ordenado)
}

#getMediana() Funcion para calcular la mediana 
#@param vector El vector a evaluar
#@return el valor de la mediana
getMediana <- function ( vector ) {
    vector <- ordenarVector(vector)
    long <- length( vector )
    res <- long %% 2
    if ( res == 0) {
        mitad <- long/2
        mediana <- (vector[mitad]+vector[mitad+1])/2
        return (mediana)
    }else {
        mitad <- (long + 1)/2
        mediana <- vector[mitad]
        return (mediana)
    }
}

#getIndiceDelMayor() Funcion encontrar el numero mas grande de un vector
#@param vector El vector a evaluar
#@return el indice del mayor valor del vector
getIndiceDelMayor <- function ( vector ) {
    long <- length(vector)
    mayor <- 0
    indice <- 0
    for (i in 1:long)
        if (vector[i] > mayor){
            mayor <- vector[i]
            indice <- i 
        }
    return(indice)
}

#getModa() Funcion para calcular la moda del vector
#@param vector El vector a evaluar
#@return el valor de la moda
getModa <- function ( vector ) {
    numeros <- vector[1]
    veces <- 1

    longIni <- length(vector)
    for (i in 2:longIni){
        longAct <- length(numeros)
        num_en_vec <- FALSE 
        indice <- 0
        for (j in 1:longAct){
            #Hit !
            if (numeros[j] == vector[i]){
                num_en_vec <- TRUE 
                indice <- j
            }
        }
        #El numero esta en el vector?
        if (num_en_vec) {
            veces[indice] <- veces[indice] + 1
        }else{
            numeros <- c( numeros, vector[i] )
            veces <- c( veces, 1)
        }
    }
    moda <- numeros[getIndiceDelMayor(veces)]
    return (moda)
}

    seed <- Sys.time()
    print("SEED: ")
    print(seed)
    print("Vector: ")
    print(getRandonVector(seed))
    print("Vector Ordenado:")
    print(ordenarVector( getRandonVector(seed)))
    print("Media:")
    print(getMedia(getRandonVector(seed)))
    print("Mediana:")
    print(getMediana(getRandonVector(seed)))
    print("Moda:")
    print(getModa(getRandonVector(seed)))
