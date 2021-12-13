#Gauss
#@param matriz
#@return matriz Triangular superior
gauss <- function( matriz ){
    n <- dim(matriz)[1]
    n_mas_1 <- n+1
    for(i in 1:(n-1)){ #Recore las COLUMNAS
        pivote <- matriz[i,i]
        for(j in (i+1):n){ #Recore las FILAS
            # [ J , I ]
            factor <- -(matriz[j,i]/pivote) # F = - (ACTUAL)/(PIVOTE)
            for(k in 1:n){ #Recorre las COLUMNAS otra vez 
                matriz[j,k] <- matriz[j,k]+( factor* matriz[i,k])
            }
            #vector[j] <- vector[j]+( factor*vector[i])
            matriz[j,n_mas_1] <- matriz[j,n_mas_1]+( factor*matriz[i,n_mas_1])
        }
    }
    print(matriz)
    return (matriz)
}
#Solucionar
#@param matriz
#@return matriz solucion
solucionar <- function( matriz ) {
    #Dimenciones de la matriz
    n <- dim(matriz)[1]
    n_mas_1 <- n+1
    #Vector Solucion
    vector_x <- NULL 
    for (i in n:1){ #Recorre las Columnas
        pivote <- matriz[i,i]
        for (j in 1:n_mas_1){ # Recore las Filas
            matriz[i,j] <- matriz[i,j]/pivote
        }
    }
    #TODO
    for (i in n:2){ #Recorre la diagonal
        for (j in (i-1):1){ # Recore las Filas
            #[j,i]
            matriz[j,n_mas_1] <- matriz[j,n_mas_1] + matriz[i,n_mas_1]*(-matriz[j,i])
            matriz[j,i] = 0
        }
    }
    print(matriz)
    return(matriz)
}

test <- function(n){
    m <- c(6,36,244,1800,36,244,1800,14020,244,1800,14020,113016,1800,14020,113016,927684,18,121,923,7549)
    dim(m) <- c(4,5)
    print(m)
    print(gauss( m ))
    print(solucionar(gauss( m )))
}

test()