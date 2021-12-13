#Regracion Polinomial
#@param n (2,12)
#@param data (x,y)
#@return Matriz
regrecion <- function(n , data) {
    #Numero de datos del data set
    data_length <- dim(data)[1]
    #Crear una lista para almacenar los valores a calcular
    list <- data_length # = Sumatoria x^0
    #Numero de x a calcular (n_xy = n)
    num_x <- (n-1)*2
    #Calacular las sumatorias de x^n (n=i)
    for (i in 1:num_x){
        suma <- 0 #sumatoria
        for (j in 1:data_length){ #Recorre la data
            fx <- data[j,1]^i #f(x) = x^n 
            suma <- suma + fx 
        }
        list <- c(list,suma)
    }
    #Calcular las sumatorias de x^n*y 
    for (i in 0:(n-1)){ #Por qué empieza en 0
        suma <- 0 #sumatoria
        for (j in 1:data_length){ #Recorre la data
            fx <- data[j,1]^i*data[j,2] #f(x) = x^n*y
            suma <- suma + fx  
        }
        list <- c(list,suma)
    }
    #Lista a matriz
    matriz <- NULL
    cont <- 1 
    for (i in 1:n){   
        for (j in cont:(n+cont-1)){##?????
            matriz <- c(matriz, list[j])    
        }
        cont <- cont + 1
    }
    for (i in 1:n){
        matriz <- c(matriz , list[num_x+i+1])
    }
    dim(matriz) <- c(n,n+1)
    print(matriz)
    return(matriz)
}
test <- function() {
    data <- c(1,2,3,4,5,1,2,3,4,5)
    dim(data) <- c(5,2)
    
    reg <- regrecion(4,data)
}

test()