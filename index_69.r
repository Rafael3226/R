#CONSTANTES
R_2_ESPERADO <- 0.8
#VARIABLES GLOBALES
num_datos <- -1
#evaluar_chunk
act_reg_data <- NULL
act_n_matriz <- NULL
#guardar data
l_data_x <- NULL
l_data_y <- NULL
l_r_2 <- NULL
l_grado_regrecion <- NULL

#Ejecuta el Algoritmo
run <- function(){
    #TAKE TIME
    start_time <- Sys.time()
    #Cargar el CSV
    data <-read.csv("data_2.csv")
    #GUARDAR EL NUMERO DE DATOS 
    num_datos <- dim(data)[1]
    assign("num_datos",num_datos, envir = .GlobalEnv)
    #Imprimir el numero de datos
    print(paste("LOADED DATA=",num_datos))
    #DIVIDIR LA DATA
    dividir_chunk(data)
    #TAKE END TIME
    end_time <- Sys.time()
    #RUN TIME
    run_time <- end_time - start_time
    print(paste("RUNTIME =",run_time))
}
#
dividir_chunk <- function(data){
    #Data a lineal
    n_data <-dim(data)[1]

    print(paste("DIV ",n_data," DATA"))
    
    #DATA_1
    top_1 <- floor(n_data/2)
    data_1 <- NULL
    data_1x <- NULL
    data_1y <- NULL
    for(i in 1:top_1){
        data_1x <- c(data_1x,data[i,1])
        data_1y <- c(data_1y,data[i,2])
    }
    data_1 <- c(data_1x,data_1y)
    dim(data_1) <- c(top_1,2)
    evaluar_chunk(data_1)
    #DATA_2
    par <- (n_data)%%2==0
    if(par){
        bot_2 <- top_1 + 1
    }else{
        bot_2 <- n_data - top_1
    }
    data_2 <- NULL
    data_2x <- NULL
    data_2y <- NULL
    for(i in bot_2:n_data){
        data_2x <- c(data_2x,data[i,1])
        data_2y <- c(data_2y,data[i,2])
    }
    data_2 <- c(data_2x,data_2y)
    data_length <- length(data_2x)
    dim(data_2) <- c(data_length,2)
    evaluar_chunk(data_2)
}
#Evaluar Chunk
#@param data
#@return BOOLEAN
evaluar_chunk <- function(data){
    #Dimencion de la Data
    n_data <- dim(data)[1]
    #Chunk a evaluar
    p_evaluar <- (n_data/num_datos)*100
    #print(paste("TRY CHUNK",p_evaluar,"%"))
    #Promedio y regrecion
    promedio <- promedio(data)
    regrecion <- regrecion(data) 
    #Datos While
    seguir <- TRUE
    n <- dim(regrecion)[1]
    #MAX R_2
    max_r_2 <- -1000
    max_reg_data <- NULL
    max_n_matriz <- NULL
    #
    while (seguir){
        #print(paste("TRY n=",n))
        matriz <- reducir_matriz(n,regrecion)
        gauss <- gauss(matriz)
        solucion <- solucionar(gauss)
        r_2 <- evaluar(data, solucion, promedio)
        if(is.na(r_2)){
            max_r_2 <- 1
            max_reg_data <- data
            max_n_matriz <- 0
        }else if(r_2 >= max_r_2){
            max_r_2 <- r_2
            max_reg_data <- act_reg_data
            max_n_matriz <- act_n_matriz
        }
        if(n == 2){
            seguir <- FALSE
        }
        n <- n-1
    }
    if(max_r_2 >= R_2_ESPERADO){
        print(paste("CHUNK PASS",p_evaluar,"%"))
        guardar_data(max_r_2,max_reg_data,max_n_matriz) #GUARDAR
    }else if(n_data>=4){
        dividir_chunk(data) 
    }else{
        print(paste("FAIL SAVE",p_evaluar,"%"))
        guardar_data(max_r_2,max_reg_data,max_n_matriz)
    }
    
}
#Set max values
set_act_values <- function(act_reg_data,act_n_matriz){
    assign("act_reg_data",act_reg_data, envir = .GlobalEnv)
    assign("act_n_matriz",act_n_matriz, envir = .GlobalEnv)
}
#Solucionar
#@param data para contrastar
#@param matriz con la funcion
#@param media_data promedio de los datos
#Si r_2_esperado <=  r_2 calculado
#@return BOOLEAN if TRUE guardar_data()
evaluar <- function( data , matriz , media_data) {    
    #Dimenciones de la matriz y la data
    n_matriz <- dim(matriz)[1]
    n_data <- dim(data)[1]
    n_matriz_1 <- n_matriz + 1
    #Calcular la media de Y (Regrecion)
    data_variance <- 0
    reg_variance <- 0
    reg_data_x <- NULL
    reg_data_y <- NULL
    #Recorrer la Data
    for (i in 1:n_data){
        fx <- 0
        for (j in 1:n_matriz){
                fx <- fx + matriz[j,n_matriz_1]*data[i,1]^(j-1)
        }
        data_variance <- data_variance + (data[i,2]-media_data)^2
        reg_variance <- reg_variance + (data[i,2]-fx)^2
        reg_data_x <- c(reg_data_x,data[i,1])
        reg_data_y <- c(reg_data_y,fx)
    }
    #Concatenar Listas
    reg_data <- c(reg_data_x,reg_data_y)
    dim(reg_data) <- c(n_data,2)
    #Calcular R^2
    r_2 <- 1 -  (  reg_variance / data_variance ) 
    #SET VALORES ACTUALES
    set_act_values(reg_data,n_matriz)
    return(r_2)   
}
#Regracion Polinomial
#@param n por defecto 12
#@param data (x,y)
#@return Matriz
regrecion <- function( data , n=12) {
    #Numero de datos del data set
    data_length <- dim(data)[1]
    #
    if(data_length < 12){
        n <- data_length
    }
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
    return(matriz)
}
#Gauss
#@param matriz
#@return matriz Triangular superior
gauss <- function( matriz ){
    n <- dim(matriz)[1]
    n_1 <- n+1
    for(i in 1:(n-1)){ #Recore la Diagonal
        for(j in (i+1):n){ #Recore las FILAS
            # [ J , I ]
            # Factor = - (ACTUAL)/(PIVOTE)
            factor <- -(matriz[j,i]/matriz[i,i])
            for(k in i:n_1){ #Recorre las COLUMNAS
                if(i!=k){
                    temp <- matriz[j,k]+( factor* matriz[i,k])
                }else{
                    temp <- 0 
                }
                matriz[j,k] <- temp
            }
        }
    }
    return (matriz)
}
#Solucionar
#@param matriz triangular superior
#@return matriz solucion
solucionar <- function( matriz ) {
    #Dimenciones de la matriz
    n <- dim(matriz)[1]
    n_1 <- n+1
    for (i in n:1){ #Recorre las Columnas
        pivote <- matriz[i,i]
        for (j in 1:n_1){ # Recore las Filas
            matriz[i,j] <- matriz[i,j]/pivote
        }
    }
    #TODO
    for (i in n:2){ #Recorre la diagonal
        for (j in (i-1):1){ # Recore las Filas
            #[j,i]
            matriz[j,n_1] <- matriz[j,n_1] + matriz[i,n_1]*(-matriz[j,i])
            matriz[j,i] = 0
        }
    }
    return(matriz)
}
#Promedio
#@param data
#@return promedio
promedio <- function(data) {
    n <- dim(data)[1]
    suma <- 0
    for (i in 1:n){
        suma <- suma + data[i,2]
    }
    promedio <- suma / n
    return(promedio)
}
#Reducir matriz
#@param n 
#@param matriz (12*13)
#@return matriz n*n+1
reducir_matriz <- function( n , matriz ){
    filas <- dim(matriz)[1]
    columnas <- dim(matriz)[2]
    if (n > filas){
        return(NULL)
    }else if( n == filas){
        return(matriz)
    }
    else{
        new_matriz <- NULL
        n_1 <- n + 1 
        for(i in 1:n){
            for(j in 1:n){
                new_matriz <- c(new_matriz,matriz[i,j])
            }
        }
        for (i in 1:n){
            new_matriz <- c(new_matriz,matriz[i,columnas])
        }
        dim(new_matriz) <- c(n,n_1)
        return(new_matriz)
    }   
}
#Guardar data
#@param r_2
#@param data
#@param grado_regrecion 
#@void print exitoso%
guardar_data <- function(r_2, data, grado_regrecion){
    n_data_mit <- dim(data)[1]
    n_data <- n_data_mit*2
    n_data_mit_1 <- n_data_mit + 1
    data <- as.vector(data) # Volver lineal el arreglo
    
    l_data_x <- c(l_data_x, data[1:n_data_mit])
    assign("l_data_x",l_data_x, envir = .GlobalEnv)
    l_data_y <- c(l_data_y, data[n_data_mit_1:n_data])
    assign("l_data_y",l_data_y, envir = .GlobalEnv)
    for(i in 1:n_data_mit){
        l_r_2 <- c(l_r_2,r_2)
        assign("l_r_2",l_r_2, envir = .GlobalEnv)
        l_grado_regrecion <- c(l_grado_regrecion,grado_regrecion)
        assign("l_grado_regrecion",l_grado_regrecion, envir = .GlobalEnv)
    }
    p_valido <- (length(l_data_x)/num_datos)*100
    print(paste("TOTAL",p_valido,"%"))
    if(p_valido>=100){
        print("SAVE")
        l_data <- c(l_data_x,l_data_y)
        dim(l_data) <- c(num_datos,2)
        l_resultado <- c( l_data_x , l_data_y , l_grado_regrecion , l_r_2 )
        dim(l_resultado) <- c(num_datos,4)
        write.csv( l_data ,"res_data_2.csv", row.names = FALSE)
        write.csv( l_resultado ,"all_res_data_2.csv", row.names = FALSE)
    }    
}

run2 <- function(m){
    data <-read.csv("data_2.csv")
    x <- NULL
    y <- NULL
    for(i in 1:m){
        x <- c(x,data[i,1])
        y <- c(y,data[i,2])
    }
    data <- c(x,y)
    dim(data) <- c(m,2)
    assign("num_datos",m, envir = .GlobalEnv)
    print(paste("LOADED DATA=",num_datos))
    dividir_chunk(data)
}
#run2(85);
run();
