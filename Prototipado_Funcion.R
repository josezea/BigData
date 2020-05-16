

beta <- 1.5
# Día 1
NumCasosTot <- 1

add.uneven <- function(x, y) {
  l <- max(length(x), length(y))
  length(x) <- l
  length(y) <- l
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  x + y
}

# c(1, 4, 5) + c(100, 1000)
# add.uneven(c(1, 4, 5), c(100, 1000))


set.seed(1234567)
Duracion_Proceso <- extraDistr::rdunif(n =  NumCasosTot, min = 5, 7)

vctr_DummyInfeccioso <- rep(1, Duracion_Proceso)
vctr_TotCasosAcumExtend <- vctr_DummyInfeccioso

Num_casosNew < vector(mode = "numeric")
Num_casosNew <- vctr_DummyInfeccioso[1]

Num_casosTotales < vector(mode = "numeric")
Num_casosTotales <- vctr_DummyInfeccioso[1]


# Día 2
set.seed(12345)
Casos_nuevos <- rpois(n = 1, lambda = beta * Num_casosTotales[1])
set.seed(1234567)
Duracion_Proceso <- extraDistr::rdunif(n =  Casos_nuevos, min = 5, 7)

# En cada paso t se va actualizando lista_DummyInfeccioso
lista_DummyInfeccioso <- vector("list")
for(i in 1:Casos_nuevos){
  lista_DummyInfeccioso[[i]] <- c(rep(0, (2-1)), rep(1, Duracion_Proceso[i])) # 2 camiarlo por t
}

vctr_TotCasosAcumExtend <- Reduce(add.uneven, c(list(vctr_TotCasosAcumExtend), lista_DummyInfeccioso))
Num_casosNew[2] <- vctr_TotCasosAcumExtend[2] - vctr_TotCasosAcumExtend[1]
Num_casosTotales[2] <- vctr_TotCasosAcumExtend[2]


# Tiempo 3
set.seed(12345)
Casos_nuevos <- rpois(n = 1, lambda = beta * Num_casosTotales[2])
set.seed(1234567)
Duracion_Proceso <- extraDistr::rdunif(n =  Casos_nuevos, min = 5, 7)

# En cada paso t se va actualizando lista_DummyInfeccioso
lista_DummyInfeccioso <- vector("list")
for(i in 1:Casos_nuevos){
  lista_DummyInfeccioso[[i]] <- c(rep(0, (3-1)), rep(1, Duracion_Proceso[i])) # 2 camiarlo por t
}

vctr_TotCasosAcumExtend <- Reduce(add.uneven, c(list(vctr_TotCasosAcumExtend), 
                                                lista_DummyInfeccioso))
Num_casosNew[3] <- vctr_TotCasosAcumExtend[3] - vctr_TotCasosAcumExtend[2]
Num_casosTotales[3] <- vctr_TotCasosAcumExtend[3]



# Tiempo 4
set.seed(12345)
Casos_nuevos <- 0 # Forzar la Poisson a cero
if(Casos_nuevos != 0){
set.seed(1234567)
Duracion_Proceso <- extraDistr::rdunif(n =  Casos_nuevos, min = 5, 7)

# En cada paso t se va actualizando lista_DummyInfeccioso
lista_DummyInfeccioso <- vector("list")

contador <- 3-1

# for(i in 1:Casos_nuevos){
#   lista_DummyInfeccioso[[i]] <- c(rep(0, contador), rep(1, Duracion_Proceso[i])) # 2 camiarlo por t
# }

f_dummiesInfecta <- function(x){
  numero0 <- contador
  return(c(rep(0, length = numero0), rep(1,length = x)))
}

lista_DummyInfeccioso <- lapply(Duracion_Proceso, f_dummiesInfecta)


vctr_TotCasosAcumExtend <- Reduce(add.uneven, c(list(vctr_TotCasosAcumExtend), 
                                                lista_DummyInfeccioso))
Num_casosNew[3] <- vctr_TotCasosAcumExtend[3] - vctr_TotCasosAcumExtend[2]
Num_casosTotales[3] <- vctr_TotCasosAcumExtend[3]
} else{
Num_casosNew[4] <- 0 
Num_casosTotales[4]  <- Num_casosTotales[3] 
}

########################## Volver esto función y generalizar


simula_proceso <- function(beta = c(NA,rep(1.4, 5)), Num_CasosIniciales = 1, 
                           InfeccionPMin = 5, InfeccionPMax = 7){

  library(extraDistr)  


  add.uneven <- function(x, y) {
    l <- max(length(x), length(y))
    length(x) <- l
    length(y) <- l
    x[is.na(x)] <- 0
    y[is.na(y)] <- 0
    x + y
  }

# Día 1
  Duracion_Proceso <- extraDistr::rdunif(n =  NumCasosTot,
                                       min = InfeccionPMin, InfeccionPMax)

  vctr_DummyInfeccioso <- rep(1, Duracion_Proceso)
  vctr_TotCasosAcumExtend <- vctr_DummyInfeccioso

  Num_casosNew < vector(mode = "numeric")
  Num_casosNew <- vctr_DummyInfeccioso[1]

  Num_casosTotales < vector(mode = "numeric")
  Num_casosTotales <- vctr_DummyInfeccioso[1]

# Día 2 en adelante:

  for(t in 2:(length(beta)-1)){
    Casos_nuevos <- rpois(n = 1, lambda = beta[t] * Num_casosTotales[t-1])
  
    if(Casos_nuevos != 0){
       Duracion_Proceso <- extraDistr::rdunif(n =  Casos_nuevos,
                         min = InfeccionPMin, max = InfeccionPMax)
       lista_DummyInfeccioso <- vector("list")
    
       contador <- t - 1
    
       f_dummiesInfecta <- function(x){
        return(c(rep(0, length = contador), rep(1,length = x)))
      }
    
      lista_DummyInfeccioso <- lapply(Duracion_Proceso, f_dummiesInfecta)
  
      vctr_TotCasosAcumExtend <- Reduce(add.uneven, 
                                        c(list(vctr_TotCasosAcumExtend), 
                                          lista_DummyInfeccioso))
      Num_casosNew[t] <- vctr_TotCasosAcumExtend[t] - vctr_TotCasosAcumExtend[t-1]
     Num_casosTotales[t] <- vctr_TotCasosAcumExtend[t]
    }
    else{
      Num_casosNew[t] <- 0 
      Num_casosTotales[t]  <- Num_casosTotales[t-1] 
    }
  } 
     salida <- list(Num_casosNew, Num_casosTotales)
  return(salida)
}

library(tictoc)
tic()
simula_proceso(beta = c(NA, 1.4, 1.3, 1.2, 1.8, 2, 1.8, 0.45, 1.5,
                        rep(0.45, 20)))
toc()



simula_proceso2 <- function(beta = c(NA,rep(1.4, 5)), Num_CasosIniciales = 1, 
                           InfeccionPMin = 5, InfeccionPMax = 7){
  
  library(extraDistr)  
  library(snow)
  
  add.uneven <- function(x, y) {
    l <- max(length(x), length(y))
    length(x) <- l
    length(y) <- l
    x[is.na(x)] <- 0
    y[is.na(y)] <- 0
    x + y
  }
  
  # Día 1
  Duracion_Proceso <- extraDistr::rdunif(n =  NumCasosTot,
                                         min = InfeccionPMin, InfeccionPMax)
  
  vctr_DummyInfeccioso <- rep(1, Duracion_Proceso)
  vctr_TotCasosAcumExtend <- vctr_DummyInfeccioso
  
  Num_casosNew < vector(mode = "numeric")
  Num_casosNew <- vctr_DummyInfeccioso[1]
  
  Num_casosTotales < vector(mode = "numeric")
  Num_casosTotales <- vctr_DummyInfeccioso[1]
  
  # Día 2 en adelante:
  
  for(t in 2:(length(beta)-1)){
    Casos_nuevos <- rpois(n = 1, lambda = beta[t] * Num_casosTotales[t-1])
    
    if(Casos_nuevos != 0){
      Duracion_Proceso <- extraDistr::rdunif(n =  Casos_nuevos,
                                             min = InfeccionPMin, max = InfeccionPMax)
      lista_DummyInfeccioso <- vector("list")
      
      contador <- t - 1
      
      f_dummiesInfecta <- function(x){
        return(c(rep(0, length = contador), rep(1,length = x)))
      }
      
      clus <- snow::makeCluster(4)
      
      lista_DummyInfeccioso <- snow::parLapply(clus, Duracion_Proceso, 
                                               f_dummiesInfecta)
      snow::stopCluster(cl)
      
      vctr_TotCasosAcumExtend <- Reduce(add.uneven, 
                                        c(list(vctr_TotCasosAcumExtend), 
                                          lista_DummyInfeccioso))
      Num_casosNew[t] <- vctr_TotCasosAcumExtend[t] - vctr_TotCasosAcumExtend[t-1]
      Num_casosTotales[t] <- vctr_TotCasosAcumExtend[t]
    }
    else{
      Num_casosNew[t] <- 0 
      Num_casosTotales[t]  <- Num_casosTotales[t-1] 
    }
  } 
  salida <- list(Num_casosNew, Num_casosTotales)
  return(salida)
}



library(tictoc)
tic()
simula_proceso2(beta = c(NA, 1.4, 1.3, 1.2, 1.8, 2, 1.8, 0.45, 1.5,
                        rep(0.45, 20)))
toc()
