## ---- eval=FALSE, include=FALSE---------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: Cree funciones CON argumentos en R con cadenas de texto y con valores numéricos  (realice al menos dos ejercicios que requieran cargar archivos externos *.csv cada uno con al menos 50 filas y tres datos por fila, lea y procese la información del archvo leído, y guarde las respuestas a los ejercicios  en archivos independientes tipo *.txt)
## 
##  4. Fuentes:
##     https://www.generatedata.com"


## ---------------------------------------------------------------------
options(scipen=999)
# Importamos la base de datos:
base1 <- read.csv(file = "Base1.csv", sep = ",", dec = ".", header = T)

## Convertimos en data frame la base dada:
base1 <- data.frame(base1)

#Convertimos en factor la 2da columna:
base1$Grupo <- as.factor(base1$Grupo)

# Creamos una función que nos calcule la nota del trabajo:
def <- function(x){
"Ingresa un dataframe, a = suma de 3 vectores o columnas, p = al papa,
  papa = Calificacion de la Asiganatura X Credito/ Suma de los creditos"
  notaFinal <- (x[,3] + x[,4] + x[,5])/3
  notaFinal <- round(notaFinal, digits = 2)
  return(notaFinal)
}

#Ahora añandimos la definitiva de base1:
base1$Definitiva <- def(base1)

"Ahora para ahorrale procesos al profesor haremos que en otra columna que diga si perdio o no perdio con Booleanos"

defBool <- function(x){
  "Recorre la columna 6 de un data frame para despues crear un vector booleano donde genera ¿Perdio el trabajo?"
  t <- x[,6]
  fb <- c()
  for (i in t) {
    if(i >= 3.0){
      fb <- c(fb, TRUE)
    }
    if(i <= 3.0){
      fb <- c(fb, FALSE)
    }
  }
  return(fb)
}

base1$Gano_Trabajo <- defBool(base1)

# Ahora vamos a exportar el .txt
easy <- paste(" El Estudiante: ", base1$Nombres, " ,Gano el trabajo?: ", base1$Gano_Trabajo)

easy <- data.frame("Gano el trabajo"=easy)

Notafinal <- data.frame(base1$Nota1, base1$Nota2, base1$Nota3, base1$Definitiva)


write.csv(Notafinal, file = "Notafinal.csv", row.names = F)
write.table(easy, file = "Trabajo.txt", row.names = F)


## ---------------------------------------------------------------------
# Importamos la base de datos:
base2 <- read.csv(file = "Base2.csv", header = T, sep = ",", dec = ".")

base2 <- data.frame(base2)
#Volvemos en factores las columnas del medio:
base2$Mascota <- as.factor(base2$Mascota)

base2$Empresa <- as.factor(base2$Empresa)

"El costo del vuelo es 500.000 Pesos de base

si lleva mascota: 150.000
Sino ignore"

#Creamos la función pet:
pet <- function(x){
  "Esta función recibe un data frame en lacual se tomara una columna de texto, para despues tomar otra del costo y aumentarsela:"
  "Generamos la nueva columnas que contendra el costo del vuelo"
  base <- 500000
  cost <- c()
  for(i in x[,2]){
    if(i == "Yes"){
      p <- base + 150000
      cost <- c(cost, p)
    }
    if(i == "No"){
      cost <- c(cost, base)
    }
  }
  return(cost)
}

base2$Costo_SinCarga <- pet(base2)

#Si el usuario desea conocer el costo con el equipaje extra.
# Sera el siguiente:
"Segun los siguientes precios establecidos por el aeropuerto"

"Si carga es menor que 10 kilos, entonces es (0.03 X Peso) del precio del vuelo más el 19% del excendente"

"Si Carga es mayor a 10 Kilos (Carga maxima son 23 kg) entonces es (0.05 X Peso) del precio del vuelo más el 19% del excendente"

add <- function(x){
  "Calcula el costo del vuelo con el sobrepeso y el iva de este"
  cosadd <- c()
  for (i in x[,4]) {
    if(i <= 10.00){
      p <- i + (i*0.03)
      iva <- p*0.19
      t <- p + iva
      cosadd <- c(cosadd ,t) 
    }
    if(i > 10.00){
      p <- i + (i*0.05)
      iva <- p*0.19
      t <- p + iva
      cosadd <- c(cosadd, t)
    }
  }
  return(round(cosadd, digits = 2))
}

# Añandimos la nueva columna:

base2$Costo_ConCarga <- add(base2)*base2$Costo_SinCarga

##Ahora exportamos algo nuevo:
new_txt <- paste("La persona: ",
                 base2$Personas,
                 "LLeva mascota: ",
                 base2$Mascota,
                 "Su Sobrepeso es: ",
                 base2$Carga_Add,
                 "El Costo del Vuelo Base es: ",
                 base2$Costo_SinCarga,
                 "Su Costo con Sobrepeso es: ",
                 base2$Costo_ConCarga)

new_txt <- data.frame("El vuelo del Aeropuerto es: "= new_txt)

## Exportemos:
write.table(new_txt, file = "Resultado2.txt", row.names = F)


