#### ALgebra lógica

5>3

4<=6

6<6

6<=6

4>45

4!=5

3=6



3=6  ## Que sucede? (Error en la asignacion)

3==6  ## Comparación correcta

## Asignaci???n de objetos

## Flecha
x <- c(1,4,5,6,7,86867,7,89,4,5,6,7)

## igual
Alirio = c(1,4,5,6,7,86867,7,89,4,5,6,7)


x^3  ## no se asigna


y = Alirio^3

nombre= "Alejandra"  ### Entre comillas para nombres
nombres= c("Alejandra", "Alexsy", "Carlos", "Jhonny", "Karen") ## Separadas por coma

nombres^2  ## Problema con non-numeric

w=1:10

w==x  ### Son tamaños diferentes, pero lo hace
### Mucho cuidado
x
w[1]=6 ## Cambiando el valor de la posicion 1

w==x

z==y

z==x  ## Error, no existe z

z = 1:100

z==w  ## Porque 10 veces w, es igual a z (W es multiplo en posiciones de z)

## Nombre <- Operacion
y <- z>7 ## Un vecto con TRUE si z es mayor a 7, y false 

y
str(y) ### Estructura de y

a =y^2  ## Los FALSE=0, los TRUE=1

A=y+2 ## le suma 2 a todo

## El objeto a es diferente al A

y^2

y+y

y+x  ## Distinto tamaño, warning

##### Posiciones de vectores

x[0] ### Vector vacio (numeric(0))

x[1] ## Posición 1

length(x) ## La longitud, el tamaño

paola(x) ## No existe la función paola

x[12] ## Posicion 12

x[13] ## No existe la posición 13

x[-1] ### La posición 1 es eliminada
x[-2] ### Elimina la posicion 2

x[c(1,3,4,5)] ### Las trae

x[c(-3,-4,-5)]  ### las elimina

(2,3) ### Esto no funciona
c(2,3)

x[]  ### Me trae todo

x[x<7] ## Trae todos los valores menores a 7


x[c(3,3,3,3,3,3,3,3,3,3)] ### Trae la posición 3, 10 veces

nombres[c(1,3,45)] ### trae los nombres1,3 y 45 (NA)

##### Algo más elaborado
##### Primero matrices

unamatriz <- matrix(c(2,3,4,6,7,8,9,10,100,30), ncol=3)

unamatriz

dim(unamatriz)

unamatriz+3

unamatriz+c(2,3,4,56,8)

unamatriz+1:13

t(unamatriz)

unamatriz * unamatriz

unamatriz %*% unamatriz


t(unamatriz)%*% unamatriz

unamatriz[12]

unamatriz[-3]

unamatriz[2,2]

unamatriz[,3]

unamatriz[4,]

unamatriz[c(1,3), c(2,3)]

unamatriz[2,3]<-2

unamatriz


#### Vamos a cargar unos datos 

url="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/Global_Carbon_Budget_2018.csv"

carbon=read.csv(url, header = T, sep=";")
### El header TRUE es porque tiene encabezamiento, el sep=";",
## es porque el separador de columnas
carbon


carbon=read.csv(url, header = T, sep=";", dec=",")
carbon

dim(carbon)

carbon[5,3] ## Fila 5 columna 3
carbon[268,1] ## Fila 268, columna 1

carbon[,1] ## Primer columna

carbon[1,] ## Primera fila


carbon[,8] ### Octava columna, error no existe

carbon[300,] ### Fila 300, no hay error

subset(carbon, Year==2000) ## La base cuando el año es 2000

subset(carbon, fossil.fuel.and.industry<0.5) ### Cuando la variable
## Fosil and fuel industry, menor a 0.5

carbon2 =subset(carbon, fossil.fuel.and.industry<0.5)



subset(carbon, fossil.fuel.and.industry==1) ## Ninguna fila que cumpla la condicion
### dataset vacio


subset(carbon, fossil.fuel.and.industry==0.8)


#### cargar un excel  ####

#### Cargar un dta Stata   ####

###  cargar un sav SPSS  ####



## Como se manejan las gráficas 

plot(carbon$Year)

plot(carbon$Year, carbon$fossil.fuel.and.industry)

plot(carbon$Year, carbon$fossil.fuel.and.industry, color="red")

plot(carbon$Year, carbon$fossil.fuel.and.industry, col="red")

plot(carbon$Year, carbon$fossil.fuel.and.industry,
     col="red", type="l")

boxplot( carbon$fossil.fuel.and.industry)

carbon$nueva_var = ifelse(carbon$Year<1950, "Viejo", "Nuevo")

boxplot(carbon$fossil.fuel.and.industry~carbon$nueva_var)


#### vamos por los ciclos de R ####
#### Tema libre                ####

