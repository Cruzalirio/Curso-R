#### ALgebra l???gica

5>3

4<=6

4>45

4!=5

3==6



3=6  ## Que sucede?


## Asignaci???n de objetos

x <- c(1,4,5,6,7,86867,7,89,4,5,6,7)

x^3

log(x)

z <- c(x, x)
y=c(x,x)
w=1:10


z==y

z==x  ## Tienen diferente longitud
## pero son multiplos

z==w

y<-z>7

y
str(y)

y^2

y+y

y+x

##### Posiciones de vectores

x[0]

x[1]

length(x)

x[12]

x[13]

x[-1]

x[c(1,3,4,5)]

x[c(-3,-4,-5)]

x[]

x[x<7]

x[c(TRUE,TRUE,TRUE,FALSE,FALSE)]

x[c(3,3,3,3,3,3,3,3,3,3)]

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
carbon


carbon=read.csv(url, header = T, sep=";", dec=",")
carbon

dim(carbon)

carbon[,1]

carbon[1,]


carbon[,8]

carbon[300,]

subset(carbon, Year==2000)

subset(carbon, fossil.fuel.and.industry<0.5)



subset(carbon, fossil.fuel.and.industry==1)


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

