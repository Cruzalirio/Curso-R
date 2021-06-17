
url="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/Global_Carbon_Budget_2018.csv"

carbon=read.csv(url, header = T, sep=";", dec=",")

carbon$Year[150] <- -2000

carbon$Unax <- ifelse(carbon$atmospheric.growth<0, "Negativa", "Positiva")

carbon$Unax[10] <- "bryan"


carbon$Unax <- ifelse(carbon$atmospheric.growth<0, "Negativa", "Positiva")

carbon$Otrax <- c(1,2,3,4)

carbon$Year1 = as.factor(carbon$Year)

### gl crea factores

#### factor es una variable con niveles

carbon$Unax <- as.factor(carbon$Unax)

carbon$Unax[10] <- "bryan"

## Como se manejan las gráficas 

plot(c(1,2,3,4,20))

plot(carbon$Year)

plot(carbon$Year, carbon$fossil.fuel.and.industry)

plot(carbon$Year, carbon$fossil.fuel.and.industry, color="red")

plot(carbon$Year, carbon$fossil.fuel.and.industry, col="red")

plot(carbon$Year, carbon$fossil.fuel.and.industry,
     col="red", type="l", main="Fossil", xlab="Katherin", ylab="Fossil")

warnings()
modelo=lm(log(fossil.fuel.and.industry+1)~Year, data=carbon)

YY = exp(-15.9639 + 0.0088*carbon$Year)-1

lines(carbon$Year, YY)
text(2000, 5, "Nelson")
text(c(1950, 2000), c(2, 6), c("N", "A"))
text(carbon$Year, carbon$fossil.fuel.and.industry, carbon$Unax)



boxplot( carbon$fossil.fuel.and.industry)




warnings()
#### vamos por los ciclos de R ####
#### Tema libre                ####
i=1000
while (i>0){
  print(paste("Arrancamos en ", i, sep = ""))
  i=i-1
}

i=1
x=1
while(i==1){
  x=c(x,i)
}

ifelse(runif(100)<0.5, "CARA", "SELLO")

y<-runif(100, 50, 250)

floor(y)

round(y, digits = 0)


ifelse(runif(100)<0.5, 50, 250)

ifelse(runif(100)<0.2, 50, 250)

ifelse(runif(100)<0.5, -1, 1)

### ifelse

carbon$nueva_var = ifelse(carbon$Year<1950, "Viejo", "Nuevo")

table(carbon$nueva_var)

boxplot(carbon$fossil.fuel.and.industry~carbon$nueva_var)

x <-3
if(x<5){
  print(x^2)
}


x =20
if(x<5){
  print(x^2)
}else{
  print(x+8)
}

x =20

if(x<5){
  print(x^2)
}else if( x<10){
  print(paste(x+8, "Yuju"))
}else{
  print(paste("el resultado es", x))
}

paste(2,3,3,4,5,35,35,35, sep="")

paste(y , "lina", YY, sep="aaaa")

## CONCAT (A2, " ", B2, " su nombre ")

### ciclo for
n <-100
for (i in 1:n){
  print(i)
}

x <-1
n<- 10
for(i in 1:n){
  x=x*i
}
x
1*2*3*4*5*6*7*8*9*10

prod(1:10)

factorial(10)


x <-1
n<- 10
for(i in 1:n){
  x=x*i
  print(x)
}


cumprod(1:10)

cumsum(1:10)

x=numeric(0)
x[1]=1
x[2]=1

for(lina in 3:100){
  x[lina]=x[lina-1]+x[lina-2]
}


for( lina in 1:100){
  print(lina^2)
}


tm=matrix(0, ncol=10, nrow=10)

for(filas in 1:10){
 for(columnas in 1:10){
   tm[filas, columnas]=prod(c(filas,columnas))
 }
}
tm

#### crear funciones

### f(x): x^2   x<1
### log(x)  1 <= x <2
###  sqrt(x)  x >=2

f=function(x){
  if(x<1){
    x^2
  }else if(x>=1 & x<2){
      log(x)
  }else{
      sqrt(x)
    }
}


f(4)
f(0)
f(1.5)

x=c(35,40,38,36,42, 33,38,42,31,41,37,40,
    39,31,39,43, 32, 38,45,34,41,31,44,36,
    49,42,47, NA)

mean(x)

mu=mean(x, na.rm=TRUE)

s=sd(x, na.rm=TRUE)

cbind(mu-2*s/sqrt(27), mu+2*s/sqrt(27))


#########

url="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/Cancer.csv"

cancer=read.csv2(url, sep=";")

apply(cancer,1, mean)

apply(cancer[,-c(1,2)], 2, mean)

apply(cancer[,-c(1,2)], 1, mean)

library(tidyverse)


url="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/Cancer.csv"

cancer=read.csv2(url, sep=";")



#### MAnejo de dataframes con dplyr  ####

### cancer maligno 

cancer %>% filter(Tipo=="M")

### cancer maligno y seleccionar el area y el perimetro

cancer %>% filter(Tipo=="M") %>% select(Area, Perimetro)

### 

cancer %>% group_by(Tipo) %>% count()


resumen=cancer %>% group_by(Tipo) %>% 
  summarise(mArea=mean(Area), mPeri=mean(Perimetro), Conteo=n())


#### Varios filtros

cancer %>% filter(Area>2000, Perimetro >2000 & Perimetro<=7000) %>%
  group_by(Tipo)

### group by ordenado
### 10 más altas de Area por grupo

Base1=cancer %>% 
  group_by(Tipo) %>% 
  arrange(desc(Area)) %>% 
  mutate(Pos_grupo=1:n())%>%
  filter(Pos_grupo<=10)



### 10 más bajas de Area por grupo

Base1=cancer %>% 
  group_by(Tipo) %>% 
  arrange(Area) %>% 
  mutate(Pos_grupo=1:n())%>%
  filter(Pos_grupo<=10) %>%
  summarise(mean(Area)) 
