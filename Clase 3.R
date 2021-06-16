
url="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/Global_Carbon_Budget_2018.csv"

carbon=read.csv(url, header = T, sep=";", dec=",")

## Como se manejan las gráficas 


plot(carbon$Year)

plot(carbon$Year, carbon$fossil.fuel.and.industry)

plot(carbon$Year, carbon$fossil.fuel.and.industry, color="red")

plot(carbon$Year, carbon$fossil.fuel.and.industry, col="red")

plot(carbon$Year, carbon$fossil.fuel.and.industry,
     col="red", type="l")

boxplot( carbon$fossil.fuel.and.industry)





#### vamos por los ciclos de R ####
#### Tema libre                ####
i=1000
while (i>0){
  print(paste("Arrancamos en ", i, sep = ""))
  rnorm(100000)
  i=i-1
}

### ifelse

carbon$nueva_var = ifelse(carbon$Year<1950, "Viejo", "Nuevo")
table(carbon$nueva_var)

boxplot(carbon$fossil.fuel.and.industry~carbon$nueva_var)

x <-300
if(x<5){
  print(x^2)
}


if(x<5){
  print(x^2)
}else{
  print(x+8)
}


if(x<5){
  print(x^2)
}else if( x<10){
  print(paste(x+8, "Yuju"))
}else{
  print(paste("algo es", x))
}



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

x=0
x[1]=1
x[2]=1

for(i in 3:20){
  x[i]=x[i-1]+x[i-2]
}


#### crear funciones

### f(x): sen(x)   x<1
### log(x)  1 <= x <2
###  sqrt(x)  x>=3

f=function(x){
  if(x<1){
    sin(x)
  }else if(x>=1 & x<2){
      log(x)
  }else{
      sqrt(x)
    }
}


g(1:10)

curve(f, from=-10, to=10)
