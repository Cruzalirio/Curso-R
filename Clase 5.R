library(tidyverse)

Hurtos <- read_csv("C:/Users/nelso/Downloads/Hurto_de_celulares_2018 (2).csv")

head(Hurtos)

### Como cambiamos el nombre de una columna
a=gsub(" ", "_", colnames(Hurtos))
gsub("ó", "o", a)

colnames(Hurtos)=a
colnames(Hurtos)
row.names(Hurtos)

summary(Hurtos)

Hurtos$Fecha

### Vamos a trabajar con fechas

library(lubridate)


a <- Hurtos$Fecha[30000]
a


b <- mdy_hms(a)
b

b+1  ## fecha más un segundo
## La unidad de tiempo es un segundo

b+60
b+24*60*60

as.numeric(b)

as_datetime(1)

dmy_hms("21/06/2021 18:59:00")- dmy_hms("15/07/1992 00:00:00")

date()
today() 

now()

#### Convertir todo el vector
str(Hurtos$Fecha)

str(mdy_hms(Hurtos$Fecha))

Hurtos <- Hurtos %>% mutate(Fecha = mdy_hms(Fecha))

summary(Hurtos)

### Ahora que hacemos con la hora

Hurtos$Hora

mdy_hms(Hurtos$Hora)

Hurtos <- Hurtos %>% mutate(Hora = mdy_hms(Hora))

summary(Hurtos)

hour(Hurtos$Hora)
minute(Hurtos$Hora)

table(second(Hurtos$Hora)==0)

### Agregamos columnas 

Hurtos <- Hurtos %>% mutate(Agno =year(Fecha), Mes=month(Fecha), Dia=day(Fecha),
                            Hora1=hour(Hora), Minuto=minute(Hora), Segundo=second(Hora))



### problemas?
### Hurtos por mes

tabla=Hurtos %>% group_by(Mes) %>% count()
tabla$Acu=cumsum(tabla$n)
tabla

ggplot(tabla, aes(x=Mes, y=Acu)) + geom_line() + geom_line(aes(y=n))

fig <- ggplot(Hurtos %>% group_by(Mes) %>% count(),aes(x=Mes, y=n))
fig +geom_line()


## Gráfica sin sentido
fig1 <- ggplot(Hurtos, aes(x=Dia, y=Cantidad))
fig1 + geom_line()

fig1+geom_point()



Hurtos %>% group_by(Cantidad) %>% count() %>% data.frame()

Hurtos %>% filter(Cantidad==153818) %>% data.frame()

Hurtos <- Hurtos[!Hurtos$Cantidad==153818,]

Hurtos<- Hurtos %>% filter(Cantidad < 153818)

Hurtos <- Hurtos %>% arrange(desc(Cantidad)) %>% data.frame() 

### Hurtos <- Hurtos[-1,] (Esto toca con mucha cuidado)

## 
fig1 <- ggplot(Hurtos %>% group_by(Dia) %>% count(), aes(x=Dia, y=n))
fig1 + geom_line()

### Para gráficar los hurtos por día que??


tabla <- Hurtos %>% group_by(Agno, Mes, Dia) %>% count()

tabla

paste("2018", "/", "1","/", "23", sep="")
ymd(paste("2018", "/", "1","/", "23", sep=""))

tabla <- tabla %>% 
  mutate(Dia_agno = ymd(paste(paste(Agno, "/", Mes,"/", Dia, sep=""))))
tabla

fig2 <- ggplot(tabla, aes(x=Dia_agno, y=n))

fig2 + geom_line()
####
plot(as.ts(rnorm(1000)))

## Como calcular la desviación mediana??

desv_med <- function(x){
  desv_med=sqrt(sum((x-median(x))^2)/(length(x)-1))
}

desv_med(Hurtos$Edad)

desv_med <- function(x){
  desv_med=sqrt(sum((x-median(x))^2)/(length(x)-1))
  return(desv_med)
}

desv_med(Hurtos$Edad)

desv_med <- function(x){
  x=na.omit(x)
  desv_med=sqrt(sum((x-median(x))^2)/(length(x)-1))
  return(desv_med)
}

desv_med(Hurtos$Edad)


Hurtos %>% group_by(Día) %>% summarise(desv_sta=sd(Edad,na.rm=TRUE),
                                       desv_med=desv_med(Edad))


### Ahora vamos a Rmarkdown








