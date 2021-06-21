library(tidyverse)

Hurtos <- read_csv("C:/Users/nelso/Downloads/Hurto_de_celulares_2018 (2).csv")

head(Hurtos)

summary(Hurtos)

Hurtos$Fecha

### Vamos a trabajar con fechas

library(lubridate)

a <- Hurtos$Fecha[30000]
a

b <- mdy_hms(a)
b

b+1
b+60
b+24*60*60

as.numeric(b)

#### Convertir todo el vector
mdy_hms(Hurtos$Fecha)

Hurtos <- Hurtos %>% mutate(Fecha = mdy_hms(Fecha))
summary(Hurtos)

### Ahora que hacemos con la hora

Hurtos$Hora

mdy_hms(Hurtos$Hora)

Hurtos <- Hurtos %>% mutate(Hora = mdy_hms(Hora))

summary(Hurtos)

hour(Hurtos$Hora)
minute(Hurtos$Hora)
second(Hurtos$Hora)

### Agregamos columnas 

Hurtos <- Hurtos %>% mutate(Agno =year(Fecha), Mes=month(Fecha), Dia=day(Fecha),
                            Hora1=hour(Hora), Minuto=minute(Hora), Segundo=second(Hora))

### problemas?
### Hurtos por mes

Hurtos %>% group_by(Mes) %>% count()

fig <- ggplot(Hurtos %>% group_by(Mes) %>% count(),aes(x=Mes, y=n))
fig +geom_line()


## Gráfica sin sentido
fig1 <- ggplot(Hurtos, aes(x=Dia, y=Cantidad))
fig1 + geom_line()




## 
fig1 <- ggplot(Hurtos %>% group_by(Dia) %>% count(), aes(x=Dia, y=n))
fig1 + geom_line()

### Para gráficar los hurtos por día que??

tabla <- Hurtos %>% group_by(Agno, Mes, Dia) %>% count()

paste("2018", "/", "1","/", "23", sep="")
ymd(paste("2018", "/", "1","/", "23", sep=""))

tabla <- tabla %>% 
  mutate(Dia_agno = ymd(paste(paste(Agno, "/", Mes,"/", Dia, sep=""))))
tabla

fig2 <- ggplot(tabla, aes(x=Dia_agno, y=n))
fig2 + geom_line()
####

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








