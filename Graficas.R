#### Primero vamos a cargar paquetes

library(tidyverse)
library(lubridate)


### carga una base de datos
url="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/ICFES/PruebaSaber2.csv"

Icfes <- read.csv(url, sep=";")

Icfes


d <- ggplot(Icfes, aes(ESTU_GENERO))
d + geom_bar() + xlab("Genero") + 
  ylab("Conteo") + ggtitle("Barras")


### grafico de barras con PAIS donde reside

d <- ggplot(Icfes, aes(ESTU_NACIONALIDAD))

d + geom_bar() + xlab("ESTU_NACIONALIDAD") + 
  ylab("Conteo") + ggtitle("Barras")


### Grafico con más cosas


d <- ggplot(Icfes, aes(ESTU_GENERO, fill=ESTU_ESTADOCIVIL))

d + geom_bar() + xlab("ESTU_NACIONALIDAD") + 
  ylab("Conteo") + ggtitle("Barras")


d + geom_bar(position="dodge") + xlab("ESTU_NACIONALIDAD") + 
  ylab("Conteo") + ggtitle("Barras")


d + geom_bar(position="dodge") + xlab("ESTU_NACIONALIDAD") + 
  ylab("Conteo") + ggtitle("Barras")+
  facet_wrap(.~FAMI_TIENECOMPUTADOR)

### Gráfico de cajas y bigotes 
### Edad y estrato

Icfes$FECHA= dmy(Icfes$ESTU_FECHANACIMIENTO)
hoy=today()

Icfes$Edad =hoy-Icfes$FECHA
Icfes$Edad =Icfes$Edad/365
Icfes$Edad = floor(Icfes$Edad) ## Para años cumplidos

fig = ggplot(Icfes, aes(x=FAMI_ESTRATOVIVIENDA, y=Edad))
fig + geom_boxplot()

Icfes_100 = Icfes %>% filter(Edad<100)

fig = ggplot(Icfes_100, aes(x=FAMI_ESTRATOVIVIENDA, y=Edad))
fig + geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

table(Icfes$FAMI_ESTRATOVIVIENDA)

Icfes$FAMI_ESTRATOVIVIENDA = factor(Icfes$FAMI_ESTRATOVIVIENDA)
levels(Icfes$FAMI_ESTRATOVIVIENDA)

levels(Icfes$FAMI_ESTRATOVIVIENDA)<- c("Sin dato", "NA", "E1", "E2", 
                                       "E3", "E4", "E5", "E6", "Sin estrato")

Icfes_100 = Icfes %>% filter(Edad<100)
fig = ggplot(Icfes_100, aes(x=FAMI_ESTRATOVIVIENDA, y=Edad))
fig + geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Un grafico de barras de el estado civil, 
### con personas mayores de 20 y menores de 40

Icfes %>% filter(Edad>20, Edad<40) %>% ggplot(aes(x=ESTU_ESTADOCIVIL))+
  geom_bar()+theme(axis.text.x = element_text(angle =150, hjust = 1))


Icfes2040 = Icfes %>% filter(Edad>20, Edad<40)

Grafica=ggplot(Icfes2040, aes(x=ESTU_ESTADOCIVIL))+
  geom_bar()+theme(axis.text.x = element_text(angle =45, hjust = 1))
Grafica


#### grafica de densidad y histograma

grafica = ggplot(Icfes_100, aes(x=Edad))

grafica+geom_density() ### Densidad 

grafica+geom_histogram()



grafica = ggplot(Icfes_100, aes(x=Edad))
grafica+geom_density(aes(fill=ESTU_GENERO),alpha=0.8)


grafica = ggplot(Icfes_100, aes(x=Edad))
grafica+geom_density(aes(fill=ESTU_ESTADOCIVIL),alpha=0.8)


grafica = ggplot(Icfes_100, aes(x=Edad))
grafica+geom_density(aes(fill=ESTU_ESTADOCIVIL))+facet_wrap(.~ESTU_ESTADOCIVIL)


### Grafico de densidad de los puntajes de comunicacion escrita
### por genero

grafica = ggplot(Icfes, aes(x=MOD_COMUNI_ESCRITA_PUNT))
grafica+geom_density(aes(fill= ESTU_GENERO),alpha=0.8)+ 
  facet_wrap(.~ESTU_GENERO)


grafica+geom_density(aes(fill= ESTU_GENERO),alpha=0.8)+ 
  facet_grid(ESTU_GENERO~.)


#### Vamos a calcular el puntaje 
## en razonamiento cuantitativo por edad
Icfes_100$Edad =as.numeric(Icfes_100$Edad)

Prom_Edad = Icfes_100 %>% group_by(Edad) %>% 
  summarise(Conteo=n(), Prom_RC=mean(MOD_RAZONA_CUANTITAT_PUNT, na.rm=TRUE))
Prom_Edad


fig= ggplot(Prom_Edad, aes(x=Edad, y=Prom_RC))
fig + geom_line()


### grafica en el eje X la edad, y en el eje y 
## el puntaje de razonamiento cuantitativo y el de
## Comunicacion escrita


Prom_Edad = Icfes_100 %>% group_by(Edad) %>% 
  summarise(Conteo=n(), Prom_RC=mean(MOD_RAZONA_CUANTITAT_PUNT, na.rm=TRUE),
            Prom_CE=mean(MOD_COMUNI_ESCRITA_PUNT, na.rm=TRUE))
Prom_Edad


grafica = ggplot(Prom_Edad, aes(x=Edad, y=Prom_RC, colour="RC"))
grafica+geom_line()+geom_line(aes(y=Prom_CE, colour="CE"))
