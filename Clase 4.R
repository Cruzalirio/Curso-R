### Una base

url1="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/ICFES/PruebaSaber1.csv"

url2="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/ICFES/PruebaSaber2.csv"

Icfes1 <- read.csv(url1, sep=";")
Icfes2 <- read.csv(url2, sep=";")


Icfes=rbind(Icfes1, Icfes2)


library(tidyverse)



fig <- ggplot(Icfes, aes(x=ESTU_GENERO))

fig + geom_bar()

fig+ xlab("Genero") + ylab("Conteo")


fig+ geom_bar()+xlab("Genero") + ylab("Conteo")


fig <- fig+ geom_bar()+xlab("Genero") + ylab("Conteo")

fig+ ggtitle("Frecuencias por Genero")

fig <- ggplot(Icfes, aes(x=ESTU_GENERO))+geom_bar(width=0.8, colour="red", fill="blue")
fig


### Un error

fig <- ggplot(Icfes, aes(x=ESTU_GENER))+geom_histogram(width=0.8, colour="red", fill="blue")
fig


fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL))+geom_histogram( colour="red", fill="blue")
fig



#### 

fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL))+geom_boxplot( colour="red", fill="blue")
fig


#### 

fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL, y=ESTU_GENERO))+geom_boxplot( colour="red", fill="blue")
fig


fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL, y=ESTU_GENERO))+geom_boxplot()
fig


fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL, y=ESTU_GENERO))+geom_boxplot()
fig

####

fig <- ggplot(Icfes,aes(x=ESTU_GENERO, fill=ESTU_ESTADOCIVIL))

fig+geom_bar()

fig+geom_bar(position = "dodge")




fig <- ggplot(Icfes,aes(x=ESTU_GENERO, fill=ESTU_ESTADOCIVIL))

fig+geom_bar()

fig+geom_bar(position = "dodge")

Icfes %>% group_by(ESTU_ESTADOCIVIL) %>% summarise(n())

gsub("¢","ó","Uni¢n libre")
gsub("¢","ó",c("Uni¢n libre", "Uni¢n"))

Icfes <- Icfes %>% mutate(ESTU_ESTADOCIVIL= gsub("¢","ó",ESTU_ESTADOCIVIL))

fig <- ggplot(Icfes,aes(x=ESTU_GENERO, fill=ESTU_ESTADOCIVIL))

fig+geom_bar()

fig+geom_bar(position = "dodge")


fig+geom_bar(position = "dodge")+facet_grid(FAMI_TIENEINTERNET ~.)

fig+geom_bar(position = "dodge")+facet_grid(.~FAMI_TIENEINTERNET)


## Que hacemos?

Icfes %>% group_by(FAMI_TIENEINTERNET) %>% summarise(n())

Icfes<- Icfes %>% mutate(FAMI_TIENEINTERNET1 =replace(FAMI_TIENEINTERNET,
            FAMI_TIENEINTERNET =="", NA))

Icfes %>% group_by(FAMI_TIENEINTERNET1) %>% summarise(n())

Icfes<- Icfes %>% mutate(FAMI_TIENEINTERNET1 =replace(FAMI_TIENEINTERNET,
             FAMI_TIENEINTERNET %in% c("No", "Si"), NA))

Icfes %>% group_by(FAMI_TIENEINTERNET1) %>% summarise(n())


Icfes<- Icfes %>% mutate(FAMI_TIENEINTERNET1 =replace(FAMI_TIENEINTERNET,
                !FAMI_TIENEINTERNET %in% c("No", "Si"), NA))

Icfes %>% group_by(FAMI_TIENEINTERNET1) %>% summarise(n())



### Histogramas

fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_histogram()

fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL))
fig+geom_histogram(binwidth = 81)+facet_grid(.~ESTU_ESTADOCIVIL)


fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_density()
fig+geom_density(alpha=0.5)

fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL))
fig+geom_density()+facet_grid(.~ESTU_ESTADOCIVIL)

fig <- ggplot(Icfes%>% filter(ESTU_ESTADOCIVIL %in% c("Soltero", "Unión libre")),
              aes(x=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_density()
fig+geom_density(alpha=0.5)


fig+geom_boxplot()

fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_boxplot()



fig <- ggplot(Icfes,aes(y=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_boxplot()
