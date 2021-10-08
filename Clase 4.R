
### Una base

url1="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/ICFES/PruebaSaber1.csv"

url2="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/ICFES/PruebaSaber2.csv"

Icfes1 <- read.csv(url1, sep=";")
Icfes2 <- read.csv(url2, sep=";")

names(Icfes1)==names(Icfes2) ### Son las mismas columnas

Comparacion = Icfes1==Icfes2 ### No son las mismas filas

summary(Comparacion)

Icfes=rbind(Icfes1, Icfes2) ### Uniendolas por filas
### row bind, sean las mismas columnas


library(tidyverse)

table(Icfes$ESTU_GENERO) ### Tabla de frecuencias

fig <- ggplot(Icfes, aes(x=ESTU_GENERO))
fig

fig + geom_bar()

fig+ xlab("Genero") + ylab("Conteo")


fig+ geom_bar()+xlab("Genero") + ylab("Conteo")


fig <- fig+ geom_bar()+xlab("Genero") + ylab("Conteo")
## En el objeto fig incluyo a fig, las barras, etiquetas a x y etiquetas a y

fig+ ggtitle("Frecuencias por Genero") ### Además del titulo

fig <- ggplot(Icfes, aes(x=ESTU_GENERO))+geom_bar(width=0.7,
              colour="red", fill="blue")
fig

Icfes$ESTU_GENERO_Mo = factor(Icfes$ESTU_GENERO, levels=c("M", "F"),
                              ordered = TRUE) ### Ordenando

levels(Icfes$ESTU_GENERO_Mo) = c("Masculino", "Femenino")### Cambiando las etiquetas

## Graficar

fig <- ggplot(Icfes, aes(x=ESTU_GENERO_Mo))+geom_bar(width=0.7,
                                                  colour="red", fill="blue")
fig

colors()

### Un error

fig <- ggplot(Icfes, aes(x=ESTU_GENERO))+geom_histogram(width=0.8, colour="red", fill="blue")
fig



### Un histograma
fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL))+
  geom_histogram( colour="red", fill="blue")
fig



#### Un boxplot

fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL))+
  geom_boxplot( colour="red", fill="blue")
fig

#####
fig <- ggplot(Icfes, aes(y=PUNT_GLOBAL))+
  geom_boxplot( colour="red", fill="blue")
fig

### Ejemplo de la ayuda en 
### chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/viewer.html?pdfurl=https%3A%2F%2Fraw.githubusercontent.com%2Frstudio%2Fcheatsheets%2Fmaster%2Fdata-visualization-2.1.pdf&clen=1949514&chunk=true
e <- ggplot(mpg, aes(cty, hwy))

e + geom_label(aes(label = cty), nudge_x = 1, nudge_y = 1) 


c <- ggplot(mpg, aes(hwy)); c2 <- ggplot(mpg)
c + geom_density(kernel = "gaussian") 


fig = ggplot(Icfes, aes(x=MOD_RAZONA_CUANTITAT_PUNT))
fig+geom_density()+ xlab("Puntaje en RC")+
  ylab("Densidad")+ ggtitle("Distribución de los puntajes")


#### 

fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL, y=ESTU_GENERO_Mo))+
  geom_boxplot( colour="red", fill="blue")
fig


fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL, y=ESTU_GENERO))+geom_boxplot()
fig



####

fig <- ggplot(Icfes,aes(x=ESTU_GENERO, fill=ESTU_ESTADOCIVIL))

fig+geom_bar()

fig+geom_bar(position = "dodge")




fig <- ggplot(Icfes,aes(x=ESTU_ESTADOCIVIL, fill=ESTU_GENERO))

fig+geom_bar()

fig+geom_bar(position = "dodge")

Icfes %>% group_by(ESTU_ESTADOCIVIL) %>% summarise(n())


### library string 

gsub("¢","ó","Uni¢n libre") ### reemplaza el simbolo raro por ó

gsub("¢","ó",c("Uni¢n libre", "Uni¢n", "Alirio", "Algodón")) ### Le doy 4 textos

gsub("¢","NA",c("Uni¢n libre", "Uni¢n", "Alirio", "Algodón"))


### Lo qeu voy es a cambiar los simbolos raros
Icfes <- Icfes %>% mutate(ESTU_ESTADOCIVIL= gsub("¢","ó",ESTU_ESTADOCIVIL))

Icfes %>% group_by(ESTU_ESTADOCIVIL) %>% summarise(n())

fig <- ggplot(Icfes,aes(x=ESTU_GENERO, fill=ESTU_ESTADOCIVIL))

fig+geom_bar(position = "dodge")

### ordenar la variable

Icfes %>% group_by(ESTU_ESTADOCIVIL) %>% summarise(n())



Icfes$ESTADO_CIVIL_ORDE = factor(Icfes$ESTU_ESTADOCIVIL,levels=c("Soltero","Unión libre","Casado",
                                                                 "Separado y/o Divorciado", "Viudo",
                                                                 ""),
                                 ordered = TRUE)

Icfes %>% group_by(ESTADO_CIVIL_ORDE) %>% summarise(n())
fig <- ggplot(Icfes,aes(x=ESTU_GENERO, fill=ESTADO_CIVIL_ORDE))

fig+geom_bar(position = "dodge")


fig <- ggplot(Icfes,aes(x=ESTU_GENERO_Mo, fill=ESTADO_CIVIL_ORDE))

fig+geom_bar(position = "dodge")+ scale_fill_discrete(name = "Estado Civil")+
  geom_text()

### Anexar viudo a separado o divorciado

Icfes <- Icfes %>% 
  mutate(ESTADO_CIVIL=replace(ESTU_ESTADOCIVIL,
                              ESTU_ESTADOCIVIL=="Viudo","Separado y/o Divorciado"))


Base1 <-Icfes %>% group_by(ESTU_DEPTO_PRESENTACION, ESTU_GENERO) %>%
  summarise(Conteo=n(), punta_prom=mean(PUNT_GLOBAL))%>%
  filter(Conteo>20) %>% group_by(ESTU_DEPTO_PRESENTACION) %>% 
  summarise(mean(punta_prom))


fig <- ggplot(Icfes,aes(x=ESTU_GENERO, fill=ESTADO_CIVIL))

fig+geom_bar()

fig+geom_bar(position = "dodge")


fig+geom_bar(position = "dodge")+facet_grid(FAMI_TIENEINTERNET ~.)

Icfes %>% group_by(FAMI_TIENEINTERNET) %>% count()


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

for(i in names(Icfes)){
  
}

Icfes %>% group_by(FAMI_TIENEINTERNET1) %>% summarise(n())



### Histogramas

fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_histogram()

fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL))
fig+geom_histogram(binwidth = 81)+facet_grid(.~ESTU_ESTADOCIVIL)


fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_density()+facet_grid(.~ESTU_ESTADOCIVIL)


fig+geom_density()+facet_grid(ESTU_ESTADOCIVIL~.)


fig+geom_density(alpha=0.1)

fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL))
fig+geom_density()+facet_grid(.~ESTU_ESTADOCIVIL)

fig <- ggplot(Icfes%>% filter(ESTU_ESTADOCIVIL %in% c("Soltero", "Unión libre")),
              aes(x=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_density()
fig+geom_density(alpha=0.5)



fig <- ggplot(Icfes%>% filter(ESTU_ESTADOCIVIL %in% c("Soltero", "Casado")),
              aes(x=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_density()
fig+geom_density(alpha=0.5)

Icfes %>% group_by(ESTU_METODO_PRGM)%>% count()

t.test(Icfes%>%filter(ESTU_ESTADOCIVIL=="Soltero")%>%select(PUNT_GLOBAL),
       Icfes%>%filter(ESTU_ESTADOCIVIL=="Casado")%>%select(PUNT_GLOBAL))


fig <- ggplot(Icfes%>% filter(ESTU_METODO_PRGM %in% c("DISTANCIA", "PRESENCIAL")),
              aes(x=PUNT_GLOBAL, fill=ESTU_METODO_PRGM))

fig+geom_density()
fig+geom_density(alpha=0.5)

t.test(Icfes%>%filter(ESTU_METODO_PRGM=="DISTANCIA")%>%select(PUNT_GLOBAL),
       Icfes%>%filter(ESTU_METODO_PRGM=="PRESENCIAL")%>%select(PUNT_GLOBAL))

fig+geom_boxplot()

fig <- ggplot(Icfes,aes(x=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_boxplot()



fig <- ggplot(Icfes,aes(y=PUNT_GLOBAL, fill=ESTU_ESTADOCIVIL))

fig+geom_boxplot()
