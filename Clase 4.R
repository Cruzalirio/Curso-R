n=1000  ## Número de pasos
tt=1:n
pos=numeric(n) ## Vector vacio de tamaño n numerico
### X número de personas con miopia en 22 personas
rbinom(100,1, 0.5)  ### 0 es cara, 1 Sello
pos[1]=0
p=0.501 ### Probabilidad de sello
for(i in 2:n){
  x=rbinom(1,1,p)
  if(x==0){
    pos[i]=pos[i-1]-1
  }else{
    pos[i]=pos[i-1]+1
  }
}
pos
plot(pos, type="o")




### Una base

url1="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/ICFES/PruebaSaber1.csv"

url2="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/ICFES/PruebaSaber2.csv"

Icfes1 <- read.csv(url1, sep=";")
Icfes2 <- read.csv(url2, sep=";")


Icfes=rbind(Icfes1, Icfes2)


library(tidyverse)

table(Icfes$ESTU_GENERO)

fig <- ggplot(Icfes, aes(x=ESTU_GENERO))
fig

fig + geom_bar()

fig+ xlab("Genero") + ylab("Conteo")


fig+ geom_bar()+xlab("Genero") + ylab("Conteo")


fig <- fig+ geom_bar()+xlab("Genero") + ylab("Conteo")

fig+ ggtitle("Frecuencias por Genero")

fig <- ggplot(Icfes, aes(x=ESTU_GENERO))+geom_bar(width=0.8,
              colour="red", fill="blue")
fig

colors()

### Un error

fig <- ggplot(Icfes, aes(x=ESTU_GENERO))+geom_histogram(width=0.8, colour="red", fill="blue")
fig


fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL))+
  geom_histogram( colour="red", fill="blue")
fig



#### 

fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL))+
  geom_boxplot( colour="red", fill="blue")
fig




s <- ggplot(mpg, aes(fl, fill = drv))

s + geom_bar(position = "fill")



library(mapproj)

b <- ggplot(mpg, aes(fl))
r <- b + geom_bar()
r + coord_map(projection = "ortho",
              orientation=c(41, -74, 0))

#### 

fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL, y=ESTU_GENERO))+
  geom_boxplot( colour="red", fill="blue")
fig


fig <- ggplot(Icfes, aes(x=PUNT_GLOBAL, y=ESTU_GENERO))+geom_boxplot()
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

gsub("¢","ó","Uni¢n libre")

gsub("¢","ó",c("Uni¢n libre", "Uni¢n", "Alirio", "Algodón"))

gsub("¢","NA",c("Uni¢n libre", "Uni¢n", "Alirio", "Algodón"))


Icfes <- Icfes %>% mutate(ESTU_ESTADOCIVIL= gsub("¢","ó",ESTU_ESTADOCIVIL))

Icfes %>% group_by(ESTU_ESTADOCIVIL) %>% summarise(n())

fig <- ggplot(Icfes,aes(x=ESTU_GENERO, fill=ESTU_ESTADOCIVIL))

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
