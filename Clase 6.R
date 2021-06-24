library(tidyverse)
## cargar una base 

urls<-paste("https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/ICFES/PruebaSaber",1:12,".csv", sep="")
urls


datos=read_csv2(urls[1])


for( i in urls[-1]){
  print(i)
  datos <- rbind(datos, read_csv2(i))
}

n=length(urls)
for( i in 2:n){
  print(i)
  datos <- rbind(datos, read_csv2(urls[i]))
}



nombres <- c("Al", "El", "Il", "Ol", "Ul")

for(i in nombres){
  print(i)
}


for(i in 1:length(nombres)){
  print(nombres[i])
}


## Algo de validacion

datos %>% group_by(FAMI_CUARTOSHOGAR) %>% count()

datos <-datos %>% mutate(FAMI_CUARTOSHOGAR= replace(FAMI_CUARTOSHOGAR,
                         FAMI_CUARTOSHOGAR %in% c("Una", "Uno"),"Uno"))

datos <-datos %>% mutate(FAMI_CUARTOSHOGAR= replace(FAMI_CUARTOSHOGAR,
                FAMI_CUARTOSHOGAR==c("Diez o más"),"Diez o mas"))

datos %>% group_by(FAMI_CUARTOSHOGAR) %>% count()

datos %>% filter(FAMI_CUARTOSHOGAR=="Diez o m\xa0s")

### Quitar lo que no sea alfanumerico

cuartos <-str_replace(datos$FAMI_CUARTOSHOGAR, "[^[:alnum:]]", "")

cuartos <- as.factor(cuartos)

levels(cuartos) <- c("Cinco","Cuatro","Diez o más","Dos","Nueve","Ocho","Seis",     
                     "Siete","Tres","Uno")

cuartos <- ordered(cuartos, levels=c("Uno", "Dos", "Tres", "Cuatro", "Cinco",
                                     "Seis", "Siete", "Ocho", "Nueve", "Diez o más"))

cuartos
datos <- datos %>% mutate(FAMI_CUARTOSHOGAR=cuartos)

cuartos
datos %>% group_by(FAMI_CUARTOSHOGAR) %>% count()


as.numeric(cuartos)


faltantes <-datos %>% filter(is.na(FAMI_CUARTOSHOGAR)) %>% data.frame()
write_csv2(faltantes, "faltantes.csv")
getwd()


### Usemos plotly

library(plotly)

fig <- plot_ly(datos, labels=~ESTU_GENERO, type="pie")

fig

fig %>% layout(title="Un pastel")

####
###Instalar la libreria magic

### Que es una lista?

caneca=list("obj1"=datos, "obj2"=fig, "faltantes"=faltantes, "links"=urls)
caneca$obj1

caneca[1]

caneca[[1]]
caneca$obj2
print(fig)

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)')

fig <- plot_ly(datos, labels = ~ESTU_GENERO, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               showlegend = FALSE)
fig <- fig %>% layout(title = 'Genero de los evaluados',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


fig
### Uno de dispersion

datos %>% plot_ly(x=~MOD_RAZONA_CUANTITAT_PUNT, y=~MOD_LECTURA_CRITICA_PUNT, type="scatter")



datos %>% plot_ly(x=~MOD_RAZONA_CUANTITAT_PUNT, y=~MOD_LECTURA_CRITICA_PUNT, type="scatter",
                  color =~ESTU_GENERO)

datos %>% plot_ly(x=~MOD_RAZONA_CUANTITAT_PUNT, y=~MOD_LECTURA_CRITICA_PUNT, type="scatter",
                  color =~ESTU_GENERO, mode="markers")


### No tiene tanta logica

datos %>% group_by(ESTU_GENERO, ESTU_DEPTO_RESIDE) %>% 
  summarise(Conteo=n(), Razo_cuan = mean(MOD_RAZONA_CUANTITAT_PUNT),
            Lec_crit = mean(MOD_LECTURA_CRITICA_PUNT)) 


datos %>% group_by(ESTU_GENERO, ESTU_DEPTO_RESIDE) %>% 
  summarise(Conteo=n(), Razo_cuan = mean(MOD_RAZONA_CUANTITAT_PUNT),
            Lec_crit = mean(MOD_LECTURA_CRITICA_PUNT)) %>%
  plot_ly(x=~Razo_cuan, y=~Lec_crit, type="scatter", mode="markers", color =~ESTU_GENERO)


datos %>% group_by(ESTU_GENERO, ESTU_DEPTO_RESIDE) %>% 
  summarise(Conteo=n(), Razo_cuan = mean(MOD_RAZONA_CUANTITAT_PUNT),
            Lec_crit = mean(MOD_LECTURA_CRITICA_PUNT)) %>%
  plot_ly(x=~Razo_cuan, y=~Lec_crit, type="scatter", mode="markers",
          color =~ESTU_GENERO, marker=list(size=~Conteo))


datos %>% group_by(ESTU_GENERO, ESTU_DEPTO_RESIDE) %>% 
  summarise(Conteo=n(), Razo_cuan = mean(MOD_RAZONA_CUANTITAT_PUNT),
            Lec_crit = mean(MOD_LECTURA_CRITICA_PUNT)) %>%
  plot_ly(x=~Razo_cuan, y=~Lec_crit,text=~ESTU_DEPTO_RESIDE, type="scatter", mode="markers",
          color =~ESTU_GENERO, marker=list(size=~Conteo*0.002, opacity=0.5))


### Problema con el depto

datos%>% group_by(ESTU_DEPTO_RESIDE) %>% count() %>% data.frame()

datos %>% filter(ESTU_DEPTO_RESIDE=="NARIÑO")

datos %>% filter(ESTU_DEPTO_RESIDE=="NARI<U+00A5>O")

### Eliminamos lo que no reconozca

str_replace(datos$ESTU_DEPTO_RESIDE, "[^[:alnum:]]", "")

datos <- datos %>% mutate(ESTU_DEPTO_RESIDE = str_replace(ESTU_DEPTO_RESIDE,"[^[:alnum:]]", "" ))

datos%>% group_by(ESTU_DEPTO_RESIDE) %>% count() %>% data.frame()

datos %>% group_by(ESTU_GENERO, ESTU_DEPTO_RESIDE) %>% 
  summarise(Conteo=n(), Razo_cuan = mean(MOD_RAZONA_CUANTITAT_PUNT),
            Lec_crit = mean(MOD_LECTURA_CRITICA_PUNT)) %>%
  plot_ly(x=~Razo_cuan, y=~Lec_crit,text=~ESTU_DEPTO_RESIDE, type="scatter", mode="markers",
          color =~ESTU_GENERO, marker=list(size=~Conteo*0.003, opacity=0.5))


datos %>% filter(!is.na(ESTU_GENERO)) %>%
  group_by(ESTU_GENERO, ESTU_DEPTO_RESIDE) %>% 
  summarise(Conteo=n(), Razo_cuan = mean(MOD_RAZONA_CUANTITAT_PUNT),
            Lec_crit = mean(MOD_LECTURA_CRITICA_PUNT)) %>%
  plot_ly(x=~Razo_cuan, y=~Lec_crit,text=~ESTU_DEPTO_RESIDE, type="scatter", 
          mode="text", colors=c("Red", "Green"),
          color =~ESTU_GENERO, marker=list(size=~Conteo*0.005, opacity=0.5))


datos %>% filter(!is.na(ESTU_GENERO)) %>%
  group_by(ESTU_GENERO, ESTU_DEPTO_RESIDE) %>% 
  summarise(Conteo=n(), Razo_cuan = mean(MOD_RAZONA_CUANTITAT_PUNT),
            Lec_crit = mean(MOD_LECTURA_CRITICA_PUNT)) %>%
  plot_ly(x=~Razo_cuan, y=~Lec_crit,text=~ESTU_DEPTO_RESIDE, type="scatter", 
          mode="markers", colors=c("Red", "Green"),
          marker=list(size=~Conteo*0.005, opacity=0.5))



datos %>% group_by(FAMI_EDUCACIONMADRE) %>% summarise(conteo=n()) %>% 
  plot_ly(y=~conteo, x=~FAMI_EDUCACIONMADRE, type="bar")

datos <- datos %>% mutate(FAMI_EDUCACIONMADRE=  str_replace(FAMI_EDUCACIONMADRE,"[^[:alnum:]]", "" ))

datos %>% group_by(FAMI_EDUCACIONMADRE) %>% summarise(conteo=n()) %>% 
  plot_ly(y=~conteo, x=~FAMI_EDUCACIONMADRE, type="bar")


datos <- datos %>% mutate(FAMI_EDUCACIONMADRE=  str_replace(FAMI_EDUCACIONMADRE,"[[:punct:]]", "" ))
datos %>% group_by(FAMI_EDUCACIONMADRE) %>% summarise(conteo=n()) %>% 
  plot_ly(y=~conteo, x=~FAMI_EDUCACIONMADRE, type="bar")

##### No funciona, odio esto

aa <- factor(datos$FAMI_EDUCACIONMADRE)

levels(aa)

factor(datos$FAMI_EDUCACIONMADRE, 
       labels=c( "Educacin profesional completa","Educacion profesional incompleta",  
                 "Ninguno","Postgrado","Primaria completa" ,"Primariaincompleta",
                 "Secundaria Bachillerato) completa","SecundariaBachillerato) incompleta",
                 "Tecnica o tecnologica completa" ,"Tecnica o tecnologica incompleta"   ))

datos <- datos %>% 
  mutate(FAMI_EDUCACIONMADRE= factor(datos$FAMI_EDUCACIONMADRE, 
  labels=c( "Educacin profesional completa","Educacion profesional incompleta",  
   "Ninguno","Postgrado","Primaria completa" ,"Primariaincompleta",
     "Secundaria Bachillerato) completa","SecundariaBachillerato) incompleta",
                                                                "Tecnica o tecnologica completa" ,"Tecnica o tecnologica incompleta"   )))

datos %>% group_by(FAMI_EDUCACIONMADRE) %>% summarise(conteo=n()) %>% 
  plot_ly(y=~conteo, x=~FAMI_EDUCACIONMADRE, type="bar")

#### Hagamos unos más divertidos

## Un grafico de radar

datos%>% group_by(FAMI_TIENEINTERNET) %>% summarise(n())

datos<-datos %>% 
  mutate(FAMI_TIENEINTERNET=replace(FAMI_TIENEINTERNET, !FAMI_TIENEINTERNET%in%c("No", "Si"), NA ))

datos%>% group_by(FAMI_TIENEINTERNET) %>% summarise(n())


datos %>% group_by(FAMI_TIENEINTERNET) %>% summarise(conteo=n(), p1=mean(MOD_RAZONA_CUANTITAT_PUNT),
        p2=mean(MOD_LECTURA_CRITICA_PUNT), p3=mean(MOD_COMPETEN_CIUDADA_PUNT),
        p4=mean(MOD_INGLES_PUNT), p5=mean(MOD_COMUNI_ESCRITA_PUNT))

datos %>% group_by(FAMI_TIENEINTERNET) %>%
  summarise(conteo=n(), p1=mean(MOD_RAZONA_CUANTITAT_PUNT),
  p2=mean(MOD_LECTURA_CRITICA_PUNT), p3=mean(MOD_COMPETEN_CIUDADA_PUNT),
  p4=mean(MOD_INGLES_PUNT), p5=mean(MOD_COMUNI_ESCRITA_PUNT, na.rm=TRUE))


tabla <-datos %>% group_by(ESTU_DEPTO_RESIDE) %>%
  summarise(conteo=n(), p1=mean(MOD_RAZONA_CUANTITAT_PUNT),
            p2=mean(MOD_LECTURA_CRITICA_PUNT), p3=mean(MOD_COMPETEN_CIUDADA_PUNT),
            p4=mean(MOD_INGLES_PUNT), p5=mean(MOD_COMUNI_ESCRITA_PUNT, na.rm=TRUE))

tabla

fig <- plot_ly(type= 'scatterpolar', r=t(tabla[,3:7])[,1], theta=c("RC", "LC", "CC", "IN", "CE"),
               fill="toself", mode="markers")

fig



fig <- plot_ly(type= 'scatterpolar', r=t(tabla[,3:7])[,1], theta=c("RC", "LC", "CC", "IN", "CE"),
               fill="toself", mode="markers", name=tabla[1,1])

fig %>% add_trace(r=t(tabla[,3:7])[,2], theta=c("RC", "LC", "CC", "IN", "CE"),
                  fill="toself", mode="markers", name=tabla[2,1])

fig %>% add_trace(r=t(tabla[,3:7])[,3], theta=c("RC", "LC", "CC", "IN", "CE"),
                  fill="toself", mode="markers", name=tabla[3,1])

fig <- plot_ly(type= 'scatterpolar', r=t(tabla[,3:7])[,1], theta=c("RC", "LC", "CC", "IN", "CE"),
               fill="toself", mode="markers", name=tabla[1,1])

for(i in 2:nrow(tabla)){
  fig <- fig %>% add_trace(r=t(tabla[,3:7])[,i], theta=c("RC", "LC", "CC", "IN", "CE"),
                           fill="toself", mode="markers", name=tabla[i,1])
  fig
}

fig


## Una mezcla fea

deptos <- c("AMAZONAS", "CAQUETA", "GUAINIA", "GUAVIARE", "PUTUMAYO", "VAUPES")

fig <- plot_ly(type= 'scatterpolar', r=t(tabla[,3:7])[,1], theta=c("RC", "LC", "CC", "IN", "CE"),
               fill="toself", mode="markers", name=tabla[1,1])

for(i in 2:nrow(tabla)){
  if(tabla[i,1] %in% deptos){
  fig <- fig %>% add_trace(r=t(tabla[,3:7])[,i], theta=c("RC", "LC", "CC", "IN", "CE"),
                           fill="toself", mode="markers", name=tabla[i,1])
  }
}

fig
