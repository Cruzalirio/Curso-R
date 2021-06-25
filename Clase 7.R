library(tidyverse)

library(plotly)

n=50
pos=numeric(n)
p=0.5
pos[1]=0
for( i in 2:n){
  if(rbinom(1,1,p)==1){
    pos[i]=pos[i-1]+1
  }else{
    pos[i]=pos[i-1]-1
  }
}

plot_ly(x=1:n, y=pos, frame=1:n, type = 'scatter',
        mode = 'markers')

datos=data.frame("Paso"=1, "Posicion"=pos[1], "frame"=1)
for( i in 2:n){
  datos=rbind(datos,data.frame("Paso"=1:i, "Posicion"=pos[1:i], "frame"=i))
}

datos %>% plot_ly(x=~Paso, y=~Posicion, frame=~frame, type = 'scatter',
        mode = 'lines')

datos %>% plot_ly(x=~Paso, y=~Posicion, frame=~frame, type = 'scatter',
                  mode = 'lines')%>%
     animation_opts(frame =40,transition = 1, redraw = FALSE)


### Una caminata del borracho
theta=runif(1, 0, 2*pi)
sin(theta)
cos(theta)

n=400
pos_x=numeric(n)
pos_y=numeric(n)
for(i in 2:n){
  theta=runif(1, 0, 2*pi)
  pos_x[i]<-pos_x[i-1]+sin(theta)
  pos_y[i]<-pos_y[i-1]+cos(theta)
}
fig <- plot_ly(x=pos_x, y=pos_y, type="scatter", mode="lines")
fig




datos=data.frame("Pos_x"=pos_x[1], "Pos_y"=pos_y[1], "frame"=1)
for( i in 2:n){
  datos=rbind(datos,data.frame("Pos_x"=pos_x[1:i], "Pos_y"=pos_y[1:i], "frame"=i))
}

datos %>% plot_ly(x=~Pos_x, y=~Pos_y, frame=~frame, type = 'scatter',
                  mode = 'lines')%>%
  animation_opts(frame =200,transition = 1, redraw = FALSE)



### Algo de estadistica

url="https://raw.githubusercontent.com/Cruzalirio/Ucentral/master/Bases/Cancer.csv"

cancer=read_csv2(url)
head(cancer)

cancer %>% plot_ly(x=~Radio, color=~Tipo, type="box")


cancer %>% plot_ly(y=~Radio, color=~Tipo, type="box")


cancer %>% plot_ly(y=~Radio, color=~Tipo, type="violin")

