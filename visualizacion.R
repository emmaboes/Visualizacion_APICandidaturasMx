# Script para generar visualizaciones a partir de la API CandidaturasMX --------
rm(list = ls())

# Librerias ---------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(mxmaps)
library(leaflet)
library(scales)


# Cargar datos ---------------------------------------------------------
#Endpoints
person <-read_csv("./mx-elections-2021-data/person.csv",locale=locale(encoding="UTF-8"))
contest<-read_csv("./mx-elections-2021-data/contest.csv",locale=locale(encoding="UTF-8"))
area <-read_csv("./mx-elections-2021-data/area.csv",locale=locale(encoding="UTF-8"))
membership<-read_csv("./mx-elections-2021-data/membership.csv",locale=locale(encoding="UTF-8"))
role<-read_csv("./mx-elections-2021-data/role.csv",locale=locale(encoding="UTF-8"))

# Relacionar tablas -------------------------------------------------------
df<-merge(person,contest, by ="contest_id",all=TRUE) %>% 
  merge(area, by="area_id",all=TRUE) %>% 
  merge(membership, by=c("person_id","contest_id"), all=TRUE) %>% 
  merge(role, by="role_id",all=TRUE)

# Se seleccionan variables de interés
df<-as.data.frame(df) %>% 
  select(gender,state,city,membership_type,title.y)
#Transformar género en factor cambiando etiquetas
df$gender<-factor(df$gender, levels = c("1","2"),
                  labels = c("H","M"))
# Filtrar personas ganadoras
win<-df %>% filter(membership_type==1) 


# Visualizaciones ---------------------------------------------------------

## Diputaciones ---------------
### Datos ------
# Se cuenta el número de ganadorxs por estado y género
dip <-win %>% filter(title.y=="Deputy") %>% 
  group_by(state,gender) %>% 
  summarise(n=n()) %>% 
  ungroup()


### Gráfica de barras apiladas------

# Se redefine el orden para acomodar la gráfica 
dip$state<-factor(dip$state, levels = 
    c("CHH","HID","SON","VER","CMX","MOR","QUE","SIN","TAM", "PUE","MEX",
      "BCN","BCS","CAM","COL","DUR","ROO","TAB","ZAC","JAL","GRO","GUA",
      "OAX","YUC","AGU","MIC","NAY","NLE","TLA","CHP","COA","SLP"))

# Se genera la gráfica
  dip %>% ggplot(aes(x=state, y=n, fill= gender)) +
  geom_bar(position="fill",stat="identity", alpha=.6, width=.4) +
# Se ajustan detalles 
     scale_fill_manual(values=c("darkviolet","cyan3"),labels=c("H","M"," "))+
  coord_flip() +
  labs(title = 'Ganadorxs de la contienda a diputaciones por estado y género',
       fill = 'Género',
       x = 'Estado',
       y = 'Porcentaje de ganadorxs')+
  theme_light() +
  theme(
    plot.title=element_text(size =15, face="bold", colour="magenta4"),
    plot.caption =element_text(size=13, color="black"),
    axis.text.x=element_text(size= 12),
    axis.title = element_text(size= 13), 
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size=13))
  

### Mapa estático ------
  
# Datos de mxmaps necesarios para generar el mapa
  df_mxstate_2020

# Se homologan las abreviaciones de los estados
dip$state<-factor(dip$state, 
          levels = 
          c("CHH","HID","SON","VER","CMX","MOR","QUE","SIN","TAM", "PUE",
            "MEX","BCN","BCS","CAM","COL","DUR","ROO","TAB","ZAC","JAL",
            "GRO","GUA","OAX","YUC","AGU","MIC","NAY","NLE","TLA","CHP",
            "COA","SLP"),
          labels = 
            c("CHIH","HGO","SON","VER","CDMX","MOR","QRO","SIN","TAM","PUE",
              "MEX","BC","BCS","CAMP","COL","DGO","QROO","TAB","ZAC","JAL",
              "GRO","GTO","OAX","YUC","AGS","MICH","NAY","NL","TLAX","CHPS",
              "COAH","SLP")
                  ) 

# Se cambia dip a formato wide
    dip_wide<-pivot_wider(dip,names_from = gender,values_from = n)
# Se calcula razón entre hombres y mujeres    
    dip_wide<-dip_wide %>% 
      mutate(value=H/M) # La librería mxmaps requiere una columna value 
      

# Se renombra para unir datos y generar el mapa
 dip_wide <- plyr::rename(dip_wide, c("state"="state_abbr")) 
          
# Se unen los datos
  df_map<-left_join(dip_wide,df_mxstate_2020, by="state_abbr")  
  df_map<-df_map[-33,] #Se elimina ultima fila de NAs
 

# Se genera el mapa
mxstate_choropleth(df_map,
              title = "Ganadorxs de la contienda a diputaciones")+
# Se ajustan detalles
    scale_fill_manual(values=c("#1dd0d3","#6cf4f6","#b3f4f5",
                             "#cdbdf6","#e0a3fa","#b853e4"),
                    na.value= "#9917D0",name="Razón Hombres/Mujeres*")+
  labs(caption= 
"*Se dividió el número de diputados sobre el número de diputadas.
Razón < 1: Hay menos hombres que mujeres.
    Razón = 1: igualdad en el número de representantes de c/género.
Razón > 1: Hay más hombres que mujeres.
" 
  )+
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.caption =element_text(size=12, color="black",hjust = 0.1),
    legend.title = element_text(size=14),
    legend.text = element_text(size = 13)
  )
  
### Mapa interactivo ------
# Se determina una paleta de colores
pal<-colorQuantile(c("#1dd0d3","#6cf4f6","#b3f4f5","#cdbdf6",
                     "#e0a3fa","#b853e4"),df_map$value,n=6,
                   na.color = "#9917D0")

# Se genera el mapa interactivo
mxstate_leaflet(df_map, pal,
                ~ pal(value),
                ~ sprintf("State: %s<br/> Razón Hombres/Mujeres: %s",
                          state_name, comma(value))) %>%
# Se añade una legenda con la escala generada manualmente  
  addLegend(position = "bottomleft", 
            colors =  c("#1dd0d3","#6cf4f6","#b3f4f5","#cdbdf6",
                        "#e0a3fa","#b853e4","#9917D0"),
            labels= c("0.286-0.667","0.667-0.875","0.875-1.111",
                      "1.111-2.000","2","2.25-6.00","SLP 100% hombres"),
                      title = "Ganadorxs de la contienda 
            a diputaciones (razón H/M)",
            values = df_map$value) #%>%
  addProviderTiles("CartoDB.Positron")

### Gráfico de piruleta  ------ 
# Se asigna el 0 a valores perdidos para poder graficar la distancia
dip_wide$M[is.na(dip_wide$M)]<-0

# Se genera la gráfica
dip_wide %>% 
  ggplot() +
  geom_segment( aes(x=state_abbr, xend=state_abbr, y=H, yend=M), color="black") +
  geom_point( aes(x=state_abbr, y=H), color="darkviolet", size=3,alpha=0.5 ) +
  geom_point( aes(x=state_abbr, y=M), color="cyan3", size=3,alpha=0.5 ) +
# Se ajustan detalles 
  coord_flip() +
  labs(title = 'Ganadorxs de la contienda a 
diputaciones por estado y género',
       fill = 'Género',
       x = 'Estado',
       y = 'Numero de ganadorxs*',
       caption = "* Los puntos color azul oscuro indican que el  número de
       ambos grupos está sobrepuesto.
       * El número de ganadores depende del número de distritos.")+
  theme_minimal() +
  theme(
    plot.title=element_text(size =15, face="bold", colour="magenta4"),
    axis.text.x=element_text(size= 13),
    axis.title = element_text(size= 14), 
    axis.text.y = element_text(size = 13))

## Gubernaturas ---------------
### Datos ------
# Se cuenta el número de ganadorxs por estado y género
gub <-win %>% filter(title.y=="Governor") %>% 
  group_by(state,gender) %>% 
  summarise(n=n()) %>% 
  ungroup()  

### Gráfica de pastel ------
# Se genera la gráfica
ggplot(gub,aes(x="", y=n, fill=gender)) +
  geom_bar(stat="identity", width=1) +
# Se ajustan detalles   
  coord_polar("y", start=0)+ #Para hacer gráfico de pastel
  scale_fill_manual(values=c("darkviolet","cyan3"))+
    labs(title = 'Ganadorxs de la contienda a gubernaturas',
       subtitle= "División por género a nivel nacional",
       fill = 'Género',
       caption= "A nivel nacional, 6 mujeres ocuparán \nel cargo de gobernadoras y 
       9 hombres serán gobernadores." 
  )+
  theme_void() +
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        plot.title=element_text(size=15 ,face="bold", colour="magenta4"),
        plot.subtitle=element_text(size=14),
        plot.caption =element_text(size=13, colour = "gray14"))

### Mapa estático 1 ------

# Datos de mxmaps necesarios para generar el mapa
df_mxstate_2020

# Se homologan las abreviaciones de los estados
gub$state<-factor(gub$state, 
                  levels = 
                    c("BCN","BCS","CAM","CHH","COL","GRO","MIC","NAY",
                      "NLE","ROO","SIN","SLP","SON","TLA","ZAC"),
                  labels=
                    c("BC","BCS","CAMP","CHIH","COL","GRO","MICH","NAY",
                      "NL","QRO","SIN","SLP","SON","TLAX","ZAC"))

# Se renombra para unir datos y generar el mapa 
#con las variables value,region
gub <- plyr::rename(gub, c("state"="state_abbr","gender"="value")) 

# Se unen los datos
df_map<-left_join(gub,df_mxstate_2020, by="state_abbr")  

# Se genera el mapa
mxstate_choropleth(df_map, num_colors = 2,
                   title = "Ganadorxs de la contienda a gubernaturas")+
# Se ajustan detalles  
  scale_fill_manual(values=c("darkviolet","cyan3"),na.value="white", 
                    name="Género",labels=c("H","M"," "))

### Mapa estático 2 ------

# Se añade información sobre el género de gobernantes anteriores
    #Info. no disponible en  la API, se consultó y capturó
df_map$gender_prev_gob <-c("H","H","H","H","H","H","H","H","H","H",
                           "H","H","M","H","H")

# Se compara el género de la persona que ocupaba la gubernatura vs
  # la persona que ganó las elecciones 2021
df_map$same_prev_gend<-df_map$gender_prev_gob==df_map$value

# Se renombra la variable que será graficada
df_map <-plyr::rename(df_map,c("value"="gender", "same_prev_gend"="value"))
    #No se pueden tener dos columnas "value"

# Se cambia value de logical a character para poder graficar
df_map$value<-as.factor(df_map$value)

# Se genera el mapa
mxstate_choropleth(df_map,num_colors = 2,
                   title = "Comparación de género: Gubernaturas")+
# Se ajustan detalles
  labs(subtitle="Persona que antes gobernaba vs persona ganadora 2021")+
  scale_fill_manual(values=c("chartreuse2","firebrick"),na.value="white",
                    name="¿Son del mismo género?", labels=c("No*","Si",""))+
  labs(caption= "*Sonora: se tenía gobernadora y \nahora se tendrá gobernador.
        *Resto de estados: se tenía gobernador y \nahora se tendrá gobernadora.")

## Presidencias municipales ---------------

### Gráfica de barras ------

p_mun <-win %>% filter(title.y=="Mayor") %>% 
  group_by(state,gender) %>% 
  summarise(n=n()) %>% 
  ungroup()

# Se redefine el orden para acomodar la gráfica 
p_mun$state<-factor(p_mun$state, levels = 
                    c("CMX","MEX","VER","YUC","ZAC","GUA","MOR","COA",
                      "BCN","CHH","OAX","MIC","COL","SON","AGU","BCS",
                      "CAM","CHP","QUE","ROO","SIN","SLP","TLA","NLE",
                      "DUR","GRO","JAL","NAY","PUE"))


# Se genera la gráfica
ggplot(p_mun,aes(x=state, y=n, fill= gender)) +
  geom_bar(stat="identity", alpha=.6, width=.4) +
# Se ajustan detalles 
  coord_flip() +
  scale_fill_manual(values=c("darkviolet","cyan3"))+
  labs(title = 'Ganadorxs de la contienda a \npresidencias municipales por estado y género',
       fill = 'Género',
       x = 'Estado°',
       y = 'Número de ganadorxs*',
       caption = " ° Ordenados dependiendo de los estados con mayor y menor número
       de municipios capturados en la API 
       *El número de ganadorxs depende del número de municipios capturados. 
          *En el caso de la CMX son alcaldías.  ")+
  theme_light()+
  theme(
    plot.title=element_text(size =15, face="bold", colour="magenta4"),
    plot.caption =element_text(size=13, color="black"),
    axis.text.x=element_text(size= 12),
    axis.title = element_text(size= 13), 
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size=13)
  )
