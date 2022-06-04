#-----------------------------------------------------------------------------------------
# Abro librerías
library(formattable)
library(plyr)
library(ggbeeswarm)
library(cowplot)
library(wesanderson) 
library(mongolite)
library(dplyr)
library(ggplot2)
library(scales)
library(tibble)
library(fmsb)
library(ggradar)
library(stringr)
library(corrplot) 
library(tidyr)
library(LSD)
library(lubridate)
library(purrr)
library(data.table)
library(RColorBrewer)
library(ggalt)
library(gridExtra)
library(ggnewscale)
library(DAAG)
library(nlme)
library(ggpubr)
library(viridisLite)
library(grid)
library(pBrackets) 

#-----------------------------------------------------------------------------------------
#Función accesoria radarchart
create_beautiful_radarchart <- function(data, color = "#00AFBB", vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
        radarchart(data, axistype = 1,pcol = color, pfcol = scales::alpha(color, 0.4), plwd = 2, plty = 1,
                   cglcol = "grey", cglty = 2, cglwd = .8,axislabcol = "grey", 
                   vlcex = vlcex, vlabels = vlabels, caxislabels = caxislabels, title = title, ...)}



create_beautiful_radarchart1 <- function(data, color = "#00AFBB", vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
    radarchart(data, axistype = 1,pcol = color, plwd = 2, plty = 1,
               cglcol = "grey", cglty = 2, cglwd = .8,axislabcol = "grey", 
               vlcex = vlcex, vlabels = vlabels, caxislabels = caxislabels, title = title, ...)}
#-----------------------------------------------------------------------------------------
# creo theme general para mis gráficos:
theme <- theme(text = element_text(size=10),plot.title = element_text(size=12, face="bold.italic",
               hjust = 0.5), axis.title.x = element_text(size=10, face="bold", colour='black'),
               axis.title.y = element_text(size=10, face="bold"),panel.border = element_blank(),
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_text(face="bold"))

#-----------------------------------------------------------------------------------------
#función accesoria para encontrar índice en cada variable, qué es outlier
extreme_outliers <- function(data) {
        Q1 = quantile(data)[2]
        Q3 = quantile(data)[4]
        IQR = IQR (data)
        # extremos
        valor_Q3 = (IQR * 3) + Q3
        valor_Q1 = Q1 - (IQR * 3)
        result <- which(data > valor_Q3 | data < valor_Q1)}

outliers <- function(data) {
    Q1 = quantile(data)[2]
    Q3 = quantile(data)[4]
    IQR = IQR (data)
    # extremos
    valor_Q3 = (IQR * 1.5) + Q3
    valor_Q1 = Q1 - (IQR * 1.5)
    result <- which(data > valor_Q3 | data < valor_Q1)}
#-----------------------------------------------------------------------------------------
# Abro datasets
charts_original <- mongo(collection='charts', db='spotify')$find() 
features_original <- mongo(collection='AFclean', db='spotify')$find()
#-----------------------------------------------------------------------------------------
# De los charts, me quedo con todo menos URL + unique
charts <- charts_original %>% select (-'URL') %>% unique() 
# Me saco de encima filas vacías en artist name o track name, y luego creo id
charts <- charts[!(is.na(charts$Artist) | charts$Artist==""), ]
charts <- charts[!(is.na(charts$Track_Name) | charts$Track_Name==""), ]
charts$id <- paste(charts$Artist, charts$Track_Name)
#-----------------------------------------------------------------------------------------
# AF1: me quedo con algunas variables, y con unique para eliminar rows idénticas.
# y luego creo un id
AF1 <- features_original%>%
        select(c("artist_name","track_name",
                 "danceability","energy","loudness", 
                 "instrumentalness", "liveness", "valence", "tempo", 
                 "time_signature", "duration_ms", "speechiness", "acousticness","album_type")) %>% unique() 
AF1$id <- paste(AF1$artist_name, AF1$track_name)

# AF2: me quedo con otras variables, y con unique para eliminar rows idénticas.
# y luego creo un id
AF2 <- features_original%>% 
        select(c("artist_name","track_name","album_release_date",
                 "album_release_date_precision", "album_type")) %>% unique()
AF2$id <- paste(AF2$artist_name, AF2$track_name)
#-----------------------------------------------------------------------------------------
# AF1 tiene datos numéricos. Como el id puede aparecer varias veces, antes de mergear
# lo agrupo por id y promedio las variables numéricas
AF1 <- AF1 %>% filter(album_type=='album' | album_type == 'single')%>%
        group_by(id)%>%
        summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 
#-----------------------------------------------------------------------------------------
# AF2 tiene datos fecha. Como el id puede aparecer varias veces, antes de mergear
# me quedo sólo con albumes y singles, me quedo con granularidad "día" y transformo as.Date
# lo agrupo por id y busco la fecha mínima (de creación)
AF2 <- AF2 %>% 
        filter (album_type=='album' | album_type == 'single') %>% 
        filter (album_release_date_precision == 'day') %>%
        mutate_at(vars(album_release_date), as.Date) %>%
        group_by(id) %>%
        summarise(fecha_release=min(album_release_date))
#-----------------------------------------------------------------------------------------
##########################################################################################
#-----------------------------------------------------------------------------------------

# DESCRIPTIVO 1: cantidad de veces que aparecen cada tupla artista-track
nro_tuplas_artista_cancion <- charts %>% group_by(Artist, Track_Name) %>% count() 
par(mar=c(4,4,1,4))
ggplot(data=nro_tuplas_artista_cancion, aes(x=n))+geom_histogram(color='black', fill='red')+theme+labs(title= 'Nro de veces que cada canción aparece en el chart \n(sin mirar permanencia)', x= 'Número apariciones en chart', y='Conteo')

#-----------------------------------------------------------------------------------------
# (DESCRIPTIVO 2) - Audio Features Radar Chart de TOP y BOTTOM 10 (posición)

# Elijo sólo posición hasta 10 y por encima de 190.
df_topbot <- charts %>% filter(Position<11 | Position>190)
# Creo una nueva columna id
df_topbot$id <- paste(df_topbot$Artist, df_topbot$Track_Name)
# Hago INNER join con id (track name + artist name) (así no tengo datos con NA)
M1 <- inner_join(df_topbot,AF1, by='id')
# Hago reescalado de datos para el radarchart
M1escalado <- as.data.frame(round(apply(M1 %>% select(-c(1:9, 21)), 2, scales::rescale), 2))
M2 <- cbind(M1 %>% select(1:3), M1escalado)
# Agarro los posicion hasta 10 y >190, y creo columna CAT
TOP <- M2 %>% filter(Position <11) %>% mutate(CAT='Primeras 10\nposiciones')
BOTTOM <- M2 %>% filter(Position >190)%>% mutate(CAT='Últimas 10\nposiciones')
# Calculo medias de cada grupo CAT y lo uno al máximo ymínimo de mi tabla original reescalada
TOPmean <- TOP%>%summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 
BOTTOMmean <- BOTTOM%>%summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 
df1 <- rbind(TOPmean, BOTTOMmean) %>% select(-c(1:3))%>% column_to_rownames('CAT')
# Variables summary 
MAX10 <- apply(M1escalado, 2, max)
MIN10 <- apply(M1escalado, 2, min)
# Junto summary y las junto con datos
Summary10 <- data.frame(t(data.frame(Max = MAX10, Min = MIN10)))
df_radar <- rbind(Summary10, df1)
df_rad_1 <- df_radar %>% select(-c(3,8,9))
# Radar chart

par(xpd=TRUE,mar=c(2,2,2,2))

create_beautiful_radarchart(df_radar_permanencia,caxislabels=c(0,0.25,.5,.75,1),
                            color = c("#F8766D","#00C0B8"), 
                            vlcex = 0.6, calcex=0.7, palcex=1)
jjjk <- recordPlot() 


#-----------------------------------------------------------------------------------------
# (DESCRIPTIVO 3) - Matriz de correlaciones de audio features 

# me quedo sólo con las variables numéricas
df_corr <- AF1 %>% ungroup %>% select(c(4:14))
#correlation plot
par(mar=c(6,3,2,1))
M=cor(df_corr, use= 'complete.obs') # NO TENER ESTANDARIZADO! (por definición)
corrplot(M, method='color',  type="upper", order="hclust", 
         addCoef.col = "black", number.cex= 7/ncol(df_corr), # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.cex=.7, cl.cex=.5) +
        title(main='Matriz de correlaciones', xlab= 'Variable', ylab='Variable', cex.main=1, cex.lab=.8)
matriz_corr <- recordPlot()

lvf  = na.omit(df_corr)
# Normalizamos 
for(i in 1:ncol(lvf)) {
    lvf[,i] <- (lvf[,i]-min(lvf[,i]))/(max(lvf[,i])-min(lvf[,i]))
}
# Calculamos la varianza para cada atributo
varianzas<-sort(round(apply(lvf, 2, var),4))
varianzas <- t(t(varianzas))
varianzas <- as.data.frame(varianzas)
colnames(varianzas) <- 'Varianza'
varianzas <- cbind(Atributos = rownames(varianzas), varianzas)
rownames(varianzas) <- 1:nrow(varianzas)
vvv <- ggtexttable(varianzas, rows = NULL, 
            theme = ttheme("mOrange"))

ggarrange(matriz_corr, vvv, ncol = 2, nrow = 1, heights=2:1, widths=2:1)

df_corr %>%
         keep(is.numeric) %>% 
         gather() %>% 
         ggplot(aes(value)) +
         facet_wrap(~ key, scales = "free") +
         geom_histogram()

lvf %>%
    keep(is.numeric) %>% 
    gather() %>% 
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()+
    labs(title='Histograma de distribución de aufio features de todas las canciones de la database', x='valor', y='cantidad')+theme


#-----------------------------------------------------------------------------------------
# (DESCRIPTIVO 3) - Relación de posición con otras variables 
Me1 <- inner_ <- oin(charts,AF1, by='id')%>% 
        group_by(Position)%>%
        summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 

Me2 <- as.data.frame(round(apply(Me1 %>% select(-c(1:9,21)), 2, scales::rescale), 2)) # Me1 escalado
Me2 <- cbind(Me1$Position, Me2)
colnames(Me2)[1] <- "Position"
# Grafico ggplot y agrego stat_smooth. Posicion vs AF
Me2 %>% gather(-Position, key = "var", value = "value") %>%
        ggplot(aes(x = value, y = Position)) +
        geom_point() +
        stat_smooth() +
        facet_wrap(~ var, scales = "free") +
        theme_bw() #esto es con datos promedio por posición
# Fancy graph de posición vs duración ms (podemos poner a cualquiera)
ggplot(Me2, aes(Position, duration_ms)) +
        stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +       
        scale_fill_viridis_c() +
        coord_cartesian(expand = FALSE) +
        geom_point(shape = '.', col = 'white')+ theme +
        labs(title='GRAFICO DE DENSIDAD', x= 'Posición en el chart', y= 'Duración canción')
# idem Fancy graph, pero con heatscatter
par(mar=c(6,4,6,.5))
heatscatter(Me2[,1],Me2[,10], main='Heatscatter de \nDuración vs Posición', xlab= 'Posición', ylab='Duración (ms)') #posición vs # duration
# todos contra todos (variables, lo mismo que matriz de correlación)
pairs(Me2, cex=.1) # todos los datos AF + posición (están escalados)
#-----------------------------------------------------------------------------------------
# (DESCRIPTIVO 4) - Relación de posición con EDAD del hit
#primero paso columnas con fechas como tipo "date"
charts <- charts %>% mutate_at(vars(week_start, week_end), as.Date)
df_edad <- left_join(charts,AF2, by='id')
df_edad$edad_track <-  df_edad$week_start- df_edad$fecha_release 
df_edad <- df_edad%>% group_by(Position, week_start, Track_Name, Artist) %>% summarize (edade=max(edad_track)) %>% filter(edade>=0)
df_edad$Position <- as.numeric(df_edad$Position)
df_edad <- df_edad %>% mutate(dias=as.numeric(edade))

df_edad_top <- df_edad %>% filter(Position <11) %>% mutate(categoria='Primeras 10')
df_edad_bottom <- df_edad %>% filter(Position >190) %>% mutate(categoria='Últimas 10')

out_top10 <- outliers(df_edad_top$dias)
out_bottom10 <- outliers(df_edad_bottom$dias)

#length(out_top10)/length(df_edad_top$dias)*100 #-> 4.47% de los datos son outliers
#length(out_bottom10)/length(df_edad_bottom$dias)*100 #-> 8.81% de los datos son outliers

TOPsinoutliers <- df_edad_top [-out_top10,]
BOTTOMsinoutliers <- df_edad_bottom [-out_bottom10,]

df_edad_clean <- rbind(TOPsinoutliers,BOTTOMsinoutliers)

mu <- df_edad_clean%>%ungroup()%>%group_by(categoria)%>%summarise(media=mean(dias), mediana=median(dias))


g1 <- ggplot(data=df_edad_clean, aes(x=categoria, y=dias)) + 
        geom_boxplot(aes(group=categoria),outlier.shape=1, fill=c('darkred','orange'), alpha=0.3, outlier.size=1)+
        theme+ labs(title='Edad canción en cada instancia del chart', x='Categoría', y='Días')
#density plot

g2 <- ggplot(data=df_edad_clean, aes(x=dias, fill=categoria)) + geom_density(alpha=0.4)+
    geom_vline(data=mu, aes(xintercept=media, color=categoria),linetype="dashed", show.legend=T)+
    geom_vline(data=mu, aes(xintercept=mediana, color=categoria),linetype="solid",show.legend=T)+theme+
    labs(y="Densidad", x="Tiempo (días)", title='DISTRIBUCIÓN')+
    theme(legend.position = 'none')+theme(plot.margin = unit(c(2.2,0.5,0.2,0.2), "cm"))+
    annotate(geom='text', x=1000, y=.0075, vjust=0.5, hjust=0, label="MEDIANA", size=2, col='black')+
    annotate(geom='text', x=1000, y=.007, vjust=0.5, hjust=0, label="MEDIA", size=2, col='black')+
    geom_segment(aes(x = 780, y = .0075, xend =930, yend = .0075), col='black')+
    geom_segment(aes(x = 780, y = .007, xend =930, yend = .007), col='black', linetype='dashed')
# analisis para ver cómo le va por año 

df_edad_clean2 <- df_edad_clean%>% ungroup%>%select(c(2,6,7))%>%
    mutate (year= year(week_start))

g3 <- ggplot(data=df_edad_clean2, aes(x = as.factor(year), y = dias)) +
    geom_boxplot(aes(fill=categoria), alpha=0.4, outlier.colour='gray', outlier.shape=1, outlier.size=1) +
    theme+
    labs(title='DESGLOSADO POR AÑO',x="Año", y= "Edad (días)", fill= 'Categoría')+theme(plot.margin = unit(c(2.2,0.5,0.2,0.2), "cm"))

#grid.arrange(g2, g3, nrow = 1, ncol=2, rel_widths = c(1, 2))
plot_grid(g2, g3, align = "h", ncol = 2, rel_widths = c(3/5, 4/5))+
    annotate("text", x=.5, y=.9, size=4, label='atop(bold("Edad de canciones en el chart"),"Comparación primeras y últimas 10 posiciones")', parse=TRUE)

#-----------------------------------------------------------------------------------------
# (DESCRIPTIVO 5) - Análisis de permanencia

df_permanencia <- charts %>% 
        arrange (id, week_start) %>% #ordeno por id y week start, 
        #así tengo agrupado por artista+canción(id) y cronológicamente
        select (c(1,2,3,5,7)) %>% #position, track, artist, weekstart, id
        group_by(id) %>% #para cada grupo artista+track:
        mutate(delta_sem=difftime(week_start, lag(week_start), units = "weeks"))
        #calculo difftime entre esa semana y la anterior 
        #(me da NA para el primer elemento dentro de cada grupo artista+track (id))

df_permanencia$delta_sem <- as.numeric(df_permanencia$delta_sem)%>% replace(is.na(.), 0) 
#reemplazo Na como 0 -> cuando diga 0 sólo estuvo 1 semana en chart. Si le agrego 1 estuvo 2 (and so on...)
# esto lo soluciono al final con "+1" a todos los valores, para hacerlo más interpretable

df_perm <- df_permanencia %>% 
        group_by(id) %>% 
        group_by(IDtemp=data.table::rleid(delta_sem==1)) %>% 
        mutate(Consec_weeks = if_else(delta_sem ==1, row_number(), 0L) +1)
# con esta secuencia de funciones me agrupa por track+autor, y luego crea un nuevo indice
# temporario para aquellos casos donde delta_sem==1 (consecutivos)
# y después creo columna Consec_weeks que pregunta: si delta sem es 1, consec_weeks es el 
# número de fila dentro de ese agrupamiento ((track+artista) + cuando tengo 1 ->soy consec)
# si no, devuelve 0. Al final de todo le sumo 1 a toda esa columna, porque mi semana empieza
# con notación 0, pero en realidad es 1! :) [estuve mil años para llegar a esto!!]

# busco máxima permanencia para cada artista+track(id) y los ordeno de manera descendente
max_perm <-  df_perm %>% group_by(id) %>% summarise(max_perm=max(Consec_weeks)) %>% arrange(desc(max_perm))


# Grafico boxplot 
par(mar=c(3,3,10,2))
ggplot(data=max_perm, aes(x=max_perm)) +
        geom_boxplot(fill='red', alpha=0.3)+
        theme+
        labs(title='Distribución de máxima permanencia (semanas)', x='Máxima permanencia (semanas)')+
        geom_text(x=60, y=0.2, label='y éstos qué onda?', col='blue')+ 
        geom_segment(aes(x = 56, y = 0.17, xend = (80), yend = 0.03), 
                     col='blue', arrow = arrow(length = unit(0.25, "cm")))


max_perm2 <- max_perm
max_perm3<- max_perm2 %>% mutate(es_outlier= ifelse(max_perm %in% boxplot.stats(max_perm2$max_perm)$out, 'YES', 'NO'))
max_perm3<- max_perm3 %>% mutate(es_extreme= ifelse(max_perm %in% max_perm3$max_perm[extreme_outliers(max_perm3$max_perm)], 'SI', 'NO'))
top10_permanencia_prueba<- max_perm3 %>% filter(es_extreme=='SI') %>% arrange(desc(max_perm)) %>% head(10)
pick <- function(condition){function(d) d %>% filter_(condition)}
par(mar=c(12,12,10,12))
max_perm_graph <- ggplot(data=max_perm3, aes(x=factor(1), y=max_perm))+
    geom_boxplot(fill="#FDDDA0", width=.4,outlier.colour = NA)+
    theme+labs(fill=' Outlier\nextremo')+
    geom_point(data = pick(~es_outlier == "YES"), aes(fill=es_extreme), shape=21, position = position_jitterdodge(jitter.width = 0.2,jitter.height = 0.5, dodge.width = 0),show.legend = TRUE)+
    labs(title= "Máxima permanencia de\nlas canciones en el chart", x= "Canciones del chart", y= "Máxima permanencia ininterrumpida (semanas)")+
    scale_fill_manual(values = c('#F39B7FB2','darkred'))+theme(legend.position = c(0.85, 0.84))+
    geom_text(x=.6, y=130, label='Analizamos los\n10 outliers\nmás extremos', col='darkblue',family = 'mono', cex=3)+
    geom_segment(aes(x = .83, y = 160, xend = .83, yend = 110), 
                 col='darkblue', arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm")))
    
    
#geom_point(data = pick(~es_outlier == "NO"), fill='white', color='black', shape=21, position= 'jitter')
    

#https://dbaranger.medium.com/showing-your-data-scatter-box-violin-plots-1f3bb06c8c2b
#library(ggfx)
#library(tidyverse)
#library(data.table)
#library(readxl)
#library(albersusa)
#library(gganimate)
#library(httr)
#library(patchwork)
#library(albersusa)
#library(tigris)
#library(sf)
#library(darklyplot)
# todo esto porque quería poner "with outer glow"
# ------------------------------------------------------------------------------------------
# agarro los top 10 que más tiempo estuvieron en el chart
top10_permanencia <- head(max_perm3, 10)
top10_perm <- inner_join(df_perm, top10_permanencia, by='id')
top10_perm <- top10_perm %>% mutate(TNA= paste(Track_Name, ' (',Artist, ')'))%>% arrange(desc(max_perm))


top11_permanencia <- head(max_perm3, 11)
top11_perm <- inner_join(df_perm, top11_permanencia, by='id')
top11_perm <- top11_perm %>% mutate(TNA= paste(Track_Name, ' (',Artist, ')'))%>% arrange(desc(max_perm))




x <- ggplot(data=top10_perm, aes(x=week_start, y=Position)) +
        ylim(200,1)+
        geom_point(aes(color=TNA, alpha=0.7, size=max_perm))+
        #geom_smooth(aes(color=TNA))+ 
        theme+
        labs(title= 'Posición de los 10 temas con mayor\npermanencia (consecutiva) en el chart',
             x= 'Fecha', y= 'Posición', color= 'Track Name (Artist)')+
        scale_color_brewer(palette="Spectral")+
        geom_vline(xintercept = as.numeric(as.Date("2020-03-11")), linetype=1, col='red')+
        geom_vline(xintercept = as.numeric(as.Date("2020-03-30")), linetype=1)+
        scale_radius(range=c(.2, 3))+guides(alpha=FALSE, size=FALSE)+
        annotate(geom='text', x=as.Date ('2020-01-07') , y=193, vjust=.4, label="COVID\nDeclaración\nde Emergencia", size=2.5, col='red')+
        annotate(geom='text', x=as.Date ('2020-05-22') , y=193, vjust=.4, label="COVID\nDeclaración\nPANDEMIA", size=2.5)+
    theme(legend.key.size = unit(0.15, "cm"), legend.position=c(0.26,0.14))

plot_grid(max_perm_graph, x, align = "h", ncol = 2, rel_widths = c(1/3, 3/5))



ggplot(data=top10_perm, aes(x=week_start, y=Position)) +ylim(200,1)+
        #geom_point(aes(color=TNA, alpha=0.7, size=max_perm))+
        geom_smooth(aes(color=TNA))+ 
        theme(text = element_text(size=10),plot.title = element_text(size=9, face="bold.italic",hjust = .1), 
              axis.title.x = element_text(size=10, face="bold"),
              axis.title.y = element_text(size=10, face="bold"),
              panel.border = element_blank(),panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        labs(title= 'Posición en el tiempo de los 10 temas con mayor permanencia (consecutiva) en el chart',
             x= 'Fecha de entrada al chart', y= 'Posición', color= 'Track Name (Artist)')+
        scale_color_brewer(palette="Spectral")+
        geom_vline(xintercept = as.numeric(as.Date("2020-03-11")), linetype=3)+
        geom_vline(xintercept = as.numeric(as.Date("2020-01-30")), linetype=3, col='red')+
        scale_radius(range=c(.2, 3))+guides(alpha=FALSE, size=FALSE)+
        annotate(geom='text', x=WHO_covid_emergencia, y=193, vjust=.4, label="COVID\nDeclaración\nde Emergencia", size=2.5, col='red')+
        annotate(geom='text', x=WHO_pandemia_COVID, y=193, vjust=.4, label="COVID\nPANDEMIA\nDeclaración", size=2.5)

# los de máxima permanencia, cómo son respecto la media (para los otros valores de AF?)
charts_sin_mas_permanentes <- charts%>%filter(id!=top10_permanencia$id)

all_data <- inner_join(charts, AF1, by='id')
MAPPER <- inner_join(charts_sin_mas_permanentes,AF1, by='id')

MAPPER_esc <- as.data.frame(round(apply(MAPPER %>% select(-c(1:9,21)), 2, scales::rescale), 2))

alldata_esc <- as.data.frame(round(apply(all_data %>% select(-c(1:9,21)), 2, scales::rescale), 2))
alldata_esc$id <- all_data$id

MAX_M10 <- apply(MAPPER_esc, 2, max)
MIN_M10 <- apply(MAPPER_esc, 2, min)
AVG10 <- apply (MAPPER_esc,2, mean, na.rm=TRUE)
MAPPER_esc$id <- MAPPER$id
Summary10b <- data.frame(t(data.frame(Max = MAX_M10, Min = MIN_M10, Promedio_canciones=AVG10)))

TOPPER <- inner_join(top10_perm,alldata_esc, by='id')
TOPPER <- TOPPER %>% select(c(5,9,13:23)) %>% group_by(id) %>% summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
TOPPER <- TOPPER %>% ungroup() %>% select(-c(2:3)) %>% summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
TOPPER$id <- '10 con > permanencia'
TOPPER <- TOPPER %>% column_to_rownames('id')
df_radar_permanencia <- rbind(Summary10b, TOPPER)
df_radar_permanencia <- df_radar_permanencia%>% select(-c(3,4,8,9))
# Radar chart

par(xpd=TRUE,mar=c(2,2,2,2))

create_beautiful_radarchart1(df_radar_permanencia,caxislabels=c(0,0.25,.5,.75,1),pfcol = c(scales::alpha("#440154FF", 0.2), NA), 
           color= c(NA,"#FDE725FF"), vlcex = 0.6, calcex=0.6, palcex=1)
j <- recordPlot() #porque el base plot no me deja asignar a objeto

#----------  
# ---------------------------------------------------------------------------------
# lo mismo pero con BP
# Todos los datos están en MAPPER_esc (datos totales - los de catgoría TOPPER)
# los datos de los top10 + permanentes están en TOPPER (en realidad correr de vuelta, 
# porque topper me quedo con solmaente 1 dato promedio)

BP_mapper <- MAPPER_esc %>% select(-12) 
BP_mapper$var <- 'Promedio'
BP_TOPPER <- inner_join(top10_perm,MAPPER_esc, by='id') %>% 
        select(c(5,9,13,14,16:19,22,23)) %>% 
        group_by(id) %>% 
        summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
        select (-c(1:3))
BP_TOPPER$var <- 'Top10 permanencia'
BP_bd <- rbind(BP_mapper%>%select(-c(3,8,9)), BP_TOPPER)
BP_bd_incomplete <- BP_bd%>%select(-3)

# cambio la df para poder hacer violin plot
violin_bd <- pivot_longer(BP_bd_incomplete, 1:7)

grafiq2 <- ggplot(data = violin_bd,aes(x = name, y = value, fill = var))+
    scale_fill_viridis_d()+theme(plot.margin = unit(c(2.2,0.5,0.2,0.2), "cm"))+ 
    geom_violin(alpha=0.5, position = position_dodge(width = .75),size=1,color=NA,show.legend = F) +
    ggbeeswarm::geom_quasirandom(shape = 21,size=.5, dodge.width = .75, color="gray",alpha=.3,show.legend = F)+
    geom_boxplot(notch = FALSE,  outlier.size = -1, color="black",lwd=.5, alpha = 0.3)+
    labs(x='Variable', y='Valor normalizado', fill='Categoría')+
    theme(text = element_text(size=10), plot.title = element_text(size=12, 
    face="bold.italic",hjust = 0.5), axis.title.x = element_text(size=10, face="bold"),
    axis.text.x = element_text(size=8, angle=45, hjust=1),
    axis.title.y = element_text(size=8, face="bold"),panel.border = element_blank(),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    legend.title = element_text(face="bold"), legend.key.size = unit(0.40, "cm"), 
    legend.position="bottom")

plot_grid(j, grafiq2, align = "h", ncol = 2, rel_widths = c(1,1.3))+
    annotate("text", x=.5, y=.9, size=4, label='atop(bold("Análisis de características de audio"),"Comparación top10 más permanentes vs restantes")', parse=TRUE)


#creo que está mal
#sapply(seq_along(BP_bd)[-1], function(i) {
#        y <- BP_bd[, i-1]
#        boxplot(y ~ BP_bd$var, outline=TRUE, ylab=colnames(BP_bd)[i], tck = 1.0, 
#                names=c("AVG perm","Top 10"), las=1,  col=c('lightgray',2), xlab = element_blank())+theme+
#                mtext(text="Categoría de canción (permanencia)",side=1,line=-0.6,outer=TRUE)+
#                mtext(text="ANÁLISIS DE AUDIOFEATURES\nComparación entre canciones y Top 10 con > permanencia ininterrumpida",side=3,line=-0.4,outer=TRUE)+
#                mtext(text="Variable (audio)",side=2,line=-0.1,outer=TRUE)})


# ########-----------------------------------------------------
#-----------------------------------------------------------------------------------------
# (DESCRIPTIVO 2BIS por año) - Audio Features BOXOPLOT de TOP y BOTTOM 10 (posición) por año

# Elijo sólo posición hasta 10 y por encima de 190 en cada año y creo id
df_topbotxyear <- charts %>% 
        filter(Position<11 | Position>190) %>% 
        group_by(year(week_start)) %>% # con esto ya le estoy creando una columna factor
        mutate (id= paste(Artist, Track_Name)) #creo id
colnames(df_topbotxyear)[8] <- "year"
# Hago INNER join con id (track name + artist name) (así no tengo datos con NA)
M1xyear <- inner_join(df_topbotxyear,AF1, by='id') %>% ungroup()
# Hago reescalado de datos para el radarchart
M1xyearescalado <- as.data.frame(round(apply(M1xyear %>% select(-c(1:10,22)), 2, scales::rescale), 2))
M2xyear <- cbind(M1xyear %>% select(1:3,8), M1xyearescalado)
# Agarro los posicion hasta 10 y >190, y creo columna CAT
M2xyearT <- M2xyear %>% filter(Position <11) %>% mutate(CAT='Primeras 10')
M2xyearB <- M2xyear %>% filter(Position >190)%>% mutate(CAT='Últimas 10')
M3xyear <- rbind(M2xyearT,M2xyearB)
M3xyear <- M3xyear %>% select(c(4:6, 8:11,14:16))

# Grafico ggplot y agrego stat_smooth. Posicion vs AF
umpa <- M3xyear %>% gather(-c(year,CAT), key = "var", value = "value") %>%
        ggplot(aes(x = as.factor(year), y = value)) +theme(plot.margin = unit(c(2.2,0.5,0.2,0.2), "cm"))+
        geom_boxplot(aes(fill=CAT), outlier.colour='gray', outlier.shape=1, outlier.size=1) +
        facet_wrap(~ var, scales = "free") +
        theme+
        labs(x="Año", y= "Valor normalizado de variable (audio)", fill= 'Categoría')+theme(legend.position = c(0.84, 0.12))
   

plot_grid(jjjk, umpa, align = "h", ncol = 2, rel_widths = c(.7,1.3))+
    annotate("text", x=.5, y=.9, size=4, label='atop(bold("Características de audio promedio"),"Comparación de las primeras y últimas 10 canciones en el chart")', parse=TRUE)


#year(strptime(year, '%Y')) # para obtener año (en as.Date) de int 2018
#p <- ggplot()
#grid.arrange(p,p,p,p,p, top = "Análisis de audio features para top y bottom 10",
 #            layout_matrix = matrix(c(1,1,2,3,4,5), ncol=2, byrow=TRUE))




# ---------------------------------------------------------------------------------
# Exito de un artista en el tiempo: 

# calculo Permanencia max alcanzada + Streams promedio + 
# Posición media en el tiempo de todos sus temas + posición minima
# para los 10 más exitosos
# AGRUPADO POR SEMESTRE!


df_streams <- charts %>% 
        mutate(semestre=case_when(week_start >'2020-12-31' ~ '1S_2021',
                                  week_start >'2020-06-30' ~ '2S_2020',
                                  week_start >'2019-12-31' ~ '1S_2020',
                                  week_start >'2019-06-30' ~ '2S_2019',
                                  week_start >'2018-12-31' ~ '1S_2019',
                                  week_start >'2018-06-30' ~ '2S_2018',
                                  week_start >'2017-12-31' ~ '1S_2018')) %>%
        group_by(Artist, semestre) %>%
        mutate(totalstreams=sum(Streams))%>%
        summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 

perm_pos <-  df_perm %>%
    mutate(semestre=case_when(week_start >'2020-12-31' ~ '1S_2021',
                              week_start >'2020-06-30' ~ '2S_2020',
                              week_start >'2019-12-31' ~ '1S_2020',
                              week_start >'2019-06-30' ~ '2S_2019',
                              week_start >'2018-12-31' ~ '1S_2019',
                              week_start >'2018-06-30' ~ '2S_2018',
                              week_start >'2017-12-31' ~ '1S_2018'))%>%
        group_by(Artist, semestre) %>%
        mutate(max_perm=max(Consec_weeks)) %>%
        mutate(mean_pos=mean(Position)) %>%
        mutate(min_pos=min(Position))%>%
    summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 

exito <- inner_join(df_streams, perm_pos, by=c('Artist','semestre'))%>%select(c(1,2,9,17,18,19))
exito$totalstreams <- round(scales::rescale(exito$totalstreams,to = c(1, 100)),5)
exito$max_perm <- round(scales::rescale(exito$max_perm, to = c(1, 100)),5)
exito$min_pos <- round(scales::rescale(exito$min_pos,to = c(1, 100)),5)
exito$mean_pos <- round(scales::rescale(exito$mean_pos,to = c(1, 100)),5)


autores_c_mayor_perm <- top11_perm$Artist%>%unique()  #acá pedí 11 porque se repetía post malone

exito <- exito %>% mutate(indice_exito=((max_perm)*(0.5*mean_pos)*totalstreams)/min_pos)
exito <- exito %>% mutate(en_top10perm= ifelse(Artist %in% autores_c_mayor_perm, 'SI', 'NO'))

exito <- exito %>% group_by(semestre)%>%mutate(es_outlier= ifelse(indice_exito %in% boxplot.stats(exito$indice_exito)$out, 'YES', 'NOo'))%>%
    arrange(semestre,indice_exito)%>%mutate(log=log(indice_exito))

exito <-exito %>%mutate(es_extreme= ifelse(indice_exito %in% exito$indice_exito[extreme_outliers(exito$indice_exito)],'ok','notok'))
exito2 <- exito%>% ungroup()%>%select(c(2,8,10,11))

grafico_indice1 <- ggplot(data=exito2, aes(x=semestre, y=log))+theme+
geom_boxplot(fill='gray', width=.5, outlier.colour = NA)+
geom_point(data=(exito2%>%filter(es_extreme=='notok')),aes(color=en_top10perm), alpha=0.35, size=1.5, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),show.legend = TRUE)+
geom_point(data=(exito2%>%filter(es_extreme=='ok')), aes(fill=en_top10perm), shape=21,  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5),show.legend = TRUE)+
    labs(title= 'Indice de éxito', x='Semestre', y='log (índice éxito)')+guides(fill = FALSE)+
    geom_segment(aes(x = 6.7, y = -2, xend = (7), yend = -1), 
                 col='black', arrow = arrow(length = unit(0.25, "cm")))+
    geom_label(label=" Semestre \nincompleto", x=6.2, y=-2.5, # Rectangle size around label
               label.size = 0.2,
               size=2.5,
               color = "black",
              fill="beige")+
    geom_label(label="puntos con borde son outliers extremos\nen su variable no transformada", x=5.5, y=13.5,
              size=2.5,color = "black")+ylim(-5,14)+
    scale_x_discrete(limits=c('1S_2018','2S_2018','1S_2019','2S_2019','1S_2020','2S_2020','1S_2021'))+
    theme(legend.key.size = unit(0.11, "cm"), legend.position=c(0.2,0.12), axis.text.x=element_text(size=7))
# -------------------------------------------------------------------------------------
# ahora averiguo la historia de exito de los 10 artistas más exitosos en lo que va de 2021
exitosos_2021 <- head(exito %>% filter(semestre=='1S_2021') %>% arrange(desc(indice_exito)),10)$Artist
hist_exitosos2021 <- exito %>% filter (Artist %in% exitosos_2021)

grafico_indice2 <- ggplot(hist_exitosos2021, aes(x=semestre, y=log(indice_exito), fill=Artist, group=Artist))+
    scale_fill_brewer(palette="Spectral")+geom_line(aes(color=Artist))+geom_point(shape=21)+
    scale_color_brewer(palette="Spectral")+ theme+
    theme(plot.subtitle = element_text(color = "black",hjust = 0.5))+
    scale_x_discrete(limits=c('1S_2018','2S_2018','1S_2019','2S_2019','1S_2020','2S_2020','1S_2021'))+
    labs(title='Historia de éxito', subtitle='10 ARTISTAS MÁS EXITOSOS - 2021', x='Semestre', y='log (índice de éxito)')+
    theme(legend.key.size = unit(0.15, "cm"), legend.position=c(0.84,0.22), axis.text.x=element_text(size=7))
    

plot_grid(grafico_indice1, grafico_indice2, align = "h", ncol = 2, rel_widths = c(1.2,1.3))
