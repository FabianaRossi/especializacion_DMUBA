#https://raw.githubusercontent.com/dm-uba/dm-uba.github.io/master/2021/laboratorios/LAB08/scripts/download-lyrics-v2.r
# usan el código de arriba para buscar todas las canciones que están en los charts (español e inglés)
#-----------------------------------------------------------------------------------------
# Abro librerías
library(rlist)
library(tibble)
library(qdapTools)
library(formattable)
library(plyr)
library(ggbeeswarm)
library(cowplot)
library(wesanderson) 
library(mongolite)
library(dplyr)
library(ggplot2)
library(scales)
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
library(dummies)
library(arules)
library(arulesViz)

#-----------------------------------------------------------------------------------------
# Abro datasets (están colgados en drive https://drive.google.com/drive/folders/1ym38Camd1k_cU7hF0XPi8Bbvt7LcroXd)
charts_original <- mongo(collection='charts', db='spotify')$find() # charts es charts-dm.zip
features_original <- mongo(collection='AFclean', db='spotify')$find() # AFclean es artist_audio_features_solo_art-dm.json
#-----------------------------------------------------------------------------------------
# PRE PROCESAMIENTO
#De los charts, me quedo con todo menos URL + unique
charts <- charts_original %>% select (-'URL') %>% unique() 
# Me saco de encima filas vacías en artist name o track name, y luego creo id
charts <- charts[!(is.na(charts$Artist) | charts$Artist==""), ]
charts <- charts[!(is.na(charts$Track_Name) | charts$Track_Name==""), ]
charts$id <- paste(charts$Artist, charts$Track_Name) # tabla de 31400 x 7
#-----------------------------------------------------------------------------------------
# CREO 3 BASES DE DATOS DE AUDIO FEATURES, QUE LUEGO VOYA MERGEAR (CADA UNA TIENE SU PREPROCESAMIENTO PARTICULAR):

# AF1, QUE TIENE TODAS VARIABLES NUMÉRICAS Y DEL AUDIO EN SÍ
# AF2, QUE TIENE INFORMACIÓN DEL ÁLBUM (RELEASE Y TIPO)
# AF3, QUE TIENE DATOS DE PAISES

# AF1: me quedo con algunas variables, y con unique para eliminar rows idénticas.
# y luego creo un id
AF1 <- features_original%>% select(c("artist_name","track_name", "danceability","energy","loudness", "instrumentalness", "liveness", "valence", "tempo", "time_signature", "duration_ms", "speechiness", "acousticness","album_type")) %>% unique() 
AF1$id <- paste(AF1$artist_name, AF1$track_name)
# AF1 tiene datos numéricos. Como el id puede aparecer varias veces, antes de mergear lo agrupo por id y promedio las variables numéricas
# ESTOY DESCARTANDO COMPILADOS !!!
AF1 <- AF1 %>% filter(album_type=='album' | album_type == 'single')%>%group_by(id)%>% summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 
# AF1 tabla de 81531 x 15

# AF2: me quedo con variables de fecha de release, y con unique para eliminar rows idénticas.
# y luego creo un id
AF2 <- features_original%>%select(c("artist_name","track_name","album_release_date","album_release_date_precision", "album_type")) %>% unique()
AF2$id <- paste(AF2$artist_name, AF2$track_name)
# AF2 tiene datos fecha. Como el id puede aparecer varias veces, antes de mergear: me quedo sólo con albumes y singles, me quedo con granularidad "día" y transformo as.Date
# lo agrupo por id y busco la fecha mínima (de creación)
# ESTOY DESCARTANDO COMPILADOS Y PRECISION!= DAY (TIRO 3% DATOS)
AF2 <- AF2 %>% filter (album_type=='album' | album_type == 'single') %>% filter (album_release_date_precision == 'day') %>%mutate_at(vars(album_release_date), as.Date) %>%group_by(id) %>%summarise(fecha_release=min(album_release_date))
# AF2 tabla de 78596 x 2


# AF3: me quedo con la lista más grande de países para cada canción
AF3 <- features_original%>% select(c("artist_name","track_name", "available_markets"))%>% mutate(cant_paises=lengths(available_markets)) #le agrego una columna contando cuantos paises hay
AF3 <- cbind(AF3, mtabulate(AF3$available_markets)) 
AF3$id <- paste(AF3$artist_name, AF3$track_name)
AF3 <- AF3 %>%group_by(id)%>% slice(which.max(cant_paises)) #slicing dentro del grupo 
AF3 <- AF3%>%select(-c(1:2))
# AF3 tabla de 84920 x 181 variables (las dummies)

# FINALMENTE HAGO UN MERGE DE LOS AUDIOFEATURES
audiofeatures <- inner_join(AF1,AF2, by='id') # 78596 x 16
audiofeatures <- inner_join(audiofeatures,AF3, by='id') 
audiofeatures <- audiofeatures%>%select(-17) # le saco la columna available_markets (lista de listas) y queda 78596 x 195

#write.csv(charts,"charts.csv", row.names = FALSE) # por si interesa
#write.csv(audiofeatures,'audiofeatures.csv', row.names = FALSE) # por si interesa

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################


# qué normalizaciones se le debe hacer a la base:

pre_st <- audiofeatures %>% select(4:14)
pre_st %>% gather() %>% ggplot(aes(value)) +facet_wrap(~ key, scales = "free") + geom_histogram()
est_log <- log(pre_st)
est_log%>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()
est_log10 <- log10(pre_st)
est_log10%>% gather() %>% ggplot(aes(value)) +facet_wrap(~ key, scales = "free") +geom_histogram()
est_sq <- sqrt(pre_st)
est_sq%>% gather() %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") +geom_histogram()
est_1_sq <- 1/sqrt(pre_st)
est_1_sq%>% gather() %>% ggplot(aes(value)) +facet_wrap(~ key, scales = "free") +geom_histogram()


sesgo <- function(data) {media = mean(data, na.rm=T)
        mediana = median(data,na.rm=T)
        sd=sd(data, na.rm=T)
        result <- (3* (media - mediana)) / sd}


a <- pre_st %>%filter_all(all_vars(!is.infinite(.))) %>% summarise(across(everything(), list(sesgo)))
b <- est_log%>%filter_all(all_vars(!is.infinite(.))) %>% summarise(across(everything(), list(sesgo)))
c <- est_log10%>%filter_all(all_vars(!is.infinite(.))) %>% summarise(across(everything(), list(sesgo)))
d <- est_sq%>%filter_all(all_vars(!is.infinite(.))) %>% summarise(across(everything(), list(sesgo)))
e <- est_1_sq%>%filter_all(all_vars(!is.infinite(.))) %>% summarise(across(everything(), list(sesgo)))

transformaciones <- rbind(a,b,c,d,e)
rm(a,b,c,d,e)
rm(AF1, AF2, AF3)
rm(charts_original)
rm(features_original)
columna_index <- c('sin transformar', 'ln', 'log10','raiz','inversa raiz')
transformaciones <- cbind(columna_index, transformaciones)
transformaciones <- transformaciones %>% column_to_rownames('columna_index')
rm('columna_index')


# Tranformaciones:
# al ver todas las transformaciones posibles y los sesgos asociados (y formas de histogramas), 
# se puede ver que las mejores transformaciones son:

# acousticness: log (queda medio raro!!!) quizás mejor no tocar
# durations: log (OK)
# instrumentalness: log (aunque queda medio raro)
# liveness: log
# speechiness: log

# loudness: sin transf (probe raiz pero tira demasiados datos)

# danceability: sin transformar
# energy: sin transformar
# tempo: sin transformar 
# time signature: sin transformar
# valence: sin transformar

# ahora vamos a hacer los gráficos para explicar cada una de las transformaciones

theme <- theme(text = element_text(size=10),plot.title = element_text(size=12, face="bold.italic",
                                                                      hjust = 0.5), axis.title.x = element_text(size=10, face="bold", colour='black'),
               axis.title.y = element_text(size=10, face="bold"),panel.border = element_blank(),
               panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title = element_text(face="bold"))


acous_graph <- ggplot(data=pre_st, aes(x = acousticness)) +
        geom_histogram(color='black', fill='blue')+theme+
        labs(title= 'Accousticness', x= 'Accousticness value', y='Count')+
        ggplot2::ggplot2::annotate(geom='text', x=0.26, y=10000, vjust=0.5, hjust=0, label="SESGO = 1,39", size=3, col='blue')

acous_graphlog <- ggplot(data=est_log, aes(x = acousticness)) +
        geom_histogram(color='black', fill='red')+theme+
        labs(title= 'ln Accousticness', x= 'ln Accousticness value', y='Count')+
        ggplot2::ggplot2::annotate(geom='text', x=-15, y=7500, vjust=0.5, hjust=0, label="SESGO = -0,61", size=3, col='red')

dance_graph <- ggplot(data=pre_st, aes(x = danceability)) +
        geom_histogram(color='black', fill='gray')+theme+
        labs(title= 'Danceability', x= 'Danceability value', y='Count')+
        ggplot2::ggplot2::annotate(geom='text', x=0, y=6000, vjust=0.5, hjust=0, label="SESGO = -0,42", size=3, col='darkgray')

energy_graph <-ggplot(data=pre_st, aes(x = energy)) +
        geom_histogram(color='black', fill='darkgreen')+theme+
        labs(title= 'Energy', x= 'Energy value', y='Count')+
        ggplot2::ggplot2::annotate(geom='text', x=0.26, y=6000, vjust=0.5, hjust=0, label="SESGO = -0,53", size=3, col='darkgreen')

duration_graph <- ggplot(data=pre_st, aes(x = duration_ms)) +
        geom_histogram(color='black', fill='blue')+theme+
        labs(title= 'Duration', x= 'Duration value', y='Count')+
        ggplot2::ggplot2::annotate(geom='text', x=1000000, y=20000, vjust=0.5, hjust=0, label="SESGO = 0,33", size=3, col='blue')

logduration_graph <- ggplot(data=est_log, aes(x = duration_ms)) +
        geom_histogram(color='black', fill='red')+theme+
        labs(title= 'ln Duration', x= 'ln Duration value', y='Count')+
        ggplot2::ggplot2::annotate(geom='text', x=0, y=40000, vjust=0.5, hjust=0, label="SESGO = 0,06", size=3, col='red')

instrumental_graph <- ggplot(data=pre_st, aes(x = instrumentalness)) +
        geom_histogram(color='black', fill='blue')+theme+
        labs(title= 'instrumentalness', x= 'instrumentalness value', y='Count')+
        ggplot2::ggplot2::annotate(geom='text', x=0.26, y=25000, vjust=0.5, hjust=0, label="SESGO = 1,04", size=3, col='blue')

loginstrumental_graph <-  ggplot(data=est_log, aes(x = instrumentalness)) +
        geom_histogram(color='black', fill='red')+theme+
        labs(title= 'ln instrumentalness', x= 'ln instrumentalness value', y='Count')+
        ggplot2::ggplot2::annotate(geom='text', x=-15, y=3000, vjust=0.5, hjust=0, label="SESGO = 0,21", size=3, col='red')

live_graph <- ggplot(data=pre_st, aes(x = liveness)) +
        geom_histogram(color='black', fill='blue')+theme+
        labs(title= 'liveness', x= 'liveness value', y='Count')+
        ggplot2::ggplot2::annotate(geom='text', x=0.51, y=10000, vjust=0.5, hjust=0, label="SESGO = 1,26", size=3, col='blue')
loglive_graph <- ggplot(data=est_log, aes(x = liveness)) +
        geom_histogram(color='black', fill='red')+theme+
        labs(title= 'ln liveness', x= 'ln liveness value', y='Count')+
        ggplot2::annotate(geom='text', x=-3.5, y=15000, vjust=0.5, hjust=0, label="SESGO = 0,70", size=3, col='red')

speech_graph <- ggplot(data=pre_st, aes(x = speechiness)) +
        geom_histogram(color='black', fill='blue')+theme+
        labs(title= 'speechiness', x= 'speechiness value', y='Count')+
        ggplot2::annotate(geom='text', x=0.26, y=20000, vjust=0.5, hjust=0, label="SESGO = 1,29", size=3, col='blue')
logspeech_graph <- ggplot(data=est_log, aes(x = speechiness)) +
        geom_histogram(color='black', fill='red')+theme+
        labs(title= 'ln speechiness', x= 'ln speechiness value', y='Count')+
        ggplot2::annotate(geom='text', x=-4, y=15000, vjust=0.5, hjust=0, label="SESGO = 0,88", size=3, col='red')

loud_graph <- ggplot(data=pre_st, aes(x = loudness)) +
        geom_histogram(color='black', fill='gray')+theme+
        labs(title= 'loudness', x= 'loudness value', y='Count')+
        ggplot2::annotate(geom='text', x=-30, y=25000, vjust=0.5, hjust=0, label="SESGO = -0,79", size=3, col='darkgray')

tempo_graph <- ggplot(data=pre_st, aes(x = tempo)) +
        geom_histogram(color='black', fill='darkgreen')+theme+
        labs(title= 'tempo', x= 'tempo value', y='Count')+
        ggplot2::annotate(geom='text', x=100, y=15000, vjust=0.5, hjust=0, label="SESGO = -0,23", size=3, col='darkgreen')

timesig_graph <- ggplot(data=pre_st, aes(x = time_signature)) +
        geom_histogram(color='black', fill='gray')+theme+
        labs(title= 'time_signature', x= 'time_signature value', y='Count')+
        ggplot2::annotate(geom='text', x=1, y=80000, vjust=0.5, hjust=0, label="SESGO = -0,51", size=3, col='darkgray')

valence_graph <- ggplot(data=pre_st, aes(x = valence)) +
        geom_histogram(color='black', fill='darkgreen')+theme+
        labs(title= 'valence', x= 'valence value', y='Count')+
        ggplot2::annotate(geom='text', x=0.26, y=4500, vjust=0.5, hjust=0, label="SESGO = 0,05", size=3, col='darkgreen')


g1 <- plot_grid(acous_graph,
          acous_graphlog ,
          duration_graph ,
          logduration_graph ,ncol = 2)
          
          
g2 <- plot_grid(instrumental_graph ,
          loginstrumental_graph ,
          live_graph, 
          loglive_graph ,ncol = 2)
          
g3 <- plot_grid(speech_graph ,
          logspeech_graph ,
          dance_graph ,
          energy_graph ,ncol = 2)
g4 <- plot_grid(loud_graph ,
          tempo_graph ,
          timesig_graph ,
          valence_graph ,ncol = 2)

plot_grid(g1,
          g2 ,
          g3 ,
          g4,ncol = 2)

rm(est_1_sq, est_log, est_log10, est_sq, pre_st)
rm(g1, g2, g3, g4, transformaciones)


##########################################################################################
##########################################################################################
########################################################################################## 
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

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

##########################################################################################
##########################################################################################

# Exito de un tema en el tiempo (sacando datos de 2021): 

# calculo Permanencia max alcanzada + Streams promedio + 
# Posición media en el tiempo de todos sus temas + posición minima

df_streams<- charts %>% 
        group_by(id) %>%
        mutate(totalstreams=sum(Streams))%>%
        summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 

perm_pos <-  df_perm %>%
        group_by(id) %>%
        mutate(max_perm=max(Consec_weeks)) %>%
        mutate(mean_pos=mean(Position)) %>%
        mutate(min_pos=min(Position))%>%
        summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 

exito <- inner_join(df_streams, perm_pos, by=c('id'))%>%select(c(1,3,4,8,16,17,18))

exito$totalstreams <- round(scales::rescale(exito$totalstreams,to = c(1, 100)),5)
exito$max_perm <- round(scales::rescale(exito$max_perm, to = c(1, 100)),5)
exito$min_pos <- round(scales::rescale(exito$min_pos,to = c(1, 100)),5)
exito$mean_pos <- round(scales::rescale(exito$mean_pos,to = c(1, 100)),5)

exito <- exito %>% mutate(indice_exito=((0.25*max_perm)*(0.5*mean_pos)*(0.25*totalstreams)/(2*min_pos)))

ggplot(data=exito, aes(x=indice_exito)) + geom_histogram(bins=40)
ggplot(data=exito, aes(x=log(indice_exito))) + geom_histogram(bins=40)
ggplot(data=exito, aes(x=1/sqrt(indice_exito))) + geom_histogram(bins=40)

k <- log(exito$indice_exito)
j <- 1/sqrt(exito$indice_exito)

print(sesgo(exito$indice_exito)) #0.44
print(sesgo(k))#0.74
print(sesgo(j))#0.90

# INDICE DE EXITO DE TEMA -> ESTO SE PODRÍA MIRAR CON AUDIOFEATURES Y CON LYRICS DE ESE TEMA

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

audiofeatures <- audiofeatures %>% select (-c(1,15:195))
audiofeatures <- audiofeatures %>% select (-c(5,6,9,10,11,12))

exito <- exito %>% select(-1)
colnames(exito) <- c('track_name', 'artist_name', 'total_streams', 'max_perm', 'mean_pos', 'min_pos','indice_exito')

df_feats_ag = merge(x=audiofeatures, y=exito,  
                    by.x = c("artist_name","track_name"),  
                    by.y = c("artist_name","track_name"))

rm(charts, audiofeatures, df_perm, df_permanencia, df_streams, exito, max_perm, perm_pos)
rm(j,k)

# transformación de liveness, indice exito y acousticness tienen que ir con log

df_feats_ag$liveness <- log(df_feats_ag$liveness)
df_feats_ag$indice_exito <- log(df_feats_ag$indice_exito)
df_feats_ag$acousticness <- log(df_feats_ag$acousticness)

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

# ARMO CORPUS DE LYRICS

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

conx_lyrics_en = mongo(collection = "lyrics_english", db = "spotify")
df_lyrics = conx_lyrics_en$find('{}')
df_lyrics <- df_lyrics%>%distinct() #536!!!
df_lyrics <-df_lyrics %>% slice(-375) #sacar duplicado
#write.csv(df_lyrics,"english_lyrics_alva.csv", row.names = FALSE)

# eliminamos las conexión dado que no la vamos a usar
rm(conx_lyrics_spa)
rm(conx_lyrics_en)


df2corpus.pro <-function(data, pro.genius=TRUE, pro.symbols=TRUE, 
                         pro.stopwords=TRUE, idioma_stopwords = "english",
                         pro.min=TRUE, pro.num=TRUE, pro.accents=TRUE,
                         pro.spaces=TRUE, pro.stemm=TRUE) {
        
        library(tm)
        library(stringi)
        library(stringr)
        library(SnowballC) # para Stemming
        
        corpus = Corpus(VectorSource(enc2utf8(data)))
        
        if(pro.genius){
                # Elimino todo lo que aparece antes del primer []
                corpus.pro <- tm_map(corpus, content_transformer(
                        function(x) sub('^.+?\\[.*?\\]',"", x)))
                
                # Elimino las aclaraciones en las canciones, por ejemplo:
                # [Verso 1: Luis Fonsi & Daddy Yankee]
                corpus.pro <- tm_map(corpus.pro, content_transformer(
                        function(x) gsub('\\[.*?\\]', '', x)))
                
                # Elimino todo lo que aparece luego de 'More on Genius'
                corpus.pro <- tm_map(corpus.pro, content_transformer(function(x) gsub("More on Genius.*","", x)))
        }
        
        if(pro.min){
                # Convertimos el texto a minúsculas
                corpus.pro <- tm_map(corpus.pro, content_transformer(tolower))
        }
        
        if(pro.stopwords){
                # Removemos palabras vacias en español
                corpus.pro <- tm_map(corpus.pro, removeWords, stopwords(idioma_stopwords))
        }
        
        if(pro.num){
                # removemos números
                corpus.pro <- tm_map(corpus.pro, removeNumbers)
        }
        
        if(pro.symbols){
                # Removemos puntuaciones
                corpus.pro <- tm_map(corpus.pro, removePunctuation)
                
                # Removemos todo lo que no es alfanumérico
                corpus.pro <- tm_map(corpus.pro, content_transformer(function(x) str_replace_all(x, "[[:punct:]]", " ")))
                
                # Removemos puntuaciones
                corpus.pro <- tm_map(corpus.pro, removePunctuation)
        }
        
        if(pro.accents){
                replaceAcentos <- function(x) {stri_trans_general(x, "Latin-ASCII")}
                corpus.pro <- tm_map(corpus.pro, replaceAcentos)
        }
        
        if(pro.spaces){
                # Se eliminan los espacios adicionales
                corpus.pro <- tm_map(corpus.pro, stripWhitespace)
        }
        
        if(pro.stemm){
                # Se eliminan los espacios adicionales
                corpus.pro <- tm_map(corpus.pro, stemDocument, language="english")
        }
        
        return(corpus.pro)
}

#####################################################
###########Pre-procesamiento del corpus #############
#####################################################

# Corremos la función (operaciones del LAB08)
corpus.pro = df2corpus.pro(df_lyrics$lyrics, pro.stemm = FALSE)

# Recuperamos la letra de la primera canción
inspect(corpus.pro[1])
df_lyrics[1,]

####################################################################
####### Generación de la Matríz Término-Documento del corpus #######
####################################################################

corpus.pro2tdm <- function(corpus, ponderacion, n_terms) {
        
        # Genero la matriz TD y la transformo en una matriz
        dtm <- TermDocumentMatrix(corpus.pro, control = list(weighting = ponderacion))
        matriz_td <- as.matrix(dtm)
        
        # Me quedo con los n_terms más frecuentes
        terminos_frecuentes = head(sort(rowSums(matriz_td), decreasing = T), n_terms)
        
        # Me quedo con la matriz transpuesta de los n_terms más frecuentes
        # Cada fila es un tema, cada columna un término
        matriz_mf = t(matriz_td[sort(names(terminos_frecuentes)),])
        
        # Paso a binaria la matriz (está o no está el término)
        matriz_mf[matriz_mf > 0] <- 1
        
        return(matriz_mf)
}

matriz <- corpus.pro2tdm(corpus.pro, "weightTfIdf", 150000)
dim(matriz)

df_tm=as.data.frame(matriz)

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
###########################  PARENTESIS BAG OF WORDS  ####################################
###########################  PARENTESIS BAG OF WORDS  ####################################
###########################  PARENTESIS BAG OF WORDS  ####################################
###########################  PARENTESIS BAG OF WORDS  ####################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

cortes_indice <- c(-4.1576239,  0.2523463,  3.8444017,  7.4663421)
df_prueba <- df_feats_ag
df_prueba$indice_exito = cut(df_prueba$indice_exito,breaks = cortes_indice,labels = c("baja","media", "alta")) 
df_prueba <- df_prueba %>% filter(indice_exito=='alta') %>% select(c(1,2)) # ARTISTAS Y TEMAS CON ÍNDICE DE ÉXITO ALTO!

df_lyrics2 <- merge(df_lyrics, df_prueba, by.x=c('artist_name','track_name'), by.y=c('artist_name','track_name'))


corp_2 <- df2corpus.pro(df_lyrics2$lyrics, pro.stemm = FALSE)
matriz2 <- corpus.pro2tdm(corp_2, "weightTfIdf", 150000)
dim(matriz2)
matriz2 <- t(matriz2 )


# Calculamos la frecuencia de cada término en el corpus
freq_term <- sort(rowSums(matriz2),decreasing=TRUE)

# Generamos un dataframe con esta sumatoria (de rows)
df_freq <- data.frame(termino = names(freq_term), frecuencia=freq_term)

# Reseteamos el índice (sino era el término "dueño" de la frecuencia)
row.names(df_freq) <- NULL

# Dataframe con frecuencia de términos
df_freq

# Gráfico de barras con los N más frecuentes
N=15
barplot(df_freq[1:N,]$frecuencia, las = 2, names.arg = df_freq[1:N,]$termino,
        col ="lightblue", main ="Palabras más frecuentes",
        ylab = "Frecuencia de palabras", ylim = c(0, max(df_freq$frecuencia)+300))

#######################################################
###################### WORDCLOUD ######################
#######################################################

topK = head(df_freq, 200)

# Visualización de los resultados
# Nube de Etiquetas
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(ggwordcloud)
library(htmlwidgets)

par(bg="white") # Fijamos el fondo en color gris

set.seed(1234)
wordcloud(words = topK$termino, freq = topK$frecuencia, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(4, "Dark2"))



Bcloud <-wordcloud2(topK,size = 1,figPath = "/Users/FR/Desktop/music.png",color = 'black')
Bcloud



ggwordcloud(words= topK$termino, freq = topK$frecuencia, scale = c(2, 0.5), min.freq = 3,
            max.words = 200, random.order = TRUE, random.color = FALSE,
            rot.per = 0.1, colors = "black", ordered.colors = FALSE)

hw <- wordcloud2(demoFreq,size = 3)
saveWidget(hw,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =10)

logo <- image_read("/Users/FR/Desktop/music.png")

ggplot(topK,aes(label = termino, size = frecuencia,color = frecuencia)) +
        geom_text_wordcloud_area(aes(angle = 45 * sample(-2:2, nrow(topK), replace = TRUE, prob = c(1, 1, 4, 1, 1))), mask = logo,rm_outside = TRUE) +
        scale_size_area(max_size = 40) + theme_minimal() + scale_color_gradient(low = "darkred", high = "red")


winchi <- wordcloud2(topK, figPath = "/Users/FR/Desktop/music.png", size = 1.5,color = "skyblue", backgroundColor="white")
winchi


##########################################################################################
##########################################################################################
##########################################################################################
###########################  PARENTESIS BAG OF WORDS  ####################################
###########################  PARENTESIS BAG OF WORDS  ####################################
###########################  PARENTESIS BAG OF WORDS  ####################################
###########################  PARENTESIS BAG OF WORDS  ####################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################


##############################################################################################################



##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
###########################  PARENTESIS CORPUS CON MENOS PALABRAS ########################
###########################  PARENTESIS CORPUS CON MENOS PALABRAS ########################
###########################  PARENTESIS CORPUS CON MENOS PALABRAS ########################
###########################  PARENTESIS CORPUS CON MENOS PALABRAS ########################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
dim(matriz)
matriz_prueba <- t(matriz)

# Calculamos la frecuencia de cada término en el corpus
freq_term_p <- sort(rowSums(matriz_prueba),decreasing=TRUE)
# Generamos un dataframe con esta sumatoria (de rows)
df_freq_p <- data.frame(termino = names(freq_term_p), frecuencia=freq_term_p)
# Reseteamos el índice (sino era el término "dueño" de la frecuencia)
row.names(df_freq_p) <- NULL
#nro términos
nro_term <- length(rownames(df_freq_p)) #11881
# Gráfico de barras con los N más frecuentes
N=8000
barplot(df_freq_p[1:N,]$frecuencia, las = 2, main ="Palabras más frecuentes",
        ylab = "Frecuencia", ylim = c(0, max(df_freq_p$frecuencia)+100),
        xlab='',font.lab=2, cex.axis = 0.8)
par(new=T)
barplot(df_freq_p1[1:N,]$frecuencia, ylim = c(0, max(df_freq_p$frecuencia)+100), border='red', axes=F)
text(1004, 400,"1% de términos\nmás frecuentes", col='red')
title(xlab="Términos", line=1, cex.lab=1, font.lab=2)

# Ahora voy a sacar el 1% de términos más frecuentes
masfreq <- as.integer(round(0.01*length(rownames(df_freq_p)),0)) 
head(df_freq_p)
#calculo de terminos más abundantes
df_freq_p1 <- df_freq_p %>% slice (c(1:masfreq))
# 1% de palabras más abundantes
terminos_no <- df_freq_p1$termino 
df_freq1_p2 <- df_freq_p %>% slice (c((masfreq+1):nro_term))
terminos_si <- df_freq1_p2$termino 

filtro2 <- colnames(df_tm)%in%terminos_si

df_l_f_ok=df_tm[, filtro2]


df_LFsin1 = merge(x = cbind(df_lyrics[-c(3)], df_l_f_ok), 
                           y = df_feats_ag, 
                           by.x = c("artist_name","track_name"), 
                           by.y = c("artist_name","track_name"))
# Quitar atributos txt con valor 0
filter2 = !names(df_LFsin1) %in% c("artist_name", "track_name")
df_LFsin1_ok = df_LFsin1[, filter2]
df_LFsin1_ok = df_LFsin1_ok[ ,-which(colSums(df_LFsin1_ok)==0)]
# Agregamos un TID
df_LFsin1_ok$tid = 1:nrow(df_LFsin1_ok)
df_LFsin1$tid = 1:nrow(df_LFsin1_ok)

##########################################################################################

# discretizaciones
breaks = 3
# discretización por kmeans
df_LFsin1_ok$danceability_cluster = arules::discretize(df_LFsin1_ok$danceability, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
df_LFsin1_ok$energy_cluster = arules::discretize(df_LFsin1_ok$energy.y, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
df_LFsin1_ok$liveness_cluster = arules::discretize(df_LFsin1_ok$liveness, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
df_LFsin1_ok$valence_cluster = arules::discretize(df_LFsin1_ok$valence, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
df_LFsin1_ok$acousticness_cluster = arules::discretize(df_LFsin1_ok$acousticness, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
df_LFsin1_ok$indice_cluster = arules::discretize(df_LFsin1_ok$indice_exito, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))


names(df_LFsin1_ok)[11121:11131]

features2 = names(df_LFsin1_ok)[c(1:10447,11131:11137)]
names(df_LFsin1_ok)

# Para los terminos necesito filtrar las frecuencias=0
df_single_sin = reshape2::melt(data = df_LFsin1_ok[,features2], id.vars = c("tid") ) 
df_single_sin = df_single_sin[df_single_sin$value!=0,]

df_single_txt = df_single_sin[df_single_sin$value==1,]
df_single_cat = df_single_sin[df_single_sin$value!=1,]

df_single_txt$variable =  paste0("TERM_",df_single_txt$variable)
df_single_cat$variable =  paste0(df_single_cat$variable, "=", as.character(df_single_cat$value))

df_single_sin = rbind(df_single_cat, df_single_txt)
df_single_sin = na.omit(df_single_sin[,-c(3)])
names(df_single_sin ) = c("TID", "item")

write.table(df_single_sin, file = "/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/transacciones-sinuno.txt", row.names = FALSE)


transacciones_LF2='/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/transacciones-sinuno.txt'


trans <- read.transactions(transacciones_LF2,format = "single",header = TRUE,sep = " ",cols = c("TID","item"),quote = '"')
trans
arules::inspect(head(trans, 3))
summary(trans)

reglas2 <- arules::apriori(trans, parameter = list(support=0.1, confidence=0.2,minlen=2, maxlen=5, target = "rules"))
reglas2

arules::inspect(head(sort(reglas2, by="lift", decreasing = T), 30))

dfreglas2 <- DATAFRAME(reglas2, separate = TRUE)
write.csv(dfreglas2, '/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/dfreglas2.csv')
colnames(dfreglas2) <- c("LHS","RHS","soporte", "confianza","coverage","lift","count")  
ggplot(data=dfreglas2,aes(x=soporte, y=lift))+
        geom_point(aes(col=confianza), alpha=0.7, size=1.3)+
        labs(title= "REGLAS DE ASOCIACIÓN",x= "Soporte", y= "LIFT")+theme+scale_color_gradient(low = "grey97", high = "red")+
        ggplot2::annotate(geom='text', x=0.1072857, y=9.259259, vjust=0.5, hjust=0, label=" reglas con \nconsecuente \n    KEEP", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.1277143, y=7.777778, vjust=0.5, hjust=0, label="           reglas con \n         consecuente \n MERRY / STAR / SLEEP\n         CHRISTMAS", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.1072857, y=5.703165, vjust=0.5, hjust=0, label="           reglas con \n         consecuente \n ACOUSTICNESS BAJO", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.1332857, y=4.103165, vjust=0.5, hjust=0, label="     reglas con \n   consecuente \n   EXITO ALTO\n ENERGIA ALTO", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.1332857, y=3.103165, vjust=0.5, hjust=0, label="       reglas con \n    consecuente \nVALENCE MEDIO", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.225, y=2.47, vjust=0.5, hjust=0, label="reglas de 2 itemset que relacionan atributos de audio", size=2, col='black')+
        ggplot2::annotate("rect", xmin=c(0.1,0.115,.099,.099,.099,.199), xmax=c(0.106,.121,.108,.108,.108,.305), ymin=c(8.5,7.1,3.9, 2.47, 2.31, 1.03) , ymax=c(9.4,7.9,4.2, 2.7, 2.05, 2.3), alpha=0, color="gray30",fill="violet",linetype=3)+
        geom_segment(aes(x = .121, y = 7.9, xend =0.136, yend = 8.1), col='gray30', size=0.01)+
        geom_segment(aes(x = .108, y = 4.2, xend =0.123, yend = 5.3), col='gray30', size=0.01)+
        geom_segment(aes(x = .108, y = 2.7, xend =0.1332857, yend = 4.103165), col='gray30', size=0.01)+
        geom_segment(aes(x = .108, y = 2.31, xend =0.1332857, yend = 3.103165), col='gray30', size=0.01)



rules_exito_sin <- arules::subset(reglas2, subset =  (rhs %pin% "indice_cluster"))
rules_exito_sin 
arules::inspect(head(sort(rules_exito_sin, by="lift", decreasing = TRUE), 85))


plot(rules_exito_sin, measure = c("support", "lift"), shading = "confidence")
plot(rules_exito_sin, method = "two-key plot")

df_exitosin <- DATAFRAME(rules_exito_sin, separate = TRUE)
df_exitosin$LHS <- as.character(df_exitosin$LHS)
for (i in 1:152){df_exitosin$cat[i] <- sub('.*=', '', df_exitosin$RHS)[[i]]}
for (i in 1:152){df_exitosin$cat[i] <- sub('}', '', df_exitosin$cat)[[i]]}
df_exitosin$cat <- factor(df_exitosin$cat,  levels=c('bajo','medio','alto'))
colnames(df_exitosin) <- c("LHS","RHS", "soporte","confianza", "covertura","lift","count","categoria")


analisis <- df_exitosin%>%filter(lift>1.25)


write.csv(df_exitosin, '/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/df_exitosin.csv')



ggplot(data=df_exitosin,aes(x=soporte, y=confianza))+
        geom_point(aes(color=categoria), size=4, alpha=0.5,show.legend = T)+geom_point(aes(fill=lift), shape=21, size=2.5)+
        labs(title= "REGLAS DE ASOCIACIÓN\nÍndice de éxito (consecuente)",
             x= "Soporte", y= "Confianza")+theme+scale_fill_gradient(low = "blue", high = "red")+
        scale_color_manual(values = c("blue", "violet", "orange"))+
        ggplot2::annotate("rect", xmin=c(0.11), xmax=c(0.21), ymin=c(1.05) , ymax=c(0.37), alpha=0.1, color="black",fill="black",linetype=2)+
        ggplot2::annotate(geom='text', x=0.1253714, y=0.9997406, vjust=0.5, hjust=0, label="4", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.1388571, y=0.4895652, vjust=0.5, hjust=0, label="5", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.1434286, y=0.5364609, vjust=0.5, hjust=0, label="3", size=2.5, col='black')+
        #ggplot2::annotate(geom='text', x=0.1582857, y=0.5481220, vjust=0.5, hjust=0, label="7", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.1400000, y=0.7322093, vjust=0.5, hjust=0, label="6", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.1177143, y=1.0250000, vjust=0.5, hjust=0, label="2", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.1145714, y=0.5184287, vjust=0.5, hjust=0, label="1", size=2.5, col='black')


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
###########################  PARENTESIS CORPUS CON MENOS PALABRAS ########################
###########################  PARENTESIS CORPUS CON MENOS PALABRAS ########################
###########################  PARENTESIS CORPUS CON MENOS PALABRAS ########################
###########################  PARENTESIS CORPUS CON MENOS PALABRAS ########################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

df_lyrics_features = merge(x = cbind(df_lyrics[-c(3)], df_tm), 
                   y = df_feats_ag, 
                   by.x = c("artist_name","track_name"), 
                   by.y = c("artist_name","track_name"))


# Quitar atributos txt con valor 0
filter = !names(df_lyrics_features) %in% c("artist_name", "track_name")
df_lyrics_features_ok = df_lyrics_features[, filter]
df_lyrics_features_ok = df_lyrics_features_ok[ ,-which(colSums(df_lyrics_features_ok)==0)]

# Agregamos un TID
df_lyrics_features_ok$tid = 1:nrow(df_lyrics_features_ok)
df_lyrics_features$tid = 1:nrow(df_lyrics_features_ok)


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

# discretizaciones
breaks = 3
# discretización por kmeans
df_lyrics_features_ok$danceability_cluster = arules::discretize(df_lyrics_features_ok$danceability, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
#Discretización: Puntos de corte 
cluster_cuts = arules::discretize(df_lyrics_features_ok$danceability, method = "cluster", breaks = breaks, onlycuts = TRUE)
#histograma
hist(df_lyrics_features_ok$danceability, main="Histograma Danceability", xlab="Danceability", ylab="Frecuencia", breaks=50)
abline(v=cluster_cuts, col="red",lty=5)

##########################################################################################

df_lyrics_features_ok$energy_cluster = arules::discretize(df_lyrics_features_ok$energy.y, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
#Discretización: Puntos de corte 
cluster_cuts = arules::discretize(df_lyrics_features_ok$energy.y, method = "cluster", breaks = breaks, onlycuts = TRUE)
#histograma
hist(df_lyrics_features_ok$energy.y, main="Histograma Energy", xlab="Energy", ylab="Frecuencia", breaks=50)
abline(v=cluster_cuts, col="red",lty=5)

##########################################################################################

df_lyrics_features_ok$liveness_cluster = arules::discretize(df_lyrics_features_ok$liveness, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
#Discretización: Puntos de corte 
cluster_cuts = arules::discretize(df_lyrics_features_ok$liveness, method = "cluster", breaks = breaks, onlycuts = TRUE)
#histograma
hist(df_lyrics_features_ok$liveness, main="Histograma Energy", xlab="Energy", ylab="Frecuencia", breaks=50)
abline(v=cluster_cuts, col="red",lty=5)

##########################################################################################

df_lyrics_features_ok$valence_cluster = arules::discretize(df_lyrics_features_ok$valence, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
#Discretización: Puntos de corte 
cluster_cuts = arules::discretize(df_lyrics_features_ok$valence, method = "cluster", breaks = breaks, onlycuts = TRUE)
#histograma
hist(df_lyrics_features_ok$valence, main="Histograma Energy", xlab="Energy", ylab="Frecuencia", breaks=50)
abline(v=cluster_cuts, col="red",lty=5)

##########################################################################################

df_lyrics_features_ok$acousticness_cluster = arules::discretize(df_lyrics_features_ok$acousticness, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
#Discretización: Puntos de corte 
cluster_cuts = arules::discretize(df_lyrics_features_ok$acousticness, method = "cluster", breaks = breaks, onlycuts = TRUE)
#histograma
hist(df_lyrics_features_ok$acousticness, main="Histograma Energy", xlab="Energy", ylab="Frecuencia", breaks=50)
abline(v=cluster_cuts, col="red",lty=5)

##########################################################################################

df_lyrics_features_ok$indice_cluster = arules::discretize(df_lyrics_features_ok$indice_exito, method = "cluster", breaks = breaks, labels=c("bajo", "medio", "alto"))
#Discretización: Puntos de corte 
cluster_cuts = arules::discretize(df_lyrics_features_ok$indice_exito, method = "cluster", breaks = breaks, onlycuts = TRUE)
#histograma
hist(df_lyrics_features_ok$indice_exito, main="Histograma Energy", xlab="Energy", ylab="Frecuencia", breaks=50)
abline(v=cluster_cuts, col="red",lty=5)

##########################################################################################
##########################################################################################

table(df_lyrics_features_ok$danceability_cluster)
table(df_lyrics_features_ok$energy_cluster)
table(df_lyrics_features_ok$liveness_cluster)
table(df_lyrics_features_ok$valence_cluster)
table(df_lyrics_features_ok$acousticness_cluster)
table(df_lyrics_features_ok$indice_cluster)

##########################################################################################
##########################################################################################
##########################################################################################

names(df_lyrics_features_ok)[11240:11250]

features = names(df_lyrics_features_ok)[c(1:10566,11250:11256)]
names(df_lyrics_features_ok)

# Para los terminos necesito filtrar las frecuencias=0
df_single = reshape2::melt(data = df_lyrics_features_ok[,features], id.vars = c("tid") ) 
df_single = df_single[df_single$value!=0,]

df_single_txt = df_single[df_single$value==1,]
df_single_cat = df_single[df_single$value!=1,]

df_single_txt$variable =  paste0("TERM_",df_single_txt$variable)
df_single_cat$variable =  paste0(df_single_cat$variable, "=", as.character(df_single_cat$value))

df_single = rbind(df_single_cat, df_single_txt)
df_single = na.omit(df_single[,-c(3)])
names(df_single ) = c("TID", "item")

write.table(df_single, file = "/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/transacciones-lyrics-features.txt", row.names = FALSE)
# Fin del preprocesamiento

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

transacciones_LF='/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/transacciones-lyrics-features.txt'


lyrics_features_trans <- read.transactions(transacciones_LF,format = "single",header = TRUE,sep = " ",cols = c("TID","item"),quote = '"')
lyrics_features_trans
arules::inspect(head(lyrics_features_trans, 3))
summary(lyrics_features_trans)

reglas <- arules::apriori(lyrics_features_trans, parameter = list(support=0.1, confidence=0.5,minlen=2, target = "rules"))
reglas

arules::inspect(head(sort(reglas, by="lift", decreasing = T), 30))

###############################################################
###############################################################
###############################################################
dfreglas <- DATAFRAME(reglas, separate = TRUE)

write.csv(dfreglas, '/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/dfreglas.csv')
library(ggplot2)
colnames(dfreglas) <- c("LHS","RHS","soporte", "confianza","coverage","lift","count")  
ggplot(data=dfreglas,aes(x=soporte, y=lift))+
        geom_point(aes(col=confianza), alpha=0.7, size=1.3)+
        labs(title= "REGLAS DE ASOCIACIÓN",x= "Soporte", y= "LIFT")+theme+scale_color_gradient(low = "grey97", high = "red")+
        ggplot2::annotate(geom='text', x=0.112, y=4, vjust=0.5, hjust=0, label="{acousticness_alto,energy_bajo,valence_bajo} -> {danceability_bajo}", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.116, y=3.5, vjust=0.5, hjust=0, label="{energy_bajo,valence_bajo} -> {danceability_bajo}", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.112, y=3.4, vjust=0.5, hjust=0, label="{acousticness_alto,danceability_bajo,valence_bajo} -> {energy_bajo}", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.14, y=3.3, vjust=0.5, hjust=0, label="{acousticness_alto,valence_bajo} -> {danceability_bajo}", size=2, col='black')+
        
        ggplot2::annotate(geom='text', x=0.115, y=2.9, vjust=0.5, hjust=0, label="{acousticness_alto,danceability_bajo,energy_bajo} -> {valence_bajo}", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.125, y=2.8, vjust=0.5, hjust=0, label="{danceability_bajo,energy_bajo} -> {valence_bajo}", size=2, col='black')+
        geom_segment(aes(x = 0.115, y = 2.9, xend =0.102, yend = 2.79), col='black', size=0.01)+
        geom_segment(aes(x = 0.122, y = 2.8, xend =0.108, yend = 2.79), col='black', size=0.01)+
        ggplot2::annotate(geom='text', x=0.15, y=2.6, vjust=0.5, hjust=0, label="{indice_alto,TERM_now,TERM_way} -> {TERM_wanna} ", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.155, y=2.5, vjust=0.5, hjust=0, label="{indice_alto,TERM_just,TERM_like,TERM_love} -> {energy_alto}", size=2, col='black')+
        geom_segment(aes(x = 0.15, y = 2.6, xend =0.103, yend = 2.25), col='black', size=0.01)+
        geom_segment(aes(x = 0.155, y = 2.5, xend =0.108, yend = 2.24), col='black', size=0.01)+
        
        ggplot2::annotate(geom='text', x=.18, y=2.4, vjust=0.5, hjust=0, label="{TERM_aint,TERM_like,TERM_wanna,TERM_yeah} -> {TERM_got}", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.185, y=2.3, vjust=0.5, hjust=0, label="{TERM_just,TERM_love,TERM_put} -> {TERM_get}", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.19, y=2.2, vjust=0.5, hjust=0, label="{TERM_aint,TERM_like,TERM_wanna} -> {TERM_got}", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.195, y=2.1, vjust=0.5, hjust=0, label="{TERM_aint,TERM_now,TERM_yeah} -> {TERM_got}", size=2, col='black')+
        geom_segment(aes(x = 0.18, y = 2.4, xend =0.1025858, yend = 2.051304), col='black', size=0.01)+
        geom_segment(aes(x = 0.185, y = 2.3, xend =0.1059771, yend = 2.045527), col='black', size=0.01)+
        geom_segment(aes(x = 0.19, y = 2.2, xend =0.1110640, yend = 2.035764), col='black', size=0.01)+
        geom_segment(aes(x = 0.195, y = 2.1, xend =0.1292921, yend = 2.025035), col='black', size=0.01)+
        
        ggplot2::annotate(geom='text', x=0.19, y=1.9, vjust=0.5, hjust=0, label="{TERM_aint,TERM_got,TERM_like,TERM_never} -> {TERM_yeah}", size=2, col='black')+
        ggplot2::annotate(geom='text', x=0.22, y=1.8, vjust=0.5, hjust=0, label="{danceability_alto,TERM_just,TERM_make} -> {TERM_like}", size=2, col='black')+
        geom_segment(aes(x = 0.19, y = 1.9, xend =0.1038576, yend = 1.689369), col='black', size=0.01)+
        geom_segment(aes(x = 0.22, y = 1.8, xend =0.11, yend = 1.689189), col='black', size=0.01)


###############################################################
###############################################################
###############################################################


rules_exito <- arules::subset(reglas, subset =  (rhs %pin% "indice_cluster=alto"))
rules_exito #122 reglas
arules::inspect(head(sort(rules_exito, by="lift", decreasing = TRUE), 121))

dfexito <- DATAFRAME(rules_exito, separate = TRUE)
dfexito$LHS <- as.character(dfexito$LHS)
for (i in 1:122){dfexito$order[i] <- length(strsplit(dfexito$LHS, ",")[[i]])+1}
dfexito$order <- factor(dfexito$order)
colnames(dfexito) <- c("LHS","RHS", "soporte","confianza", "covertura","lift","count","orden")

write.csv(dfexito, '/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/dfexito.csv')


ggplot(data=dfexito,aes(x=soporte, y=confianza))+
        geom_point(aes(color=orden), size=4, alpha=0.5,show.legend = T)+geom_point(aes(fill=lift), shape=21, size=2.5)+
        labs(title= "REGLAS DE ASOCIACIÓN\nÍndice de éxito alto (consecuente)",
             x= "Soporte", y= "Confianza")+theme+scale_fill_gradient(low = "blue", high = "red")+
        scale_color_manual(values = c("blue", "white", "yellow", "green"))+
        ggplot2::annotate(geom='text', x=.1151, y=.75, vjust=0.5, hjust=0, label="1", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.105, y=.75, vjust=0.5, hjust=0, label="2", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.0976, y=.68, vjust=0.5, hjust=0, label="3", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.227, y=.53, vjust=0.5, hjust=0, label="4", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.1610, y=.54, vjust=0.5, hjust=0, label="5", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.0997, y=.657, vjust=0.5, hjust=0, label="6", size=2.5, col='black')+
        ggplot2::annotate(geom='text', x=0.098, y=.687, vjust=0.5, hjust=0, label="7", size=2.5, col='black')
        
        #geom_label(label="1", x=0.13, y=0.75, size=2,color = "black")+
        #geom_label(label="2", x=0.13, y=0.73, size=2,color = "black")

###############################################################
###############################################################
###############################################################

rules_energy <- arules::subset(reglas, subset =  (rhs %pin% "energy_cluster=alto"))
rules_energy #203
arules::inspect(head(sort(rules_energy, by="lift", decreasing = TRUE), 203))

dfenergy<- DATAFRAME(rules_energy, separate = TRUE)
dfenergy$LHS <- as.character(dfenergy$LHS)
for (i in 1:203){dfenergy$order[i] <- length(strsplit(dfenergy$LHS, ",")[[i]])+1}
dfenergy$order <- factor(dfenergy$order)
colnames(dfenergy) <- c("LHS","RHS", "soporte","confianza", "covertura","lift","count","orden")
write.csv(dfexito, '/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/dfenergy.csv')


graf_energy <-ggplot(data=dfenergy,aes(x=soporte, y=confianza))+
        geom_point(aes(color=orden), size=4, alpha=0.5,show.legend = T)+geom_point(aes(fill=lift), shape=21, size=2.5)+
        labs(title= "REGLAS DE ASOCIACIÓN\nEnergía alta (consecuente)",
             x= "Soporte", y= "Confianza")+theme+scale_fill_gradient(low = "blue", high = "red")+
        scale_color_manual(values = c("blue", "white", "yellow", "green"))


###############################################################
###############################################################
###############################################################

rules_dance <- arules::subset(reglas, subset =  (rhs %pin% "danceability_cluster=alto"))
rules_dance #21
arules::inspect(head(sort(rules_dance, by="lift", decreasing = TRUE), 203))

dfdance<- DATAFRAME(rules_dance, separate = TRUE)
dfdance$LHS <- as.character(dfdance$LHS)
for (i in 1:21){dfdance$order[i] <- length(strsplit(dfdance$LHS, ",")[[i]])+1}
dfdance$order <- factor(dfdance$order)
colnames(dfdance) <- c("LHS","RHS", "soporte","confianza", "covertura","lift","count","orden")
write.csv(dfexito, '/Users/FR/Desktop/MARTES_2021_1C_DataMining/TP2/dfdance.csv')


graf_dance <-ggplot(data=dfdance,aes(x=soporte, y=confianza))+
        geom_point(aes(color=orden), size=4, alpha=0.5,show.legend = T)+geom_point(aes(fill=lift), shape=21, size=2.5)+
        labs(title= "REGLAS DE ASOCIACIÓN\nDanceabilidad alta (consecuente)",
             x= "Soporte", y= "Confianza")+theme+scale_fill_gradient(low = "blue", high = "red")+
        scale_color_manual(values = c("blue", "pink", "yellow"))


###############################################################
###############################################################
###############################################################

rules_valence <- arules::subset(reglas, subset =  (rhs %pin% "valence_cluster=alto"))
rules_valence #2
arules::inspect(head(sort(rules_valence, by="lift", decreasing = TRUE), 203))

###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################

# Es posible redefinir el orden de las métricas
plot(reglas, measure = c("support", "lift"), shading = "confidence")
# El método two-key permite incluir la dimensión de orden (cantidad de items)
plot(reglas2, method = "two-key plot")
# Es posible generar un gráfico interactivo
sel <- plot(reglas, measure = c("support", "lift"), shading = "confidence", interactive = TRUE)
# Muestra las reglas de forma matricial y por índice de los items
plot(subrules2, method = "matrix", shading = "support")
# Me quedo con un subconjunto de las reglas
subrules2 <- head(reglas, n = 100, by = "lift")
# The width of the arrows represents support 
# the intensity of the color represent confidence. 
# For larger rule sets visual analysis becomes difficult since with an increasing number of 
# rules also the number of crossovers between the lines increases Yang (2003)
plot(rules_exito, method = "paracoord")
