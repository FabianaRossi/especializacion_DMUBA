
pp1 <- inner_join(charts,AF1,by='id') %>% ungroup()%>%select(week_start,acousticness)%>%group_by(week_start)%>%summarize(mean=mean(acousticness))
ggplot(data=pp1, aes(x=week_start,y=mean) )+stat_smooth(method="loess", span=0.1, se=TRUE, alpha=0.3)






# cuando tengo un base plot que no puedo asignar a variable


p <- recordPlot()
plot.new() ## clean up device
p # redraw

## grab the scene as a grid object
library(gridGraphics)
library(grid)
grid.echo()
a <- grid.grab()

## draw it, changes optional
grid.newpage()
a <- editGrob(a, vp=viewport(width=unit(5,"in")), gp=gpar(fontsize=10))
j <- grid.draw(a)


########################################### PRUEBA ###############################################################
prueba_AF_artistas <- audiofeatures %>% group_by(artist_name, track_name) %>% count() 
nro_join <- audiofeatures  %>% filter(grepl('feat', track_name)) %>% count()
nro_join2 <- audiofeatures  %>% filter(grepl('remix', track_name)) %>% count()
#solo para tener una idea, saco todo aquello que tenga feat o remix en su trackname
AF2 <- audiofeatures [!grepl("feat", audiofeatures$track_name),]
AF2 <- AF2 [!grepl("remix", AF2$track_name),]
AF2 <- AF2 [!grepl("edit", AF2$track_name),]
AF2 <- AF2 [!grepl("liveat", AF2$track_name),]
AF2 <- AF2 [!grepl("aovivo", AF2$track_name),]
AF2 <- AF2 [!grepl("cover", AF2$track_name),]
AF2 <- AF2 [!grepl("commentary", AF2$track_name),]
AF2 <- AF2 [!grepl(" ", AF2$track_name),]
AF2 <- AF2 [grepl("[^[:blank:]]", AF2$track_name),]
AF2 <- AF2 %>% group_by(artist_name, track_name) %>% summarise(mean=mean(tempo))
AF2 <- AF2 %>% arrange(track_name)
AF3 <- AF2 %>% group_by(track_name) %>% count() # sigue estando en 90.000
# gente que canta en conjunto tiene el track anotado con mismo nombre para diferentes artistas
# anotaciones de lo mismo en diferentes idiomas
# remix, feat, ft, edit, liveat, aovivo, etc, about... commentary

#######################################################################################################################


# (DESCRIPTIVO 2) - audio feature radar chart para los primeros 10 y ultimos 10 (en nro de)
#hago un promedio del valor de streams en chart que tuvo cada canción/semana
chart_streams <- charts %>% group_by(artist_name, track_name) %>% summarize(mean_streams=mean(Streams)) %>% arrange(desc(artist_name))
chart_streams <- chart_streams[!(is.na(chart_streams$artist_name) | chart_streams$artist_name==""), ]
chart_streams <- chart_streams[!(is.na(chart_streams$track_name) | chart_streams$track_name==""), ]
chart_streams$id <- paste(chart_streams$artist_name, chart_streams$track_name)
AF1$id <- paste(AF1$artist_name, AF1$track_name)

# al hacer inner join, me saco de encima todos los artistas que no tienen audiofeatures
#(por más de que sean super exitosos, como olivia rodrigeuz) o que tenga mil AF asociados con 
#artistas multiples

M1 <- inner_join(chart_streams,AF1, by='id')
M1 <- M1 %>% group_by(id)%>%summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 
M1 <- M1 %>% column_to_rownames('id')
temp_M1 <- as.data.frame(round(apply(M1 %>% select(-c(1:6)), 2, scales::rescale), 2))
M2 <- cbind(M1 %>% select(1:3), temp_M1)
M2 <- M2 %>% arrange(desc(mean_streams))
#agarro los top10 y los ultimos10 
top10_streaming <- M2 %>% head(10)
bottom10_streaming <- M2 %>% tail(10)
# hago un row bind de top y bottom steaming y le creo una columna factor llamada CAT
top10_streaming$CAT <- 'top10'
bottom10_streaming$CAT<- 'bottom10'
top10_mean <- top10_streaming%>% group_by(CAT)%>%summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 
bottom10_mean <- bottom10_streaming%>% group_by(CAT)%>%summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) 

df1 <- rbind(top10_mean, bottom10_mean)
rm(top10_mean, bottom10_mean)
df1 <- df1 %>% select(-c(2:4))
df2 <- df1 %>% column_to_rownames('CAT')

#Función grafico 1
create_beautiful_radarchart <- function(data, color = "#00AFBB", vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
        radarchart(data, axistype = 1,pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
                   cglcol = "grey", cglty = 1, cglwd = 0.8,axislabcol = "grey", 
                   vlcex = vlcex, vlabels = vlabels, caxislabels = caxislabels, title = title, ...)}

# Variables summary # Get the minimum and the max of every column and avg
col_max <- apply(temp_M1, 2, max)
col_min <- apply(temp_M1, 2, min)
# Put together the summary of columns
col_summary <- data.frame(t(data.frame(Max = col_max, Min = col_min)))#, #Average = col_mean)))

# Bind variables summary to the data
df3 <- rbind(col_summary, df2)

# Create the radar charts
par(xpd=TRUE, mar=c(1,1,1,1.5))
create_beautiful_radarchart(data = df3, caxislabels = c(0, .25, 0.5, .75, 1),
                            color = c("#00AFBB", "#E7B800"),vlcex=.7)
# Add an horizontal legend
legend(x = -.7, y=-1.25, legend = rownames(df3[-c(1,2),]), horiz = TRUE,
       bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800"),text.col = "black", cex = 1, pt.cex = 1.5)
title(main= "Top & Bottom 10 (Streams)", line=-1)






#Computing the p-value of correlations
#To compute the matrix of p-value, a custom R function is used :

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
        mat <- as.matrix(mat)
        n <- ncol(mat)
        p.mat<- matrix(NA, n, n)
        diag(p.mat) <- 0
        for (i in 1:(n - 1)) {
                for (j in (i + 1):n) {
                        tmp <- cor.test(mat[, i], mat[, j], ...)
                        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
                }
        }
        colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
        p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(df_1)



#, #Text label color and rotation
# Combine with significance
#         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
# hide correlation coefficient on the principal diagonal
#         diag=FALSE)+title(main='Matriz de correlaciones: AUDIO FEATURES')

#########################
# Specialized the insignificant value according to the significant level
#corrplot(M, type="upper", order="hclust", 
#         p.mat = p.mat, sig.level = 0.01)

# Leave blank on no significant coefficient
#corrplot(M, type="upper", order="hclust", 
#         p.mat = p.mat, sig.level = 0.01, insig = "blank")



library(ggplot2)
library(gtable)
library(grid)

grid.newpage()

# two plots
p1 <- ggplot(data=spot, aes(x=Date, y=Close)) + geom_line(colour = "blue") + theme_bw()
p2 <- ggplot(data=streams, aes(x=week_start, y=sum)) + geom_line(colour = "red") + theme_bw() %+replace% 
        theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)



########################################### PRUEBA ###############################################################
########################################### PRUEBA ###############################################################
########################################### PRUEBA ###############################################################


spot <- read.csv('/Users/FR/Desktop/MARTES_2021_1C_DataMining/TPespecial/SPOT.csv')
spot$Date <- as.Date(spot$Date)
spot1 <- spot%>%select(c(1,5))%>%mutate(date2=Date+4)
spot1$date2 <- as.Date(spot1$date2)


charts_original <- mongo(collection='charts', db='spotify')$find() 
charts <- charts_original %>% select (-'URL') %>% unique() 
charts <- charts[!(is.na(charts$Artist) | charts$Artist==""), ]
charts <- charts[!(is.na(charts$Track_Name) | charts$Track_Name==""), ]
charts$id <- paste(charts$Artist, charts$Track_Name)

rm(charts_original, spot)

streams <- charts %>% ungroup%>% group_by(week_start) %>% summarise(sum=sum(Streams))
streams$week_start <- as.Date(streams$week_start)
streams2 <- inner_join(streams, spot1%>%select(c(2,3)), by=c('week_start'='date2'))

ylim.streams <- c(1e+08, 3.5e+09)   
ylim.close <- c(-300, 356)   
b <- diff(ylim.streams)/diff(ylim.close)
a <- ylim.streams[1] - b*ylim.close[1] 

ggplot(streams2, aes(week_start, sum)) +
        geom_line() +
        geom_line(aes(y = a + Close*b), color = "red") +
        scale_y_continuous("Suma de streams", sec.axis = sec_axis(~ (. - a)/b, name = "Precio de la acción al cierre")) +
        ggtitle("Correlación entre precio acciones y streams")+theme+labs(x='Fecha')+
        theme(axis.line.y.right = element_line(color = "red"), 
              axis.ticks.y.right = element_line(color = "red"),
              axis.text.y.right = element_text(color = "red"), 
              axis.title.y.right = element_text(color = "red"))+
        geom_ribbon(aes(ymin=1.5e+09, ymax=a + Close*b, fill = 'black'), alpha = .1)+
        geom_ribbon(aes(ymin=1.5e+09, ymax=sum, fill = 'red'), alpha = 1)+
        theme(plot.margin = unit(c(1,1,1,.5), "cm"))+
        annotate(geom='text', x=as.Date ('2020-07-01') , y=3.35e+09, vjust=.4, label="COVID\nDeclaración\nPANDEMIA", size=2.5, color='blue')+
        geom_vline(xintercept = as.numeric(as.Date("2020-03-30")), linetype=1, col='blue')+
        geom_vline(xintercept = as.numeric(as.Date("2018-12-31")), linetype=2, col='orange')+
        geom_vline(xintercept = as.numeric(as.Date("2019-12-31")), linetype=2, col='orange')+
        geom_vline(xintercept = as.numeric(as.Date("2020-12-31")), linetype=2, col='orange')+
        geom_label(label="fin de año", x=as.numeric(as.Date("2018-12-31")), y=3e+09, # Rectangle size around label
                   label.size = 0.2,size=2.5,color = "orange",fill="white")+
        geom_label(label="fin de año", x=as.numeric(as.Date("2019-12-31")), y=2.8e+09, # Rectangle size around label
                   label.size = 0.2,size=2.5,color = "orange",fill="white")+
        geom_label(label="fin de año", x=as.numeric(as.Date("2020-12-31")), y=2.6e+09, # Rectangle size around label
                   label.size = 0.2,size=2.5,color = "orange",fill="white")+ theme(legend.position = "none")+xlim(as.Date('2018-03-24'),as.Date('2021-02-03'))
        

########################################### PRUEBA ###############################################################
########################################### PRUEBA ###############################################################
########################################### PRUEBA ###############################################################



