#Codigos conjunto 


install.packages(c("mallet", "gdata", "gutenbergr", "mallet", "tm", "MASS", "topicmodels", "ldatuning", "SnowballC","wordcloud", "RColorBrewer", "readxl", "xlsx", "tidyverse", "tidytext", "lubridate", "zoo", "scales", "plotly", "wordcloud", "htmlwidgets"))

#Cargamos todos los paquetes necesarios 
library(gdata)
library(gutenbergr)
library(mallet) 
library(tm)
library(MASS)
library(topicmodels)
library(ldatuning)

library(wordcloud)
library(RColorBrewer)
library(readxl)
library(xlsx)
library(tidyverse)
library(tidytext)
library(lubridate)
library(zoo)
library(scales)
library(plotly)
library(wordcloud)
library(htmlwidgets)

install.packages("franc")
library(franc)

install.packages("jpeg")  
library(jpeg)

install.packages("imager")
library(imager)

install.packages("magick")
library(magick)

install.packages("cowplot")
library(cowplot)


#Leemos el csv
alta_auto_R_anonimizado <- read_excel("Comentarios_placeto_2018.xlsx")

nombre <- "alta_auto_R_anonimizado"
fecha <- "201808"


guardar <- paste0(nombre,"_",fecha)
dir.create(guardar,showWarnings=FALSE)

subiris <- table(alta_auto_R_anonimizado$Language)
write.csv(subiris,file=paste(guardar,"/cuenta_x_idioma.csv",sep=""))

datos <- alta_auto_R_anonimizado
View(datos)

####Resumen de datos
#head(datos)
#length(datos)
#summary(datos)

####################################################################
####Analisis conjunto ----
####################################################################

###  Identificar lenguaje  ###

datos$lenguaje <- apply(datos,1,franc)

nrow(datos) #8389

colnames(datos)[1]<- "porqueesexcelente"

##Seleccionar los que estan en catal?n y es cierto y a?adir una columna con 1

datos_cat <- subset(datos, lenguaje == "cat")

datos_cat_def <- datos_cat[-c(3, 12, 13, 14,17,18,19, 22, 23, 24,25,26, 27, 28, 29, 30, 31, 39, 44, 45, 46, 47, 49, 50, 53, 56, 57, 58, 59, 64,65,66,68,71,74,75,76,80),]
datos_cat_def <- datos_cat_def %>%
  mutate(num = 1)

datos_new <- left_join(x=datos, y = datos_cat_def, by = "porqueesexcelente")
    
datos <- subset(datos_new, !(num %in% c('1')))

datos <- datos[-2,-3, -4]

datos$num <- NULL
datos$lenguaje.x <-NULL

#datos = chartr('???????????','aeiounAEIOU',datos)

####### not run

#### Subset de la informacion que necesitamos 
#datos_s <- subset(datos, Language=="Spanish")
#table(datos_s$Opinion_proceso)
#datos$numper<-datos$numper
#head(datos)
#datos_s <- subset(datos, Language=="Spanish", select=c(numper, Opiniones))
#datos_texto <- datos_s$Opiniones
#head(datos_texto)

#####



####Text mining 
#Normalizaci?n
docs_ini = VCorpus(VectorSource(datos))
docs = tm_map(docs_ini, removePunctuation)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeWords, stopwords("spanish"))
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs,  stemDocument)

#writeLines(as.character(docs[[1]]))


#Generacion de la matriz
dtm = DocumentTermMatrix(docs)
freq <- sort(colSums(as.matrix(dtm)),decreasing = T)
set.seed(123)
plot.new()


#Guardar la imagen 
windows(7,7)
wordcloud(names(freq),freq=freq, min.freq = 3,colors=brewer.pal(8, "Dark2"),rot.per=0.35, scale = c(3,.5), max.words = 50, random.order = FALSE)
savePlot(file=paste(guardar,"/wordcloud_global2.jpeg",sep=""),type="jpeg")
dev.off() 


# read a sample file (R logo)

im<-load.image("logobs.jpg")
plot(im)

plot <- image_read("wordcloud_global.jpeg")
logo <- image_read("logobs.jpg") 

#image_append(image_scale(c(plot, logo), "300"))



# Palabras frecuentes 
freq = colSums(as.matrix(dtm))
freq = sort(freq, decreasing = TRUE)
#Plot
windows(7,7)
barplot(freq[1:20], las = 2, cex.names = 0.7, col ="lightblue")
savePlot(file=paste(guardar,"/freq_global.jpeg",sep=""),type="jpeg")

#Agrupamiento
#Eliminamos missings
nov_new <- removeSparseTerms(dtm, sparse = .99999)
nov_new <- nov_new %>% as.matrix()
nov_new <- t(nov_new)
no <- nov_new / rowSums(nov_new)
nov_dist <- dist(no, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")

windows(7,7)
plot(nov_hclust, main = "Hclust", sub = "", xlab = "")
savePlot(file=paste(guardar,"/hclust_global.jpeg",sep=""),type="jpeg")







####################################################################
####Analisis negativo ----
####################################################################
#### Seleccion de informaci?n a analizar en espa?ol 
# based on variable values

#datos$numper<-datos$numper
#head(datos)
#datos_s <- subset(datos, Language=="Spanish" & Opinion_proceso<7, select=c(numper, Opiniones))
#datos_texto <- datos_s$Opiniones
#head(datos_texto)

####Text mining 
#Normalizaci?n
docs_ini = VCorpus(VectorSource(datos))
docs = tm_map(docs_ini, removePunctuation)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeWords, stopwords("spanish"))
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs,  stemDocument)
#writeLines(as.character(docs[[1]]))


#Generacion de la matriz
dtm = DocumentTermMatrix(docs)
freq <- sort(colSums(as.matrix(dtm)),decreasing = T)
library(wordcloud)
set.seed(123)
plot.new()
windows(7,7)
wordcloud(names(freq),freq=freq, min.freq = 3,colors=brewer.pal(8, "Dark2"),rot.per=0.35, scale = c(3,.5), max.words = 50, random.order = FALSE)
savePlot(file=paste(guardar,"/wordcloud_negativo.jpeg",sep=""),type="jpeg")
dev.off() 

# Palabras frecuentes 
freq = colSums(as.matrix(dtm))
freq = sort(freq, decreasing = TRUE)
#Plot
windows(7,7)
barplot(freq[1:20], las = 2, cex.names = 0.7, col ="lightblue")
savePlot(file=paste(guardar,"/freq_negativo.jpeg",sep=""),type="jpeg")


#Agrupamiento
#Eliminamos missings
nov_new <- removeSparseTerms(dtm, sparse = .98)
nov_new <- nov_new %>% as.matrix()
nov_new <- t(nov_new)
no <- nov_new / rowSums(nov_new)
nov_dist <- dist(no, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")

windows(7,7)
plot(nov_hclust, main = "Hclust", sub = "", xlab = "")
savePlot(file=paste(guardar,"/hclust_negativo.jpeg",sep=""),type="jpeg")




####Analisis de sentiemientos ----
####################################################################

#Leemos el csv
#View(alta_auto_R_anonimizado)

#datos <- alta_auto_R_anonimizado





####Resumen de datos
#head(datos)
#length(datos)
#summary(datos)

#Definimos un tema para los graficos
tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

#Nos descargamos el lexico afinn
#download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv","lexico_afinn.en.es.csv")


afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

#Seleccionamos nuestros datos
#datos$numper<-datos$numper
#datos_s_pre <- subset(datos, Language=="Spanish", select=c(numper,Opinion_proceso ))
#datos_s <- subset(datos, Language=="Spanish", select=c(numper, Opiniones,Opinion_proceso))
#datos_texto <- datos_s$Opiniones



###  Identificar lenguaje  ###

datos$lenguaje <- apply(datos,1,franc)

nrow(datos) #8389

colnames(datos)[1]<- "porqueesexcelente"

##Seleccionar los que estan en catal?n y es cierto y a?adir una columna con 1

datos_cat <- subset(datos, lenguaje == "cat")

datos_cat_def <- datos_cat[-c(3, 12, 13, 14,17,18,19, 22, 23, 24,25,26, 27, 28, 29, 30, 31, 39, 44, 45, 46, 47, 49, 50, 53, 56, 57, 58, 59, 64,65,66,68,71,74,75,76,80),]
datos_cat_def <- datos_cat_def %>%
  mutate(num = 1)

datos_new <- left_join(x=datos, y = datos_cat_def, by = "porqueesexcelente")

datos <- subset(datos_new, !(num %in% c('1')))

datos <- datos[-2,-3,-4]

datos$num <- NULL
datos$lenguaje.x <-NULL


#Ahora correr desde analisis de sentimientos
#Acabar el leftjoin con numpers (mas abajo)



datos$text <- datos$porqueesexcelente
datos <- datos[-1]

datos$numper <- 1:nrow(datos)


#Separamos las palabras de las frases 
datos_afinn <- datos %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 

#Vamos a sacar una puntuacion generica 
datos_opinion <-
  datos_afinn %>%
  group_by(numper) %>%
  summarise(Puntuacion_datos = mean(Puntuacion)) %>%
  left_join(datos_afinn, ., by = "numper") %>% 
  mutate(Puntuacion_datos = ifelse(is.na(Puntuacion_datos), 0, Puntuacion_datos))


# Mutate when <1 negativa, >1 positiva


datos_opinion <- datos_opinion %>%
  mutate(opinion = case_when(Puntuacion_datos >= 0 ~ 1, 
                             Puntuacion_datos < 0 ~ 0))

datos_opinion2 <- subset(datos_opinion, select=c(numper, opinion))

datos_opinion2 <- unique(datos_opinion2)

datos_final <- left_join(x=datos, y = datos_opinion2, by = "numper")

datos_final[is.na(datos_final)] <- 5


####SUBSET

datos_final_neutro <- subset(datos_final, opinion==5, select="text")
datos_final_positivo <- subset(datos_final, opinion==1, select="text")
datos_final_negativo <- subset(datos_final, opinion==0, select="text")




#Opinion 1 es positiva, opinion 0 es negativa, opinion 5 es neutra
#Hay que rellenar NA con 5 (asumiendo neutralidad) porque el diccionario afinn no tiene guardadas
#las palabras de ciertas frases (debe ser porque son neutras?) y por tanto las frases desaparecen porque no tienen puntuaci?n

##### ANALISIS DE SENTIMIENTO: NEUTRO

####Text mining 
#Normalizaci?n
docs_neutro = VCorpus(VectorSource(datos_final_neutro))
docs = tm_map(docs_neutro, removePunctuation)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeWords, stopwords("spanish"))
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs,  stemDocument)

#writeLines(as.character(docs[[1]]))


#Generacion de la matriz
dtm = DocumentTermMatrix(docs)
freq <- sort(colSums(as.matrix(dtm)),decreasing = T)
set.seed(123)
plot.new()


#Guardar la imagen 
windows(7,7)
wordcloud(names(freq),freq=freq, min.freq = 3,colors=brewer.pal(8, "Dark2"),rot.per=0.35, scale = c(3,.5), max.words = 50, random.order = FALSE)
savePlot(file=paste(guardar,"/wordcloud_neutro.jpeg",sep=""),type="jpeg")
dev.off() 


# read a sample file (R logo)

#im<-load.image("logobs.jpg")
#plot(im)

#plot <- image_read("wordcloud_global.jpeg")
#logo <- image_read("logobs.jpg") 

#image_append(image_scale(c(plot, logo), "300"))



# Palabras frecuentes 
freq = colSums(as.matrix(dtm))
freq = sort(freq, decreasing = TRUE)
#Plot
windows(7,7)
barplot(freq[1:20], las = 2, cex.names = 0.7, col ="lightblue")
savePlot(file=paste(guardar,"/freq_neutro.jpeg",sep=""),type="jpeg")

#Agrupamiento
#Eliminamos missings
nov_new <- removeSparseTerms(dtm, sparse = .98)
nov_new <- nov_new %>% as.matrix()
nov_new <- t(nov_new)
no <- nov_new / rowSums(nov_new)
nov_dist <- dist(no, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")

windows(7,7)
plot(nov_hclust, main = "Hclust", sub = "", xlab = "")
savePlot(file=paste(guardar,"/hclust_neutro.jpeg",sep=""),type="jpeg")









##### ANALISIS DE SENTIMIENTO: POSITIVO

####Text mining 
#Normalizaci?n
docs_neutro = VCorpus(VectorSource(datos_final_positivo))
docs = tm_map(docs_positivo, removePunctuation)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeWords, stopwords("spanish"))
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs,  stemDocument)

#writeLines(as.character(docs[[1]]))


#Generacion de la matriz
dtm = DocumentTermMatrix(docs)
freq <- sort(colSums(as.matrix(dtm)),decreasing = T)
set.seed(123)
plot.new()


#Guardar la imagen 
windows(7,7)
wordcloud(names(freq),freq=freq, min.freq = 3,colors=brewer.pal(8, "Dark2"),rot.per=0.35, scale = c(3,.5), max.words = 50, random.order = FALSE)
savePlot(file=paste(guardar,"/wordcloud_positivo.jpeg",sep=""),type="jpeg")
dev.off() 


# read a sample file (R logo)

#im<-load.image("logobs.jpg")
#plot(im)

#plot <- image_read("wordcloud_global.jpeg")
#logo <- image_read("logobs.jpg") 

#image_append(image_scale(c(plot, logo), "300"))



# Palabras frecuentes 
freq = colSums(as.matrix(dtm))
freq = sort(freq, decreasing = TRUE)
#Plot
windows(7,7)
barplot(freq[1:20], las = 2, cex.names = 0.7, col ="lightblue")
savePlot(file=paste(guardar,"/freq_positivo.jpeg",sep=""),type="jpeg")

#Agrupamiento
#Eliminamos missings
nov_new <- removeSparseTerms(dtm, sparse = .98)
nov_new <- nov_new %>% as.matrix()
nov_new <- t(nov_new)
no <- nov_new / rowSums(nov_new)
nov_dist <- dist(no, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")

windows(7,7)
plot(nov_hclust, main = "Hclust", sub = "", xlab = "")
savePlot(file=paste(guardar,"/hclust_positivo.jpeg",sep=""),type="jpeg")







##### ANALISIS DE SENTIMIENTO: NEGATIVO

####Text mining 
#Normalizaci?n
docs_neutro = VCorpus(VectorSource(datos_final_negativo))
docs = tm_map(docs_negativo, removePunctuation)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeWords, stopwords("spanish"))
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs,  stemDocument)

#writeLines(as.character(docs[[1]]))


#Generacion de la matriz
dtm = DocumentTermMatrix(docs)
freq <- sort(colSums(as.matrix(dtm)),decreasing = T)
set.seed(123)
plot.new()


#Guardar la imagen 
windows(7,7)
wordcloud(names(freq),freq=freq, min.freq = 3,colors=brewer.pal(8, "Dark2"),rot.per=0.35, scale = c(3,.5), max.words = 50, random.order = FALSE)
savePlot(file=paste(guardar,"/wordcloud_negativo.jpeg",sep=""),type="jpeg")
dev.off() 






# read a sample file (R logo)

#im<-load.image("logobs.jpg")
#plot(im)

#plot <- image_read("wordcloud_global.jpeg")
#logo <- image_read("logobs.jpg") 

#image_append(image_scale(c(plot, logo), "300"))



# Palabras frecuentes 
freq = colSums(as.matrix(dtm))
freq = sort(freq, decreasing = TRUE)
#Plot
windows(7,7)
barplot(freq[1:20], las = 2, cex.names = 0.7, col ="lightblue")
savePlot(file=paste(guardar,"/freq_negativo.jpeg",sep=""),type="jpeg")

#Agrupamiento
#Eliminamos missings
nov_new <- removeSparseTerms(dtm, sparse = .98)
nov_new <- nov_new %>% as.matrix()
nov_new <- t(nov_new)
no <- nov_new / rowSums(nov_new)
nov_dist <- dist(no, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")

windows(7,7)
plot(nov_hclust, main = "Hclust", sub = "", xlab = "")
savePlot(file=paste(guardar,"/hclust_negativo.jpeg",sep=""),type="jpeg")







set.seed(1)
plotfreq <- datos_final(fac = factor(sample(LETTERS, 100, replace = TRUE)))
hist(table(plotfreq), xlab = "Frequency of Level Occurrence", main = "")

















#Fase exploracion

# Total
datos_opinion %>%
  count(Opinion_proceso)

# ?nicas
datos_afinn %>% 
  group_by(Opinion_proceso) %>% 
  distinct(Palabra) %>% 
  count()
#Las personas que puntuan bien utilizan m?s palabras diferentes 
#Esto puede estar sesgado por la cantidad de registros positivos 


#Palabras m?s utilizadas por puntuacuin 

jpeg(paste0(guardar,"/palabras_negativa.jpeg"))
map(c("Negativa"), function(sentimiento) {
  datos_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>% 
    ggplot() +
    aes(Palabra, n) +
    geom_col() +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})
dev.off()

jpeg(paste0(guardar,"/palabras_positiva.jpeg"))
map(c("Positiva"), function(sentimiento) {
  datos_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>% 
    ggplot() +
    aes(Palabra, n) +
    geom_col() +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})
dev.off()


jpeg(paste0(guardar,"/pos_neg_por_punt.jpeg"))
datos_afinn %>%
  count(Opinion_proceso, Tipo) %>%
  group_by(Opinion_proceso) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Opinion_proceso, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top")+
  scale_x_continuous( breaks=seq(0, 10, 1))
dev.off()







####COMBINE WORDCLOUD

all_combine <- c(datos_final_negativo, datos_final_positivo)
corpus_review_all=Corpus(VectorSource(all_combine)) 
corpus_review_all=tm_map(corpus_review_all, tolower)
corpus_review_all=tm_map(corpus_review_all, removePunctuation)
corpus_review_all=tm_map(corpus_review_all, removeWords, stopwords("spanish"))

corpus_review_all=tm_map(corpus_review_all, stemDocument)
review_tdm_all <- TermDocumentMatrix(corpus_review_all)
all_m=as.matrix(review_tdm_all)
colnames(all_m)=c("Yes","No")

review_term_freq_all <- rowSums(all_m)
review_word_freq_all <- data.frame(term=names(review_term_freq_all), num = review_term_freq_all)

commonality.cloud(all_m, 
                  colors = "steelblue1",
                  max.words = 30)

comparison.cloud(all_m,
                 colors = c("red", "green"),
                 max.words = 20)


