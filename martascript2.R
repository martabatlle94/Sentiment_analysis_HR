###  Identificar lenguaje y eliminar catalán ###

datos2$lenguaje <- apply(datos2,1,franc)

nrow(datos2) #8389

colnames(datos2)[1]<- "quepodriamejorar"

##Seleccionar los que estan en catalán y es cierto y añadir una columna con 1

datos_cat_def2 <- subset(datos2, lenguaje == "cat")

#datos_cat_def2 <- datos_cat2[-c(3, 12, 13, 14,17,18,19, 22, 23, 24,25,26, 27, 28, 29, 30, 31, 39, 44, 45, 46, 47, 49, 50, 53, 56, 57, 58, 59, 64,65,66,68,71,74,75,76,80),]
datos_cat_def2 <- datos_cat_def2 %>%
  mutate(num = 1)

datos_new2 <- left_join(x=datos2, y = datos_cat_def2, by = "quepodriamejorar")

datosx <- subset(datos_new2, !(num %in% c('1')))

datos2 <- datosx[-2,-3, -4]

datos2$num <- NULL
datos2$lenguaje.x <-NULL

#datos = chartr('áéíóúñÁÉÍÓÚ','aeiounAEIOU',datos)

####Text mining: corpus y matriz
#Normalización
docs_ini2 = VCorpus(VectorSource(datos2))
docs2 = tm_map(docs_ini2, removePunctuation)
docs2 = tm_map(docs2, removeNumbers)
docs2 = tm_map(docs2, content_transformer(tolower))
docs2 = tm_map(docs2, removeWords, stopwords("spanish"))
docs2 = tm_map(docs2, stripWhitespace)
docs2 = tm_map(docs2,  stemDocument)

#writeLines(as.character(docs[[1]]))


#Generacion de la matriz
dtm2 = DocumentTermMatrix(docs2)
freq2 <- sort(colSums(as.matrix(dtm2)),decreasing = T)
set.seed(123)
plot.new()


#Guardar la imagen 
windows(7,7)
wordcloud(names(freq2),freq=freq2, min.freq = 3,colors=brewer.pal(8, "Dark2"),rot.per=0.35, scale = c(3,.5), max.words = 50, random.order = FALSE)
savePlot(file=paste(guardar,"/wordcloud_global2.jpeg",sep=""),type="jpeg")
dev.off() 


# Palabras frecuentes 
freq2 = colSums(as.matrix(dtm2))
freq2 = sort(freq2, decreasing = TRUE)
#Plot
windows(7,7)
barplot(freq[1:10], las = 2, cex.names = 0.7, col ="lightblue")
savePlot(file=paste(guardar,"/freq_global2.jpeg",sep=""),type="jpeg")


####Analisis de sentimiento ----
####################################################################


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


afinn2 <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()


datos2$text <- datos2$quepodriamejorar
datos2 <- datos2[-1]

datos2$numper <- 1:nrow(datos2)


#Separamos las palabras de las frases 
datos_afinn2 <- datos2 %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn2, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) 

#Vamos a sacar una puntuacion generica 
datos_opinion2 <-
  datos_afinn2 %>%
  group_by(numper) %>%
  summarise(Puntuacion_datos = mean(Puntuacion)) %>%
  left_join(datos_afinn2, ., by = "numper") %>% 
  mutate(Puntuacion_datos = ifelse(is.na(Puntuacion_datos), 0, Puntuacion_datos))


# Mutate when <1 negativa, >1 positiva


datos_opinion2 <- datos_opinion2 %>%
  mutate(opinion = case_when(Puntuacion_datos >= 0 ~ 1, 
                             Puntuacion_datos < 0 ~ 0))

datos_opinion2 <- subset(datos_opinion2, select=c(numper, opinion))

datos_opinion22 <- unique(datos_opinion2)

datos_final22 <- left_join(x=datos2, y = datos_opinion22, by = "numper")

datos_final22[is.na(datos_final22)] <- 5


####SUBSET

datos_final_neutro2 <- subset(datos_final22, opinion==5, select="text")
datos_final_positivo2 <- subset(datos_final22, opinion==1, select="text")
datos_final_negativo2 <- subset(datos_final22, opinion==0, select="text")




#Opinion 1 es positiva, opinion 0 es negativa, opinion 5 es neutra
#Hay que rellenar NA con 5 (asumiendo neutralidad) porque el diccionario afinn no tiene guardadas
#las palabras de ciertas frases (debe ser porque son neutras?) y por tanto las frases desaparecen porque no tienen puntuación

##### ANALISIS DE SENTIMIENTO: NEUTRO

####Text mining 
#Normalización
docs_neutro = VCorpus(VectorSource(datos_final_neutro2))
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
savePlot(file=paste(guardar,"/wordcloud_neutro2.jpeg",sep=""),type="jpeg")
dev.off() 



# Palabras frecuentes 
freq = colSums(as.matrix(dtm))
freq = sort(freq, decreasing = TRUE)
#Plot
windows(7,7)
barplot(freq[1:10], las = 2, cex.names = 0.7, col ="lightblue")
savePlot(file=paste(guardar,"/freq_neutro2.jpeg",sep=""),type="jpeg")


##### ANALISIS DE SENTIMIENTO: POSITIVO

####Text mining 
#Normalización
docs_positivo = VCorpus(VectorSource(datos_final_positivo2))
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
savePlot(file=paste(guardar,"/wordcloud_positivo2.jpeg",sep=""),type="jpeg")
dev.off() 


# Palabras frecuentes 
freq = colSums(as.matrix(dtm))
freq = sort(freq, decreasing = TRUE)
#Plot
windows(7,7)
barplot(freq[1:10], las = 2, cex.names = 0.7, col ="lightblue")
savePlot(file=paste(guardar,"/freq_positivo2.jpeg",sep=""),type="jpeg")


##### ANALISIS DE SENTIMIENTO: NEGATIVO

####Text mining 
#Normalización
docs_negativo = VCorpus(VectorSource(datos_final_negativo2))
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
savePlot(file=paste(guardar,"/wordcloud_negativo2.jpeg",sep=""),type="jpeg")
dev.off() 

# Palabras frecuentes 
freq = colSums(as.matrix(dtm))
freq = sort(freq, decreasing = TRUE)
#Plot
windows(7,7)
barplot(freq[1:10], las = 2, cex.names = 0.7, col ="lightblue")
savePlot(file=paste(guardar,"/freq_negativo_2.jpeg",sep=""),type="jpeg")


#Fase exploracion

a<- nrow(datos_final_neutro2)
b<- nrow(datos_final_positivo2)
c<- nrow(datos_final_negativo2)
slice = c(a, b, c)
labels =c("Neutro", "Positivo", "Negativo")

windows(7,7)
pie(slice, label= labels, main = "Tipo de respuesta")
savePlot(file=paste(guardar,"/tipoderespuesta2.jpeg",sep=""),type="jpeg")


###WORDCLOUD 2


all_combine <- c(datos_final_positivo2, datos_final_negativo2)

# create corpus
corpus = Corpus(VectorSource(all_combine))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

# comparison cloud
windows(7,7)
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("green", "red"), 
                 title.size=1.5, max.words=40)
savePlot(file=paste(guardar,"/wordcloud_comb2.jpeg",sep=""),type="jpeg")





