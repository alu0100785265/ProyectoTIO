library(tm)
library(wordcloud)
library(lsa)
library(ggplot2)

Analisis<-function(ruta) {
  txt <- readLines(ruta,encoding="UTF-8")
  
  txt = iconv(txt, to="ASCII//TRANSLIT")
  corpus <- Corpus(VectorSource(txt))
  d <- tm_map(corpus, content_transformer(tolower))
  d <- tm_map(corpus, stripWhitespace)
  d <- tm_map(d, removePunctuation)
  
  # Palabras vacias de la biblioteca
  d <- tm_map(d, removeWords, stopwords("spanish"))
  # Palabras vacias personalizadas y lo convierte a ASCII
  sw <- readLines("C:/Users/Asus/Desktop/stopwords.es",encoding="UTF-8")
  sw = iconv(sw, to="ASCII//TRANSLIT")
  d <- tm_map(d, removeWords, sw)
  
  tdm <- TermDocumentMatrix(d)
  
  
  # convierte a una matriz
  m = as.matrix(tdm)
  
  # conteo de palabras en orden decreciente
  wf <- sort(rowSums(m),decreasing=TRUE)
  # crea un data frame con las palabras y sus frecuencias
  dm <- data.frame(word = names(wf), freq=wf)
  
  # Frecuencia minima igual a 20
  findFreqTerms(tdm, lowfreq=20)
  wordcloud(dm$word, dm$freq,random.order=FALSE, colors=brewer.pal(8,"Dark2"))
  # Grouped Bar Plot
  
  
  
}

Analisis("C:/Users/Asus/Desktop/primero.txt")
#Analisis("C:/Users/Asus/Desktop/segundo.txt")

