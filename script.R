library(tm)
library(wordcloud)
library(lsa)
library(ggplot2)




separar<-function(wf){
  datos <- c()
  for (i in 1:3) {
    num <- wf[[i]]
    nombres <- names(wf[i])
    for(i in 1:num){
      datos <- c(datos,nombres)
    }
  }
  return(datos)
  
}


count<-function(wf,ley){
  count = 0
  datos<- c()
  for(i in 1:3){
    count = count + wf[[i]]
  }
  
  for(i in 1:count){
    datos <- c(datos,ley)
  }
  return(datos)
}



grafica <- function(terminos,ley){
 
  # Stacked Bar Plot with Colors and Legend
  counts <- table(terminos, ley)
  barplot(counts, main="Comparativa Leyes",bty='L',
          legend = rownames(counts), xlab="LOGSE LOE", col=c("darkblue","red","green","yellow","brown"))
  
}

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
  #wordcloud(dm$word, dm$freq,random.order=FALSE, colors=brewer.pal(8,"Dark2"))
  
  return(wf)
  

}



loe <- Analisis("C:/Users/Asus/Desktop/primero.txt") #Este es la LOE

logse<- Analisis("C:/Users/Asus/Desktop/segundo.txt") #Este es LOGSE

terminos <- c()
ley <- c()

terminos<- c(terminos,separar(loe))
ley <- c(ley,count(loe,"LOE"))

terminos<- c(terminos,separar(logse))
ley <- c(ley,count(logse,"LOGSE"))

grafica(terminos,ley)



