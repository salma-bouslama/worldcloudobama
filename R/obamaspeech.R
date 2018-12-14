

obama_speech <- function() {

  library(tm)
  library(readr)
  library(wordcloud)
  txt=readLines("speech.txt")
  txt[1:10]
  # On va nettoyer  les données, supprimer les ponctuations
  txt <- removePunctuation(txt)
  # Supprimer les nombres
  txt <- removeNumbers(txt)
  txt[1:10]

  #Supprimer tous les espaces vides.
  txt <- txt[-which(txt=="")]
  txt[1:10]
  #Rendre  tout en miniscule
  for(i in 1:length(txt))
    txt[i]=tolower(txt[i])
  txt[1:10]
  #Supprimer tous les stopwords
  txt <- removeWords(txt,stopwords("en"))
  txt[1:10]

  #Choisir de specifier certains mots mr , us .
  txt <- removeWords(txt,c("mr","us"))
  txt[1:10]

  #Transformer l'objet txt dans un format Corpus pour qu'il puisse être analysé
  corpus <- Corpus(VectorSource(txt))

  #Traçage du word cloud

  #Transformer le Corpus en une matrice
  tdm <- TermDocumentMatrix(corpus,control = list(minWordLength=3))
  tdm
  dim(tdm)
  #On peut conclure que dans le texte il y a + 1559 mots. + 113 paragraphes
  #Chaque ligne de la matrice tdm correspond à un mot et chaque colonne correspond à un paragraphe.

  sum((tdm==0))
  sum((tdm!=0))

  # Chercher Les mots les plus fréquents dans le texte

  m <- as.matrix(tdm)
  freqWords=rowSums(m)
  freqWords=sort(freqWords,d=T)
  t(freqWords[1:20])
  #Eliminer le mot applause du Corpus
  i=grep('applause',rownames(m))
  m=m[-i,]

  #Chercher  le mot economy dans le texte et sa fréquence d'apparition
  i=grep('economy',rownames(m))
  sum(m[i,])

  #Chercher le mot security
  i=grep('security',rownames(m))
  sum(m[i,])

  #Traçer maintenant le word cloud.
  freqWords=rowSums(m)
  v=sort(freqWords,d=T)
  dt=data.frame(word=names(v),freq=v)
  head(dt)

  par(bg="gray")
  wordcloud(dt$word,dt$freq,min.freq = 20)
}
