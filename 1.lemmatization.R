library(readxl)
library(readr)
library(dplyr)
library(tm)
library(stam)
library(stringr)
library(tidyverse)
library(tidytext)
library(huge)

# Read xlsx files into one dataframe
integrum <- rbindlist(lapply(dir("data", full.names = T),
                             read_xlsx), fill = T)

# Change colums names
names(integrum) <- c("source","date","time","title","text","website","author",
                     "copies")

# Change date class
integrum$date <- as.Date(integrum$date)
# create covariate year
integrum$year <- format(as.Date(integrum$date, format="%d/%m/%Y"),"%Y")

# data description
table(year(integrum$date))
qplot(nchar(integrum$text))
qplot(integrum$date, bins = 120)

# text cleaning and lemmatization
publications <- integrum$text
publications[1]
publications <- gsub("[\r|\n]", " ", publications)

corpus <- Corpus(VectorSource(publications))

patternTransformer <- content_transformer(function(x, pattern, replace) { return (gsub(pattern, replace, x)) })

mystemAnalyze <- function(word) {
  result <- system("mystem -cl", intern = TRUE, input = word)
  result <- gsub("[{}]", "", result)
  result <- gsub("(\\|[^ ]+)", "", result)
  result <- gsub("\\?", "", result)
  result <- gsub("\\s+", " ", result)
}

corpus <- tm_map(corpus, mystemAnalyze)

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, stopwords("russian"))

corpus <- tm_map(corpus, patternTransformer, "-", " ")
corpus <- tm_map(corpus, patternTransformer, "—", " ")
corpus <- tm_map(corpus, patternTransformer, "–", " ")
corpus <- tm_map(corpus, patternTransformer, "«", " ")
corpus <- tm_map(corpus, patternTransformer, "»", " ")
corpus <- tm_map(corpus, patternTransformer, "„", " ")
corpus <- tm_map(corpus, patternTransformer, "“", " ")
corpus <- tm_map(corpus, patternTransformer, "”", " ")
corpus <- tm_map(corpus, patternTransformer, "№", " ")
corpus <- tm_map(corpus, patternTransformer, "www.+", " ")
corpus <- tm_map(corpus, patternTransformer, "http://+", " ")
corpus <- tm_map(corpus, stripWhitespace)


stopCommonWords <- c("что", "как", "который", "год", "это", "такой", "этот", "весь", "свой", "ребенок","человек", "очень", "наш", "день", "рука", "время", "дом", "место", "вода", "самый", "говорить", "знать", "мочь", "самый", "просто", "понимать", "хотеть")

corpus <- tm_map(corpus, removeWords, stopCommonWords)

# save lemmatized texts as data.frame
corpus <- data.frame(text_lemm=sapply(corpus, identity), stringsAsFactors=F)
integrum_lemm <- cbind(corpus, integrum)
integrum_lemm$time <- NULL
integrum_lemm$author <- NULL

saveRDS(integrum_lemm, file = "intermediate_data/int_lem12286.RDS")

# create term matrix
dtm <- DocumentTermMatrix(corpus, control = list(weighting = weightTf, 
                                                  stopwords = FALSE))

dtm = removeSparseTerms(dtm, 0.99) 

removeCommonTerms <- function (x, pct) 
{
  stopifnot(inherits(x, c("DocumentTermMatrix", "TermDocumentMatrix")), 
            is.numeric(pct), pct > 0, pct < 1)
  m <- if (inherits(x, "DocumentTermMatrix")) 
    t(x)
  else x
  t <- table(m$i) < m$ncol * (pct)
  termIndex <- as.numeric(names(t[t]))
  if (inherits(x, "DocumentTermMatrix")) 
    x[, termIndex]
  else x[termIndex, ]
}

dtm = removeCommonTerms(dtm ,.8)

saveRDS(dtm, file = "intermediate_data/dtm.RDS")