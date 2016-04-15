# Set directory

#dir <- "G:/admin/GW/CSCI6907/project3"
#dir <- file.path("C:","GW","CSCI6907","project3")


# Load package and data
#require(ctv)
#install.views("NaturalLanguageProcessing")
#update.views("NaturalLanguageProcessing")

require(RColorBrewer)
require(ggplot2)
require(tm)
require(SnowballC)
require(textreuse)
require(wordnet)
require(zipfR)
require(wordcloud)
require(openNLP)
require(proxy)
require(stringr)
require(data.table)
require(openNLP)
require(qdap)

# For wordnet package
setDict(file.path("C:","Program Files (x86)","WordNet","2.1","dict"))


#oldDir <- getwd()

data(acq)

# Convert to Lower Case
myCorp <- tm_map(acq, content_transformer(tolower))

#changeDlrs <- function(x) gsub("dlrs", "dollars", x)
#myCorp <- tm_map(myCorp, content_transformer(changeDlrs))

#### 10 Largest Documents by wordcount
tenLargest <- function(corpus){
  ## This function find the largest 10
  ## documents based on word counts and 
  ## returns a table of those documents
  ## with their word counts sorted by size.
  ## Returns a table of top 10 and a corpus
  ## of the top 10 documents for further
  ## analysis.
  
  # Find largest 10 documents by summing 
  # rows in Document Term Matrix
  origDTM <- 
    DocumentTermMatrix(corpus, 
                       control = list(wordLengths = c(1, Inf)))
  Large10Freq <- sort(rowSums(as.matrix(origDTM)),
                      decreasing = T)[1:10]
  
  # Now will get the corpus of the docs
  Large10Corp <- myCorp[names(myCorp) 
                        %in% names(Large10Freq)]
  
  # Now will get the names of the docs
  doctitles <- sapply(Large10Corp, 
                      function(x) meta(x, "heading"))
  doctitles <- data.frame(docID = names(doctitles),
                          name = unlist(doctitles))
  
  doctitles <- data.table(
    merge(doctitles,
          data.frame(wordCount = Large10Freq),
          by.x = "docID", 
          by.y = 0))
  doctitles <- doctitles[order(-wordCount)]
  
  
  return(list(doctitles, Large10Corp))
}

top10Results <- tenLargest(myCorp)

# Top 10 Docs by wordcount in a table
top10 <- top10Results[[1]]

# Top 10 docs in a corpus
top10Corp <- top10Results[[2]]
rm(top10Results)

## Find longest Sentences and words
maxWord <- function(doc){
  byWord <- bag_o_words(content(doc))
  wordLengths <- character_count(byWord)
  long_word <- max(wordLengths)
  long_word1 <- byWord[which(wordLengths == long_word)]
  invisible(long_word1)
}

top10MaxWords <- lapply(top10Corp, maxWord)

maxSent <- function(doc){
  # splits into sentences and counts words
  sentence <- sent_detect_nlp(content(doc))
  counts <- sapply(sentence, word_count)
  
  # max sentence
  mSent <- max(counts)

  # puts the sentences and their word counts into a Data Table
  counts1 <- data.table(DocID = meta(doc, "id"),
                        DocHeader = meta(doc, "heading"),
                        sentence = sentence, 
                        WordCount = counts)
  finalCounts <- counts1[order(-WordCount)]
  
  # Combines max sentence and table table
  finalList <- list(mSent, finalCounts)
  
  invisible(finalList)
}

top10Sents <- lapply(top10Corp, maxSent)

## Finds max sentence for each document
top10MaxSents <- sapply(top10Sents, function(x) x[[1]])

## Combines all sentences for top 10 doc into one table
top10AllSents <-rbindlist(lapply(top10Sents, function(x) x[[2]]), 
                          use.names = T)


top10AllSents$SentNoPunct <- removePunctuation(top10AllSents$sentence)

## Get Parts of Speech
posdat <- pos(top10AllSents$SentNoPunct)
posdat1 <- preprocessed(posdat)
desc <- setNames(str_trim(as.character(pos_tags()$Description)),
                 str_trim(as.character(pos_tags()$Tag)))

top10AllSents$sentWPOS <- posdat1[, 1]
top10AllSents$pos <- str_replace_all(posdat1[, 2], desc)







cleanCorpus <- function(corp, extraStopWords){
  ## This function will do the following:
  #   1: Remove Punctuation
  #   2: Remove Numbers
  #   3: Remove "stop" words and any extra words
  #      provided by the user
  #   4: Remove extra whitespace  
  if (missing(extraStopWords)) extraStopWords <- ""
  stopifnot(is.character(extraStopWords))
  
  corpClean <- tm_map(x = corp, FUN = PlainTextDocument) %>%
    tm_map(x = ., FUN = removePunctuation) %>%
    tm_map(x = ., FUN = removeNumbers) %>%
    tm_map(x = ., FUN = removeWords, c(stopwords(kind = 'en'),
                                       extraStopWords)) %>%
    tm_map(x = ., FUN = stripWhitespace) 
  
  invisible(corpClean)
}

myCleanCorp <- cleanCorpus(myCorp)

myCorpTDM <- TermDocumentMatrix(myCleanCorp, 
                                control = list(wordLengths = c(1, Inf)))

fOfTerms <- findFreqTerms(myCorpTDM, 5)
fOfTerms

wFreq <- rowSums(as.matrix(myCorpTDM))
wFreq <- subset(wFreq, wFreq >= 25)

myCorpDf <- data.table(term = names(wFreq), 
                       freq = wFreq)

myCorpDf[, termSort := factor(term, levels = term[order(freq)])]


plotDF <- ggplot(myCorpDf, aes(x = termSort, y = freq)) +
          geom_bar(stat = "identity") +
          xlab("Terms") + 
          ylab("Frequency") + 
          coord_flip()
plotDF

myCorpSparseTDM <- removeSparseTerms(myCorpTDM, .6)

dist(scale(myCorpSparseTDM))

### Looks like "said" is really frequent.  
### So is "dlrs" or "dollars", share, and company

findAssocs(myCorpTDM, "dlrs", 0.80)


tf_idf <- weightTfIdf(m = myCorpTDM, normalize = TRUE)
tf_idf_mat <- as.matrix(tf_idf)

tf_idf_dist <- dist(tf_idf_mat, method = 'cosine')

clust_h <- hclust(d = tf_idf_dist, method = 'ward.D2')
plot(clust_h,
     main = 'Cluster Dendrogram: Ward Cosine Distance',
     xlab = '', ylab = '', sub = '')







## Unnecessary Functions

posUsingNLP <- function(){
  zz <- top10AllSents[1, ]
  zz1 <- removePunctuation(zz$sentence)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(zz1, list(sent_token_annotator, word_token_annotator))
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  a3 <- annotate(zz1, pos_tag_annotator, a2)
  
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, `[[`, "POS")
  
  r1 <- sprintf("%s/%s", as.String(zz1)[a3w], tags)
  r2 <- paste(r1, collapse = " ")
  
  zz$SentWPOS <- r2
}

tryToUseWordnet <- function(){
  filt <- getTermFilter(type = "ExactMatchFilter", 
                        "computer", 
                        ignoreCase = TRUE)
  tms <- getIndexTerms("NOUN", 5, filt)
  aa <- getSynsets(tms[[1]])
}


convertToTextReuseCorp <- function(){
  ## Maybe try to redo 10 Largest with TextReuseCorpus?
  
  zz <- lapply(top10Corp, function(x) TextReuseCorpus(text = content(x),
                                                      meta = list(meta(x))))
  
  yy <- TextReuseTextDocument(top10Corp[[1]])
}
