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


maxWord <- function(doc){
# This function will find the longest word
# It does this by splitting up each word
# in the document into a separate item 
# and then counting the characters
  byWord <- bag_o_words(content(doc))
  wordLengths <- character_count(byWord)
  long_word <- max(wordLengths)
  long_word1 <- byWord[which(wordLengths == long_word)]
  invisible(long_word1)
}

maxSent <- function(doc){
# This function finds the max sentence
# while also creating a table of 
# with a sentence on each row
  
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

replaceSimilarWords <- function(corp){
## This function will replace similiar words in the corpus
  corpClean <- gsub("corp", "company", corp) %>%
          gsub("business", "company", x =.) %>%
          gsub("co ", "company ", x = .) %>%
          gsub("inc", "company", .) %>%
          gsub("companies", "company", .) %>%
          gsub("companys", "company", .) %>%
          gsub("firm ", "company", .) %>%
          gsub("acquired", "acquire", .) %>%
          gsub("acquisition", "acquire", .) %>%
          gsub("dlrs", "dollars", .) %>%
          gsub("ltd", "company", .) %>%
          gsub("mln", "million", .) %>%
          gsub("years", "year", .) %>%
          gsub("shares", "share", .) %>%
          gsub("tender", "cash", .) %>%
          gsub("agreed", "agree", .) 
    invisible(corpClean)
}


buildOutputs <- function(corp, 
                         minWordFreq = 10,
                         sparsity = .75,
                         docName = "Full ACQ Corpus"){
# Creates several output based on 
# minimum freq and sparsity.
# This function will do the following:
#   1. Build dendrogram
#   2. Build wordcloud
#   3. Build bar chart of freq terms
  
# Find Frequent Terms and Sparse Matrix

  myCorpTDM <- TermDocumentMatrix(corp, 
                                  control = list(wordLengths = c(1, Inf)))
  wFreq <- sort(rowSums(as.matrix(myCorpTDM)), decreasing = T)
  if(max(wFreq) < minWordFreq){
    cat("Minimum word frequency filter (minWordFreq: ",
        minWordFreq, 
        ") is greater than the maximum word frequency.",
        "\nSetting minWordFreq to",
        "maximum word frequency:", 
        max(wFreq))
    minWordFreq <- wFreq
  }
  wFreq <- subset(wFreq, wFreq >= minWordFreq)
  if(is.null(wFreq)) stop("minWordFreq too large")

  ## Check to see if there is sparsity, if not will 
  ## filter for hclust based on wFreq terms
  myCorpSparsity <- 
    ifelse(!prod(dim(myCorpTDM)),
           100, 
           round((1 - length(myCorpTDM$v)/
                    prod(dim(myCorpTDM))) * 100)) / 100
  
  if(myCorpSparsity > 0){
    myCorpSparseTDM <- removeSparseTerms(myCorpTDM, sparsity)
  } else { 
    myCorpSparseTDM <- myCorpTDM[names(wFreq), ]
  }
  myCorpDistMatrix <- dist(scale(myCorpSparseTDM))
# Dendrogram 
  DistMatrixHclust <- hclust(d = myCorpDistMatrix, method = 'ward.D2')
  plot(DistMatrixHclust,
       main = paste0('Cluster Dendrogram for ',
                    docName,
                    ': Ward Scaled Distance'),
       xlab = '', ylab = '', sub = '')
  
# Word Cloud
  pal <- brewer.pal(6, "RdYlGn")
  wordcloud(names(wFreq), wFreq, random.order = FALSE, 
            scale = c(2, .3), colors = pal)

# Find the Most Frequent Terms
  fOfTerms <- findFreqTerms(myCorpTDM, minWordFreq)
  cat("Terms Greater than ", minWordFreq, " Freq:\n\n")
  print(fOfTerms)
  
# Bar Chart of Most Frequent Terms
  myCorpDf <- data.table(term = names(wFreq), 
                         freq = wFreq)
  myCorpDf[, termSort := factor(term, levels = term[order(freq)])]
  freqBar <- ggplot(myCorpDf, aes(x = termSort, y = freq)) +
              geom_bar(stat = "identity") +
              xlab("Terms") + 
              ylab("Frequency") + 
              ggtitle(paste("Most Frequent Terms of", docName)) +
              coord_flip()
  print(freqBar)
}


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
