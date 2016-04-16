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
