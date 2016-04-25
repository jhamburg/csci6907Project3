# Load in Functions
source("project3Functions.R")


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
require(magrittr)

# For wordnet package
#setDict(file.path("C:","Program Files (x86)","WordNet","2.1","dict"))
setDict("dict")
data(acq)

# Convert to Lower Case
myCorp <- tm_map(acq, content_transformer(tolower))

###############################
## This section will run on the 
## entire corpus to find out
## more of what docs are in it

# Builds output based on original Corp
myCleanCorp <- cleanCorpus(myCorp)
buildOutputs(myCleanCorp)
buildOutputs(myCleanCorp, sparsity = .8)

### Looks like "said" is really frequent.  
### So is "dlrs" or "dollars", share, and company
### Since "said" and "will" are freq, will delete them

updCleanCorp <- tm_map(myCorp, 
                       content_transformer(replaceSimilarWords)) %>%
                cleanCorpus(., c("said", "will", "also"))

buildOutputs(updCleanCorp)

#####################################
# Find the 10 Largest Docs

# 10 Largest Documents by wordcount
top10Results <- tenLargest(myCorp)

# Top 10 Docs by wordcount in a table
top10 <- top10Results[[1]]

# Top 10 docs in a corpus
top10Corp <- top10Results[[2]]
rm(top10Results)


#####################################
# Lecture 7 functions for 10 
# Largest Docs
####################################

for (top10Doc in 1:length(top10Corp)){
  cleanCorpus(top10Corp[top10Doc], c("said", "will")) %>%
    buildOutputs(., 
                 minWordFreq = mean(freq_terms(content(.[[1]]))$FREQ), 
                 docName = paste("Doc ID:", 
                                 meta(top10Corp[top10Doc][[1]], "id")))
}


#####################################
# Functions for 10 Largest Docs
# for 2nd paragraph in proj desc.
####################################

## Find longest words
top10MaxWords <- lapply(top10Corp, maxWord)

## Find longest Sentences
top10Sents <- lapply(top10Corp, maxSent)

## Finds max sentence for each document
top10MaxSents <- sapply(top10Sents, function(x) x[[1]])

## Combines all sentences for top 10 doc into one table
top10AllSents <-rbindlist(lapply(top10Sents, function(x) x[[2]]), 
                          use.names = T)

## Get Parts of Speech
top10AllSents$SentNoPunct <- removePunctuation(top10AllSents$sentence)
posdat <- pos(top10AllSents$SentNoPunct)
posdat1 <- preprocessed(posdat)
desc <- setNames(str_trim(as.character(pos_tags()$Description)),
                 str_trim(as.character(pos_tags()$Tag)))

top10AllSents$sentWPOS <- posdat1[, 1]
top10AllSents$pos <- str_replace_all(posdat1[, 2], desc)

rm(posdat, posdat1, desc)










#tf_idf <- weightTfIdf(m = myCorpTDM, normalize = TRUE)
#tf_idf_mat <- as.matrix(tf_idf)

#tfIdfDist <- dist(tf_idf_mat, method = 'cosine')

#tfIdfDistHclust <- hclust(d = tfIdfDist, method = 'ward.D2')
# plot(tfIdfDistHclust,
#     main = 'Cluster Dendrogram: Ward Cosine Distance',
#     xlab = '', ylab = '', sub = '')







