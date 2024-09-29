
#' @title 
#' Johns Hopkins University - Data Science Capstone on Coursera
#' 
#' @author 
#' Carolina San Martín
#' 
#' @description 
#' Task 2 - Exploratory data analysis


## Setting the environment to work =============================================

packages <- c('magrittr','dplyr','NLP','tm','RWeka')

for(p in packages) {
        if (!require(p,character.only = TRUE)) 
                install.packages(p); 
        library(p,character.only = TRUE)
}

# Delimit and reduce sparsity
sparse <- 0.99995

doc_clean <- get(load('Data/output/clean_data.rda'))

#' Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora. 
#' Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.


## Tokenization ================================================================

# Unigram tokenization
unigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
mt_unigram <- TermDocumentMatrix(doc_clean, control = list(tokenize = unigram))
# Reduce sparsity
mt_unigram <- removeSparseTerms(mt_unigram, sparse)
# Find the most frequent unigrams with a frequency >= 7
unigram_mostFreq <- findFreqTerms(mt_unigram, lowfreq = 7)
unigram_mostFreq <- rowSums(as.matrix(mt_unigram[unigram_mostFreq, ]))
unigram_mostFreq <- data.frame(Word = names(unigram_mostFreq), Frequency = unigram_mostFreq)
unigram_mostFreq <- arrange(unigram_mostFreq, desc(Frequency))
unigram_mostFreq_sel <- unigram_mostFreq[1:20,]

# Bigram tokenization
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
mt_bigram <- TermDocumentMatrix(doc_clean, control = list(tokenize = bigram))
# Reduce sparsity
mt_bigram <- removeSparseTerms(mt_bigram, sparse)
# Find the most frequent bigrams with a frequency >= 7
bigram_mostFreq <- findFreqTerms(mt_bigram, lowfreq = 7)
bigram_mostFreq <- rowSums(as.matrix(mt_bigram[bigram_mostFreq, ]))
bigram_mostFreq <- data.frame(Word = names(bigram_mostFreq), Frequency = bigram_mostFreq)
bigram_mostFreq <- arrange(bigram_mostFreq, desc(Frequency))
bigram_mostFreq_sel <- bigram_mostFreq[1:20,]

# Trigram tokenization
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
mt_trigram <- TermDocumentMatrix(doc_clean, control = list(tokenize = trigram))
# Reduce sparsity
mt_trigram <- removeSparseTerms(mt_trigram, sparse)
# Find the most frequent trigrams with a frequency >= 7
trigram_mostFreq <- findFreqTerms(mt_trigram, lowfreq = 7)
trigram_mostFreq <- rowSums(as.matrix(mt_trigram[trigram_mostFreq, ]))
trigram_mostFreq <- data.frame(Word = names(trigram_mostFreq), Frequency = trigram_mostFreq)
trigram_mostFreq <- arrange(trigram_mostFreq, desc(Frequency))
trigram_mostFreq_sel <- trigram_mostFreq[1:20,]

