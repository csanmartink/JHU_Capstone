
#' @title 
#' Johns Hopkins University - Data Science Capstone on Coursera
#' 
#' @author 
#' Carolina San Martín
#' 
#' @description 
#' Task 2 - Exploratory data analysis


## Setting the environment to work =============================================

packages <- c('magrittr','dplyr','NLP','tm','RWeka','ggplot2')

for(p in packages) {
        if (!require(p,character.only = TRUE)) 
                install.packages(p); 
        library(p,character.only = TRUE)
}

# Delimit and reduce sparsity
sparse <- 0.99995

doc_clean <- get(load('Data/output/clean_data.RData'))

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
# Count the unique frequent words
length(unique(unigram_mostFreq$Word))
# Select the 20 most frequent uni-words
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
# Count the unique frequent words
length(unique(bigram_mostFreq$Word))
# Select the 20 most frequent two-words
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
# Count the unique frequent words
length(unique(trigram_mostFreq$Word))
# Select the 20 most frequent three-words
trigram_mostFreq_sel <- trigram_mostFreq[1:20,]


## Separating analysis by source of data =============================

for (s in 1:length(source)) {
        
# Unigram tokenization
        unigram_source <- TermDocumentMatrix(doc_clean[s], control = list(tokenize = unigram))
# Reduce sparsity
        unigram_source <- removeSparseTerms(unigram_source, sparse)
# Find the most frequent unigrams with a frequency >= 7
        unigram_freq_source <- findFreqTerms(unigram_source, lowfreq = 7)
        unigram_freq_source <- rowSums(as.matrix(unigram_source[unigram_freq_source, ]))
        unigram_freq_source <- data.frame(Word = names(unigram_freq_source ), Frequency = unigram_freq_source)
        unigram_freq_source <- arrange(unigram_freq_source, desc(Frequency)) %>% mutate(source = source[s])
# Count the unique frequent words
        length(unique(unigram_freq_source$Word))
# Saving results in dataframes separated by source
        assign(paste0('unigram_',source[s]),unigram_source)
        assign(paste0('unigram_freq_',source[s]),unigram_freq_source)

# Bigram tokenization
        bigram_source <- TermDocumentMatrix(doc_clean[s], control = list(tokenize = bigram))
# Reduce sparsity
        bigram_source <- removeSparseTerms(bigram_source, sparse)
# Find the most frequent bigrams with a frequency >= 7
        bigram_freq_source <- findFreqTerms(bigram_source, lowfreq = 7)
        bigram_freq_source <- rowSums(as.matrix(bigram_source[bigram_freq_source, ]))
        bigram_freq_source <- data.frame(Word = names(bigram_freq_source), Frequency = bigram_freq_source)
        bigram_freq_source <- arrange(bigram_freq_source, desc(Frequency)) %>% mutate(source = source[s])
# Count the unique frequent words
        length(unique(bigram_freq_source$Word))
# Saving results in dataframes separated by source
        assign(paste0('bigram_',source[s]),bigram_source)
        assign(paste0('bigram_freq_',source[s]),bigram_freq_source)

# Trigram tokenization
        trigram_source <- TermDocumentMatrix(doc_clean[s], control = list(tokenize = trigram))
# Reduce sparsity
        trigram_source <- removeSparseTerms(trigram_source, sparse)
# Find the most frequent trigrams with a frequency >= 7
        trigram_freq_source <- findFreqTerms(trigram_source, lowfreq = 7)
        trigram_freq_source <- rowSums(as.matrix(trigram_source[trigram_freq_source, ]))
        trigram_freq_source <- data.frame(Word = names(trigram_freq_source), Frequency = trigram_freq_source)
        trigram_freq_source <- arrange(trigram_freq_source, desc(Frequency)) %>% mutate(source = source[s])
# Count the unique frequent words
        length(unique(trigram_freq_source$Word))
# Saving results in dataframes separated by source
        assign(paste0('trigram_',source[s]),trigram_source)
        assign(paste0('trigram_freq_',source[s]),trigram_freq_source)
}


## Plotting results ============================================================

# Unigram


# Visualise the top 20 bigrams
bigram_mostFreq_sel %>% arrange(Frequency) %>% 
        mutate(Word = factor(Word, levels = Word)) %>% 
        ggplot(aes(Frequency,Word)) +
        geom_col() +
        labs(
                x = 'Counts',
                y = element_blank(),
                title = 'Number of Manufacturers in the data set'
        )


