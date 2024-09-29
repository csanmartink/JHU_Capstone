
#' @title 
#' Johns Hopkins University - Data Science Capstone on Coursera
#' 
#' @author 
#' Carolina San Martín
#' 
#' @description 
#' Task 1 - Getting and cleaning data


## Setting the environment to work =============================================

packages <- c('R.utils','tm','SnowballC','magrittr','dplyr','textreg','RWeka')

for(p in packages) {
        if (!require(p,character.only = TRUE)) 
                install.packages(p); 
        library(p,character.only = TRUE)
}

# Type of data to use in the analysis
source <- c('blogs','news','twitter')

# Create required folders 
folder <- c('raw','sampled','input','output')
for (f in 1:length(folder)) {
        
        if (!file.exists(paste0('Data/',folder[f]))) {
                
                dir.create(paste0('Data/',folder[f]))
                
        }
}


## Obtaining data required =====================================================

# Downloading and unzipping dataset
if(!file.exists('Data/raw/en_US.blogs.txt')){
        
        fileUrl <- 'https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip'
        download.file(fileUrl, destfile = 'Data/Dataset.zip', method = 'curl')
        
        # Unzip dataSet to /dataset/raw directory
        unzip(zipfile = 'Data/Dataset.zip', exdir= 'Data/raw')
} else {
        print('Files are already downloaded :)')
}


## Basic documents statistics ==================================================

# Checking the size of each file used
for (s in 1:length(source)) {
        print(paste0('Check file sizes in MB of file ',source[s]))
        print(file.info(paste0('Data/raw/final/en_US/en_US.',source[s],'.txt'))$size / (1024*1024))
}

# Counting lines of each file
for (s in 1:length(source)) {
        print(paste0('View line counts of file ',source[s]))
        print(countLines(paste0('Data/raw/final/en_US/en_US.',source[s],'.txt')))
}


## Perform Sampling ============================================================

# Given the large amount of text and limited computational resources, sampling is performed. 10000 lines per file is randomly sampled and saved to disk.
set.seed(1234)

for (s in 1:length(source)) {
        
        raw <- file(paste0('Data/raw/final/en_US/en_US.',source[s],'.txt'),'r')
        
        # Read the first line of text 
        print(readLines(raw, 1))
        # Read the next line of text 
        print(readLines(raw, 5))
        
        lines <- readLines(raw, encoding = 'UTF-8')
        close(raw)
        sampled <- lines[sample(1:length(lines),10000)]
        assign(paste0('sample_',source[s]),sampled)
}

sampled <- c(sample_twitter,sample_news,sample_blogs)
writeLines(sampled, 'Data/sampled/sample_data.txt')

# remove temporary variables
rm(sample_twitter,sample_news,sample_blogs,sample_data)


## Initial cleaning ============================================================

# Using the tm package, the sampled data is used to create a corpus. 

# Take sampled data
file <- file.path('Data/sampled', '')
doc <- Corpus(DirSource(file))

# Define a function in order to convert characters UTF-8 to ASCII. The non-convertible characters will be replaced witha a blank. The goal is to remove all special characters 
char <- c("'")
textToAscii <- function(x) (iconv(x, "UTF-8", "ASCII", sub = ""))

# Corpus creation
doc_clean <- VCorpus(VectorSource(doc))
# Substitute specific special characters (hexadecimal X92 e X91)
doc_clean <- tm_map(doc_clean, content_transformer(gsub), pattern = char, replacement = "'", ignore.case = TRUE)

# Remove special characters
doc_clean <- tm_map(doc_clean, content_transformer(textToAscii))

# Convert all characters into lowercase
doc_clean <- tm_map(doc_clean, content_transformer(tolower))

# Remove web urls
doc_clean <- tm_map(doc_clean, content_transformer(gsub), pattern = "http\\S+\\s*", replacement = "", ignore.case = TRUE)


## Profanity filtering =========================================================

# Download and cleaning a list of profanities
if (!file.exists('Data/input/Profanity-list.csv')) {

        fileUrl <- 'https://www.frontgatemedia.com/wp-content/uploads/2014/03/Terms-to-Block.csv'
        download.file(fileUrl, destfile = 'Data/input/Profanity-list.csv', method = 'curl')
        
} else {
        print('File is already downloaded :)')
}

# Leaving data as clean dataframe
profanity <- read.csv('Data/input/profanity-list.csv', header = FALSE, 
                      skip = 4)
profanity %<>% select(V2) %>% rename(word = V2)
profanity$word <- gsub(',','',profanity$word)
profanities <- as.character(profanity$word)

# Remove profane words. It is suitable to do before removing numbers as some profane words contain numbers (i.e. "a55" for "ass")
doc_clean <- tm_map(doc_clean, removeWords, profanities)


## Final cleanining ============================================================

# Remove English stopwords
doc_clean <- tm_map(doc_clean, removeWords, stopwords('english'))

# Remove punctuation, numbers, and strip whitespace
removing <- c('removePunctuation','removeNumbers','stripWhitespace')
for (f in 1:length(removing)) {
        doc_clean <- tm_map(doc_clean, eval(parse(text=removing[f])))
}

# Stemming (Porter’s stemming)
doc_clean <- tm_map(doc_clean, stemDocument)

# Save object to use in following tasks
writeCorpus(doc_clean, 'Data/output')
save.corpus.to.files(doc_clean, filename = 'Data/output/clean_data')

# Rename output file (not possible from the previous code line)
if (file.exists('Data/output/1.txt')){
        # Rename file name
        file.rename('Data/output/1.txt','Data/output/clean_data.txt')
} else {
        print('File not found')
}
