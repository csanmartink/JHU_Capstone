
## Setting the environment to work =============================================

packages <- c('R.utils','tm')
for(p in packages) {
        if (!require(p,character.only = TRUE)) 
                install.packages(p); 
        library(p,character.only = TRUE)
}

# Type of data to use in the analysis
source <- c('blogs','news','twitter')

# Create required folders 
folder <- c('raw','sampled')
for (f in 1:length(folder)) {
        
        if (!file.exists(paste0('Data/',folder[f]))) {
                
                dir.create(paste0('Data/',folder[f]))
                
        }
}


## Obtaining data required =====================================================

# Downloading and unzipping dataset
if(!file.exists("Data/raw/en_US.blogs.txt")){
        
        fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(fileUrl, destfile = "Data/Dataset.zip", method = "curl")
        
        # Unzip dataSet to /dataset/raw directory
        unzip(zipfile = "Data/Dataset.zip", exdir= "Data/raw")
} else {
        print("Files are already downloaded :)")
}


## Basic Document Statistics ===================================================

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