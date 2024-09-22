
#' @title 
#' Johns Hopkins University - Data Science Capstone on Coursera
#' 
#' @author 
#' Carolina San Martín
#' 
#' @description 
#' Quiz 1


### Question 0: Exploring data =================================================

# Due to the size of the file, the idea is working with an R connection
con <- file("Data/en_US/en_US.twitter.txt", "r") 

## Read the first line of text 
readLines(con, 1) 
# Read in the next 10 lines of text 
readLines(con, 10) 

# Close the connection
close(con)


### Question 1: How many megabytes =============================================

size <- file.info("Data/en_US/en_US.blogs.txt")
kb <- size$size/1024
mb <- kb/1024
mb


### Question 2: How many lines of text =========================================

lines <- length(readLines("Data/en_US/en_US.twitter.txt"))


### Question 3: The longest line ===============================================

source <- c("blogs","news","twitter")

for (s in 1:length(source)) {
        
        data <- file(paste0("Data/en_US/en_US.",source[s],".txt"),"r")
        lines <- readLines(data)
        close(data)
        result <- summary(nchar(lines))  ## Watch out with the max
        assign(source[s],result[6])
}


### Question 4: Divide the numbers of lines with word 'love' by the number of lines with word 'hate' ====

love <- length(grep("love", twitter_lines))
hate <- length(grep("hate", twitter_lines))
love/hate


### Question 5: Lines with word 'biostats' =====================================

grep("biostats", twitter_lines, value = T)


### Question 6: How many lines with the exact characters "A computer once beat me at chess, but it was no match for me at kickboxing" ====

grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter_lines)

