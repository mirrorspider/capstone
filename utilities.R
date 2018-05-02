require(readr)
require(stringr)
require(dplyr)
require(caret)

srcbl <- "./sources/en_US/en_US.blogs.txt"
srcnw <- "./sources/en_US/en_US.news.txt"
srctw <- "./sources/en_US/en_US.twitter.txt"


readFile <- function(filename){
        fileTbl <- read_table(filename, col_names = "text")
        fileTbl <- fileTbl %>%
                mutate(words = str_count(text, "\\S+")) %>%
                mutate(chars = nchar(text, type = "bytes"))
        fileTbl

}

produceSummary <- function(blogs, news, twitter){
        
        source <- c("blogs", "news", "twitter")
        records <- c(length(blogs$words), length(news$words),
                     length(twitter$words))
        words <- c(sum(blogs$words), sum(news$words),
                     sum(twitter$words))
        chars <- c(sum(blogs$chars), sum(news$chars),
                   sum(twitter$chars))
        max.words <- c(max(blogs$words), max(news$words),
                   max(twitter$words))
        max.chars <- c(max(blogs$chars), max(news$chars),
                   max(twitter$chars))
        mean.words <- c(mean(blogs$words), mean(news$words),
                       mean(twitter$words))
        mean.chars <- c(mean(blogs$chars), mean(news$chars),
                       mean(twitter$chars))
        out <- tibble(source = source, records = records,
                      words = words, chars = chars, 
                      max.words = max.words, max.chars = max.chars,
                      mean.words = mean.words, mean.chars = mean.chars
                      )
        out
        

}

splitDataSet <- function(tbl, p = 0.75, train.only = FALSE){
        train.idx <- createDataPartition(tbl$words, 
                                         p = p,
                                         list = FALSE)
        train <- tbl[train.idx, ]
        if(!train.only){
                test <- tbl[-train.idx, ]
                val.idx <- createDataPartition(test$words,
                                               p = 0.5, 
                                               list = FALSE)
                validation <- test[val.idx, ]
                test <- test[-val.idx, ]
                
                dSets <- list(train = train, test = test, 
                                validation = validation)
        }
        else
        {
                dSets <- list(train = train)
        }
        dSets
}

writeFile <- function(char.vect, filename, out.dir = "./", 
                      create = TRUE){
        if(!dir.exists(out.dir)){
                if(create){
                        dir.create(out.dir)
                }
                else{
                        stop("Directory does not exist.")
                }
        }
        f <- paste0(out.dir, filename)
        cat(char.vect, file = f, sep = "\n")
        f
}

extractDataSet <- function(){
        source.stem <- "./sources/en_US/"
        out.stem <- "./scratch/"
        source.data <- c("en_US.blogs.txt", "en_US.news.txt",
                         "en_US.twitter.txt")
        
        for (f in source.data){
                        source.file <- file(paste0(source.stem, f))
                        source.file <- readFile(source.file)
                        train.idx <- createDataPartition(source.file$words, 
                                                     p = 0.75,
                                                     list = FALSE)
                        train <- source.file[train.idx, ]
                        test <- source.file[-train.idx, ]
                        val.idx <- createDataPartition(test$words,
                                                       p = 0.5, 
                                                       list = FALSE)
                        validation <- test[val.idx, ]
                        test <- test[-val.idx, ]
                        
                        extract <- list(train = train, test = test, 
                                        validation = validation)
                        sets <- c("train", "test", "validation")
                        for (set in sets){
                                out.directory <- paste0(out.stem, 
                                                "/", set, "/")
                                writeFile(extract[[set]]$text, 
                                          out.dir = out.directory,
                                        filename = f)
                        }

        }
        
}




