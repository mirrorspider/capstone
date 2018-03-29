require(tm)
#require(Rcpp)
dir.stem <- "./scratch/"
data.stem <- "./data/"
source.stem <- "./sources/"

#Create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {
                return (gsub(pattern," ",x))
        })
#how to use
#corp <- tm_map(corp, toSpace, "[^[:graph:]]")

createCorpus <- function(set = "training"){
        db.name <- paste0(data.stem, set, ".db")
        directory <- paste0(dir.stem, set)
        directory <- DirSource(directory)
        cpus <- PCorpus(directory, readerControl = list(reader = readPlain, 
                        language = "en_US", load = TRUE),
                        dbControl = list(useDB = TRUE, 
                        dbName = db.name, dbType = "DB1"))
#        cpus <- Corpus(directory, readerControl = list(reader = readPlain, 
#                        language = "en", load = TRUE))
#        (Corpus(DirSource(txt), readerControl = list(reader
#= readPlain, language = "en_US", load = TRUE), dbControl = list(useDb =
#TRUE, dbName = "oviddb", dbType = "DB1")))        
        cpus
        
}

cleanCorpus <- function(c){
        f <- paste0(source.stem, "supporting/", "swearWords.txt")
        profanity <- read.table(f, sep = "\n", stringsAsFactors = FALSE, 
                                col.names = c("words"))
        cpus <- tm_map(c, toSpace, "[^[:graph:]]")
        cpus <- tm_map(cpus, content_transformer(tolower))
        cpus <- tm_map(cpus, removeWords, stopwords("english"))
        cpus <- tm_map(cpus, removeNumbers)
        cpus <- tm_map(cpus, removePunctuation)
        cpus <- tm_map(cpus, removeWords, profanity$words)
        cpus <- tm_map(cpus, stripWhitespace)
        cpus
}