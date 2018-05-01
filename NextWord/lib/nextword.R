require(dplyr)
require(tidyr)
require(ggplot2)
require(stringr)

replaceQuotes <- function(textcol){
        # unicode values for left and right single quotes
        apos.pattern <- "[\u2018|\u2019]"
        # unicode values for left and right double quotes and the double prime
        quot.pattern <- "[\u201C|\u201D|\u2033]"
        out <- gsub(pattern = apos.pattern, replace = "\'",
                    x = textcol)
        out <- gsub(pattern = quot.pattern, replace = "\"",
                    x = out)
        out
}

removeNumbers <- function(textcol){
        # matches a word consisting entirely of numbers
        num.pattern <- "\\b[0-9]+\\b"
        # matches a word consisting entirely of non alphanumerics
        non.alpha <- "\\b[^A-Za-z0-9'\" ]+\\b"
        out <- gsub(pattern = num.pattern, replace = " ",
                    x = textcol)
        out <- gsub(pattern = non.alpha, replace = " ",
                    x = out)
        out
}

formatSearchString <- function(s){
        replace_reg = "[^A-Za-z0-9 ]"
        out <- tolower(s)
        out <- out %>% replaceQuotes() %>% removeNumbers() %>%
                str_replace_all(replace_reg, "") %>%
                trimws()
        out
}

dropLastWord <- function(s){
        out <- str_split(s, " ", simplify = TRUE)
        out <- out[1:(length(out)-1)] %>%
                paste(collapse = " ")
        out
}

returnLastNWords <- function(s, n = 1, na.rm = TRUE){

        if(n == 0){
                out <- ifelse(na.rm, "", NA)
        }
        else if (n > getWordCount(s)){
                out <- s
        }
        else{
                out <- str_split(s, " ", simplify = TRUE)
                out <- out[(length(out) - (n -1)):length(out)] %>%
                paste(collapse = " ")
        }
        out
        
}

getOutcome <- function(data, search.string){
        out <- data %>%
                filter(terms == search.string) %>%
                arrange(desc(n))
        out
}

getWordCount <- function(search.string){
        ind.words <- str_split(search.string, " ", simplify = TRUE)
        num.words <- length(ind.words)
        num.words
}


sliceData <- function(str, ngramList){
        num.words <- length(str)
        out <- ngramList
        for(i in 1:num.words){
                out <- out %>% filter(get(paste0("word",i)) == str[i])
        }
        out
}

getNextWordSlim <- function(search.string, ngramList){
        
        num.ngram.models <- (dim(ngramList)[2]) - 1
        
        srch <- formatSearchString(search.string)
        
        srch <- returnLastNWords(srch, num.ngram.models -1)
        
        num.words <- getWordCount(srch)
        
        out = vector("list", num.words)

        words <- str_split(srch, " ", simplify = TRUE)
        
        filtered.list <- ngramList

        i = 1
        for(i in 1:num.words){
                
                w <- words[i:num.words]
                filtered.list <- sliceData(w, ngramList)
                
                ngram.match <- num.words - i + 1

                group.cols <- paste0("word", 1:(ngram.match + 1))
                term.cols <- paste0("word", 1:ngram.match)
                outcome.col <- paste0("word", (ngram.match + 1))
                
                out.list <- filtered.list  %>%
                        select(c(group.cols, "n")) %>%
                        group_by_at(group.cols) %>%
                        summarise(frequency = sum(n)) %>%
                        ungroup() %>%
                        unite(terms, term.cols, sep = " ") %>%
                        rename(outcome = !! outcome.col) %>%
                        rename(n = frequency) %>%
                        mutate(ngram.match = as.integer(ngram.match)) %>%
                        arrange(desc(n))
                        
                
                out[[ngram.match]] <- out.list
        }
        out
}
