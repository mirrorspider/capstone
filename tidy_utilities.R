require(readr)
require(dplyr)
require(tidytext)
require(tidyr)
require(stringr)
require(scales)
require(ggplot2)
require(SnowballC)
require(tm)

data("stop_words")
fbl <- "./scratch/training/en_US.blogs.txt"
fnw <- "./scratch/training/en_US.news.txt"
ftw <- "./scratch/training/en_US.twitter.txt"

getProfanity <- function(){
        f <- paste0("./sources/", "supporting/", "swearWords.txt")
        profanity <- read_table(f, col_names = "word")
        profanity
}

getExtraStopWords <- function(){
        words <- c("lol", "rt")
        out <- data_frame(word = words)
        out
}

readOrig <- function(filename, colname = "text"){
        t <- read_table(filename, col_names = colname)
        t
}

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

cleanText <- function(tbl){
        # relies on tbl having a column text
        out <- tbl %>% mutate(text = replaceQuotes(text)) %>%
                mutate(text = removeNumbers(text))
        #out <- out %>% anti_join(stop_words)
        out
}

tidyUp <- function (tbl){
        out <- tbl %>% select(text) %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                anti_join(getProfanity()) %>%
                anti_join(getExtraStopWords())
        out
}


tidyUpNgramN <- function (tbl, ngrams = 3){
        replace_reg = "[^A-Za-z0-9 ]"
        #removable <- "\\b[Ll][Oo][Ll]\\b |\\b[Rr][Tt]\\b "
        out <- tbl %>% select(text) %>% 
                mutate(text = str_replace_all(text, replace_reg, "")) %>% 
                #mutate(text = str_replace_all(text, removable, "")) %>% 
                unnest_tokens(ngram, text, token = "ngrams", n = ngrams)
        out
}

tidyUpNgram <- function (tbl, ngrams = 3){
        replace_reg = "[^A-Za-z0-9 ]"
        removable <- "\\b[Ll][Oo][Ll]\\b |\\b[Rr][Tt]\\b "
        out <- tbl %>% select(text) %>% 
                mutate(text = str_replace_all(text, replace_reg, "")) %>% 
                mutate(text = str_replace_all(text, removable, "")) %>% 
                unnest_tokens(ngram, text, token = "ngrams", n = ngrams)
        swear <- getProfanity()
        
        out <- out %>% 
                separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
                #filter(!(word1 %in% stop_words$word | 
                #                 word2 %in% stop_words$word | 
                #                 word3 %in% stop_words$word)) %>%
                filter(!(word1 %in% swear$word | word2 %in% swear$word |
                                 word3 %in% swear$word)) %>% 
                #%>%
                #filter(!(word1 == word2 & word2 == word3))
                #%>%
                #filter(!(word1 %in% stop_words$word & 
                #                 word3 %in% stop_words$word)) %>%
                #filter(!(word1 %in% stop_words$word & 
                #                 word2 %in% stop_words$word)) %>%
                #filter(!(word2 %in% stop_words$word & 
                #                 word3 %in% stop_words$word))
                unite(ngram, word1:word3, sep = " ")
                out
}


tidyStem <- function (tbl, dictionary = ""){
        out <- tbl %>% mutate(word = wordStem(word, 
                                              language = "english"))
        if(sum(nchar(dictionary)) > 0){
                # leaving this in, but it takes too long on even
                # relatively small texts
                out <- out %>% mutate(word = stemCompletion(word, 
                                                dictionary = dictionary,
                                                type = "shortest"))
        }
        out
}


processFileCore <- function(filename){
        orig <- readOrig(filename)
        orig <- cleanText(orig)
        orig
}

processFile <- function(filename, stem = TRUE, ...){
        src <- gsub("^.+\\.([a-z]+)\\.txt$", "\\1", filename)
        orig <- processFileCore(filename)
        tidy <- tidyUp(orig)
        dict <- tidy$word
        tidy <- tidy %>% mutate(source = src)
        if (stem){
                stemmed <- tidy %>% tidyStem()
        }
        else{
                # if no stemming is to be applied use the tidied dataset
                stemmed <- tidy
        }
        
        out <- list(clean = orig, tidy = tidy, stemmed = stemmed, 
                    dictionary = dict)
        out
}

processTbl <- function(tbl, source, stem = TRUE, ...){
        orig <- cleanText(tbl)
        tidy <- tidyUp(orig)
        dict <- tidy$word
        tidy <- tidy %>% mutate(source = source)
        if (stem){
                stemmed <- tidy %>% tidyStem()
        }
        else{
                # if no stemming is to be applied use the tidied dataset
                stemmed <- tidy
        }
        
        out <- list(clean = orig, tidy = tidy, stemmed = stemmed, 
                    dictionary = dict)
        out
}

processFileNgram <- function(filename, stem = TRUE, ...){
        src <- gsub("^.+\\.([a-z]+)\\.txt$", "\\1", filename)
        orig <- processFileCore(filename)
        tidy <- tidyUpNgram(orig)

        out <- list(clean = orig, tidy = tidy)
        out
}

processTblNgram <- function(tbl, source, stem = TRUE, ...){
        orig <- cleanText(tbl)
        tidy <- tidyUpNgram(orig)
        
        out <- list(clean = orig, tidy = tidy)
        out
}

getFrequencies <- function(source.list, gather = TRUE){
        out <- NULL
        for(src in source.list){
                out <- bind_rows(out, src)
        }
        
        out <- out %>% 
                count(source, word) %>%
                group_by(source) %>%
                mutate(proportion = n / sum(n)) %>%
                select(-n) %>%
                spread(source, proportion)
        
        if(gather){
                out <- out %>%
                        gather(source, proportion, 
                names(source.list[1]):names(source.list[length(source.list)-1]))
        }
        out
}

plotIt <- function(tbl){
        y_value <- names(tbl)[2]
        ggplot(tbl, aes(x = proportion, 
                        y = get(y_value), 
                        color = abs(get(y_value) - proportion))) + 
                geom_jitter(alpha = 0.07, size = 2.5, width = 0.3, height = 0.3) +
                geom_text(aes(label = word), check_overlap = TRUE, 
                          hjust = 0.5, vjust = 0.5, color = "darkslategray") +
                geom_abline(color = "gray40", lty = 2) +
                scale_x_log10(labels = percent_format()) + 
                scale_y_log10(labels = percent_format()) +
                scale_color_gradient(
                        limits = c(0, 0.0005), low = "darkslategray3", 
                        high = "gray75") + 
                theme_minimal() + 
                facet_wrap(~source, ncol = 2) + 
                theme(legend.position = "none") + 
                labs(y = y_value, x = NULL)
}

myStemCompletion <- function(x, dictionary){
        # I'm not that keen on the stem completion where the stemming
        # algorithm has reduced the stem to a word ending in i
        # e.g. family -> famili -> familia, city -> citi -> cities
        # this function removes the final i and then performs default stem
        # completion
        out <- x
        pattern <- "^([a-z]*)i$"
        out <- sapply(out, FUN = function(x) {
                        gsub(pattern = pattern, replacement = "\\1", x = x)
                        }, USE.NAMES = FALSE)
        out <- stemCompletion(out, dictionary)
        out
        
}

myStemCompletion2 <- function(x, dictionary){
        # I'm not that keen on the stem completion where the stemming
        # algorithm has reduced the stem to a word ending in i
        # e.g. family -> famili -> familia, city -> citi -> cities
        # this function removes the final i and then performs default stem
        # completion
        out <- data_frame(word = x)
        pattern <- "^([a-z]*)i$"
        out <- out %>% mutate( word = gsub(pattern = pattern, 
                                           replacement = "\\1",
                                           x = word, perl = TRUE))
        out <- stemCompletion(out$word, dictionary)
        out
        
}


getTopWords <- function(source.list, n = 20, dictionary = ""){
        output <- getFrequencies(source.list, gather = FALSE)
        output <- output %>% 
                        replace(is.na(.), 0) %>% 
                        mutate(total = rowSums(.[2:4]))
        output <- head(output %>% arrange(desc(total)), n = n)
        if(sum(nchar(dictionary)) > 0){
                output <- output %>% 
                        mutate(word = myStemCompletion2(word,
                                                     dictionary = dictionary))
        }
        output <- output %>% 
                        gather(key = source, value = total, 
                               twitter, blogs, news)
        output <- output %>%
                        group_by(word) %>%
                        mutate(cumulative = cumsum(total))
        output
}

barPlotIt <- function(tbl, intr = 0.005){
        # finds how many columns there'll be to position a label
        lbl.pos <- length(tbl$word)/length(unique(tbl$source)) -1
        
        ggplot(data = tbl, aes(x = reorder(word, -total), 
                               y = total, fill = source)) + 
                geom_bar(stat = "identity") + 
                labs(title = "Frequently Occurring Words", x = "word", 
                     y = "proportion") + 
                scale_fill_brewer(palette = "Set2", direction = -1) + 
                scale_y_continuous(expand = c(0,0), labels = percent_format()) + 
                theme_minimal() + 
                theme(axis.text.x = element_text(angle = 90, 
                                                 hjust = 1, vjust = 0.4)) + 
                theme(axis.line = element_line(color = "gray40", size = 0.5,
                                                 linetype = "solid")) + 
                theme(panel.grid = element_blank()) + 
                theme(plot.title = element_text(hjust = 0.5)) +
                geom_hline(yintercept = intr, color = "gray80") + 
                geom_text(aes(lbl.pos, intr), color = "gray60", 
                          label = c(percent(intr)), vjust = -0.5, size = 3)
}

igraphPlotIt <- function(bigram_graph){
        a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
        
        ggraph(bigram_graph, layout = "gem") +
                geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                               arrow = a, end_cap = circle(.05, 'inches')) +
                geom_node_point(size = 2, color = "lightblue") + 
                geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
                theme_void()
}

createNgramList <- function(text.corpora, source, maxNGram = 4L, 
                            minNGram = 1L){
        out <- vector("list", length(minNGram:maxNGram))
        clean.text <- text.corpora %>% cleanText()
        #twitter.ngram$tidy <- tidyUpNgramN(twitter.ngram$clean, n = 2)
        j <- 1
        for(i in minNGram:maxNGram){
                ngram.text <- tidyUpNgramN(clean.text, ngrams = i)
                ngram.text <- ngram.text %>%
                                count(ngram) 
                if(i > 1){
                        terms.col.names <- paste0("word", 1:(i-1))
                        ngram.text <- ngram.text %>%
                                separate(col = ngram,
                                         into = c(terms.col.names, "outcome"),
                                         sep = " ") %>%
                                unite(col = "terms", terms.col.names, sep = " ")
                        
                }
                else{
                        ngram.text <- ngram.text %>% rename(outcome = ngram)
                }
                out[[j]] <- ngram.text
                j <- j + 1
        }
        names(out) <- paste0("ngram", minNGram:maxNGram)
        out
}


createNgramListSeparate <- function(text.corpora, source, maxNGram = 4L, 
                            minNGram = 1L){
        out <- vector("list", length(minNGram:maxNGram))
        clean.text <- text.corpora %>% cleanText()
        #twitter.ngram$tidy <- tidyUpNgramN(twitter.ngram$clean, n = 2)
        j <- 1
        for(i in minNGram:maxNGram){
                ngram.text <- tidyUpNgramN(clean.text, ngrams = i)
                ngram.text <- ngram.text %>%
                        count(ngram) 
                if(i > 1){
                        terms.col.names <- paste0("word", 1:i)
                        ngram.text <- ngram.text %>%
                                separate(col = ngram,
                                         into = c(terms.col.names),
                                         sep = " ")
                        
                }
                else{
                        ngram.text <- ngram.text %>% rename(word1 = ngram)
                }
                out[[j]] <- ngram.text
                j <- j + 1
        }
        names(out) <- paste0("ngram", minNGram:maxNGram)
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

returnFirstNWords <- function(s, n = 1, na.rm = TRUE){
        
        if(n == 0){
                out <- ifelse(na.rm, "", NA)
        }
        else if (n > getWordCount(s)){
                out <- s
        }
        else{
                out <- str_split(s, " ", simplify = TRUE)
                out <- out[1:n] %>%
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

getNextWord <- function(search.string, ngramList, remove = TRUE, 
                        simplify = TRUE){

        num.ngram.models <- length(ngramList)
        
        srch <- formatSearchString(search.string)
        
        srch <- returnLastNWords(srch, num.ngram.models -1)
        
        num.words <- getWordCount(srch)
        
        filterlist <- ""
        
        if(simplify){
                out <- NA
        }
        else{
                out = vector("list", num.words)
        }

        for(i in num.words:1){
                s <- returnLastNWords(srch, i)
                message(s)
                fnd <- getOutcome(ngramList[[(i+1)]], s) %>%
                        mutate(ngram.match = i)
                if (is.na(out) && nrow(fnd) > 0){
                        out <- fnd
                        filterlist <- out$outcome
                }
                else{
                        if(remove){
                                fnd <- fnd %>% filter(!outcome %in% filterlist)
                        }
                        if(simplify){
                                if(nrow(fnd) > 0){
                                        out <- bind_rows(out, fnd)
                                }
                        }
                        else{
                                out[[i]] <- fnd
                        }
                        
                        filterlist <- c(filterlist, fnd$outcome)
                }
                
        }

        out
}

