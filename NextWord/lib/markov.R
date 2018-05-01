require(dplyr)
require(stringr)
require(igraph)
require(scales)
# source("./utils.R", chdir = TRUE)

getToChains <- function(w, text.corpora, n = 20){
        out <- text.corpora %>%
                filter(word2 == w) %>%
                group_by(word1, word2) %>%
                summarise(total = sum(n)) %>%
                arrange(desc(total)) %>%
                ungroup() %>%
                head(n)
        out
}

getFromChains <- function(w, text.corpora, n = 20){
        out <- text.corpora %>%
                filter(word1 == w) %>%
                group_by(word1, word2) %>%
                summarise(total = sum(n)) %>%
                arrange(desc(total)) %>%
                ungroup() %>%
                head(n)
        out
}

getChainLink <- function(w1, w2, text.corpora){
        out <- text.corpora %>%
                filter(word1 == w1) %>%
                filter(word2 == w2) %>%
                group_by(word1, word2) %>%
                summarise(total = sum(n)) %>%
                ungroup()
        out
}

createGraph <- function(tbl, text.corpora){
        out <- graph_from_data_frame(tbl)
        
        v <- vertex.attributes(out)$name
        
        lkp <- text.corpora %>%
                filter(word1 %in% v) %>%
                group_by(word1) %>%
                summarise(total = sum(n)) %>%
                ungroup()
        
        i <- sapply(v, FUN = function(x){ 
                s <- lkp %>% 
                        filter(word1 == x) %>%
                        select(total) %>%
                        first()
                s
                }, USE.NAMES = FALSE)
        
        i <- as.integer(rescale(i, to = c(2L,9L)))
        
        out <- set.vertex.attribute(out, "word.frequency", value = i)
        
        words = vector("list", 5)
        words[[1]] <- tbl %>%
                filter(confidence == "provided") %>%
                select(word1, confidence) %>%
                unique() %>%
                mutate(confidence = as.character(confidence))
        
        words[[2]] <- tbl %>%
                filter(confidence == "predicted") %>%
                anti_join(words[[1]], by = c("word1")) %>%
                select(word1, confidence) %>%
                unique() %>%
                mutate(confidence = "provided")
        
        words[[3]] <- tbl %>%
                filter(confidence == "predicted") %>%
                anti_join(words[[1]], by = c("word2" = "word1")) %>%
                anti_join(words[[2]], by = c("word2" = "word1")) %>%
                select(word2, confidence) %>%
                unique() %>%
                rename(word1 = word2)%>%
                mutate(confidence = as.character(confidence))
        
        tmp <- bind_rows(words)
        
        words[[4]] <- tbl %>%
                filter(confidence == "associated") %>%
                select(word1, confidence) %>%
                unique() %>%
                anti_join(tmp, by = c("word1")) %>%
                mutate(confidence = as.character(confidence))

        tmp <- bind_rows(words)

        words[[5]] <- tbl %>%
                filter(confidence == "associated") %>%
                select(word2, confidence) %>%
                unique() %>%
                rename(word1 = word2) %>%
                anti_join(tmp, by = c("word1")) %>%
                mutate(confidence = as.character(confidence))
        
        lkp <- bind_rows(words)
        
        i <- sapply(v, FUN = function(x){ 
               s <- lkp %>% 
                        filter(word1 == x) %>%
                        select(confidence) %>%
                        first()
                s
        }, USE.NAMES = FALSE)
        
        out <- set.vertex.attribute(out, "confidence", value = i)
        out
        #lkp
        #words

}


assembleChainData <- function(found, text.corpora, n = 20){
        s <- found %>% arrange(desc(ngram.match)) %>%
                select(terms) %>%
                filter()
        terms <- s[1L, "terms"]
        terms <- terms$terms
        terms <- str_split(terms, " ", simplify = TRUE)
        wc <- length(terms)
        
        out <- vector("list", (length(terms) -1L + 2L))
        
        j <- 1L
        
        if(wc > 1L){
        
                for(i in 1:(wc - 1L)){
                         lnk <- getChainLink(terms[i], 
                                             terms[(i+1L)], 
                                             text.corpora) %>%
                                 mutate(inclusion = 1.0) %>%
                                 mutate(confidence = "provided")
                         out[[j]] <- lnk
                         j <- j + 1L
                }
        }
        
        for(i in found$outcome){
                        lnk <- getChainLink(terms[wc], i, text.corpora) %>%
                                mutate(inclusion = 0.75) %>%
                                mutate(confidence = "predicted")
                        out[[j]] <- lnk
                        j <- j + 1L
        }

        tmp <- bind_rows(out)
        
        out[[j]] <- getToChains(terms[1L], text.corpora, n) %>%
                anti_join(tmp, by = c("word1" = "word2", "word2" = "word1")) %>%
                mutate(inclusion = 0.5) %>%
                mutate(confidence = "associated")
        j <- j + 1L
        out[[j]] <- getFromChains(terms[wc], text.corpora, n) %>%
                anti_join(tmp, by = c("word1", "word2")) %>%
                mutate(inclusion = 0.5) %>%
                mutate(confidence = "associated")
        
        out <- bind_rows(out) %>% mutate(confidence = as.factor(confidence))
        out <- createGraph(out, text.corpora)
        out

        
}