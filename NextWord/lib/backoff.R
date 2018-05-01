require(dplyr)

calcRemainder <- function(tbl){
        out <- tbl %>%
                group_by(terms) %>% 
                summarise(order = as.integer(max(ngram.match)), 
                          remainder = 1 - (sum(discount*n)/sum(n)))
        out
}

calcDiscount <- function(tbl){
        out <- tbl %>% mutate(discount = 1)
        for(i in 5:1){
                r <- i
                r1 <- r + 1
                
                rn <- tbl %>% filter(n == r) %>% count()
                r1n <- tbl %>% filter(n == r1) %>% count()
                
                disc <- (r1/r) * (r1n/rn)
                
                out[out$n == r, "discount"] <- disc
        }
        out
}

calcDiscountAlt <- function(tbl){
        out <- tbl %>% mutate(discount = 1)
        all.freq <- sum(tbl$n)
        for(i in 5:1){
                r <- i
                r1 <- r + 1
                
                rn <- tbl %>% filter(n == r) %>% count()
                r1n <- tbl %>% filter(n == r1) %>% count()
                
                disc <- (1/all.freq) * (r1) * (r1n/rn)
                
                out[out$n == r, "discount"] <- disc
        }
        out
}
# MyP1 %>% group_by(terms) %>% mutate(tot = sum(n)) %>% ungroup() %>% 
# mutate(prob = (n * discount)/tot) %>% arrange(desc(prob))
# this allows some comparison across ngram depth but 
# we're not discounting lower order matches yet
# use the state senate president subset as an example

err.data.frame <- data_frame(terms = c("nothing", "found"),
                             outcome = c("try", "again"),
                             n = c(0L,0L),
                             ngram.match = c(0L, 0L),
                             discount = c(0,0),
                             prob = c(0,0))

calcProbability <- function(lst, num.match = -1){
        if(is.null(lst[[1]]) || (nrow(lst[[1]]) == 0)){
                #stop("no matches")
                out <- err.data.frame
        }
        else{
                
                all.values <- bind_rows(lst)

                all.values <- calcDiscount(all.values)
                all.rem <- calcRemainder(all.values)
                highest.order <- max(all.values$ngram.match)
                
                tmp <- all.values %>% filter(ngram.match == highest.order)
                all.freq <- sum(tmp$n)
                out <- tmp %>% mutate(prob = (discount * n)/all.freq)
                
        
                j <- highest.order - 1
                while(j > 0){
                        leftoverprob <- all.rem %>% filter(order == (j+1L)) %>%
                                select(remainder) %>% first()
        
                        tmp <- all.values %>% filter(ngram.match == j)
                        tmp.rem <- tmp %>% filter(!outcome %in% out$outcome)
                        all.freq <- sum(tmp$n)
                        alpha.value <- leftoverprob / 
                                (sum((tmp.rem$n * tmp.rem$discount)/all.freq))
                        # alpha.value <- alpha.value %>% select(remainder) %>% first()
                        tmp <- tmp %>% 
                                mutate(prob = alpha.value * ((n * discount)/all.freq)) %>%
                                filter(!outcome %in% out$outcome)
                        out <- bind_rows(out, tmp)
                        j <- j - 1
                }
                out <- out %>% arrange(desc(prob))
                
                if (num.match > 0){
                        out <- out %>% head(num.match)
                }
        }
        out
        

}
