require(ggplot2)
require(ggraph)

plotConfidence <- function(plotting){
        
        n <- length(plotting$confidence)
        na.present <- anyNA(plotting$confidence)

        confidence.color <- "darkgreen"
        mx <- max(plotting$confidence, na.rm = TRUE)
        if(mx < 0.39) confidence.color <- "darkorange"
        if(mx < 0.1) confidence.color <- "darkred"
        
        g <- ggplot(data = plotting, 
                    aes(x = choice, y = confidence)) 
        if( n > 2 & !na.present){
                g <- g + geom_smooth(colour = confidence.color, 
                                     method = "loess",
                                     method.args = list(degree = 1,
                                                        na.action = na.pass),
                                     se = FALSE,
                                     show.legend = FALSE)
                g1 <- suppressWarnings({ggplot_build(g)})
                tmp_df <- data_frame(x = g1$data[[1]]$x, y = g1$data[[1]]$y)
                g <- g + geom_ribbon(data = tmp_df, inherit.aes = FALSE,
                                     aes(x = x, ymin = 0,
                                        ymax = y), alpha = 0.3,
                                     fill = confidence.color)
        }
        else if(n > 1 & !na.present){
                g <- g + geom_line(colour = confidence.color, 
                                   show.legend = FALSE) +
                        geom_ribbon(aes(x = choice, ymin = 0,
                                        ymax = confidence), alpha = 0.3,
                                    fill = confidence.color)
        }
        else{
                g <- g + geom_point(size = 3, colour = confidence.color)
        }
        g <- g + ylim(0, 1) + 
                theme_bw()
        g
        
}


plotMarkov <- function(graph.data, seed = 67L){
        # test with "the importance" or "predict the future"
        set.seed(seed)
        a <- grid::arrow(type = "closed", length = unit(.1, "inches"))
        
        g <- ggraph(graph.data, layout = "fr") +
                geom_node_point(aes(size = word.frequency, 
                                    color = confidence)) + 
                scale_color_brewer(palette = "Set2", direction = 1,
                                   name = "Link Source",
                                   limits = c("provided", "predicted", "associated")) + 
                scale_size_continuous(range = c(2,10), guide = FALSE) + 
                geom_edge_link(aes(alpha = inclusion, color = confidence),
                               show.legend = FALSE,
                               arrow = a, start_cap = circle(.1, 'inches'),
                               end_cap = circle(.1, 'inches'),
                               width = 1.25) + 
                scale_edge_color_brewer(palette = "Set2", direction = 1,
                                        limits = c("provided", "predicted", "associated")) + 
                scale_edge_alpha(range = c(0.3, 1)) +
                geom_node_text(aes(label = name), vjust = 2, hjust = 0.5) +
                theme_void() +
                theme(legend.position="top")
        g
}