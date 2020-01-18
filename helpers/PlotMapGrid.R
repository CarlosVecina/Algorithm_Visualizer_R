
PlotMapGrid <- function(df, matrix_x_size, matrix_y_size){
    
    df <-
        rbind(
            which(df== 1, arr.ind = TRUE) %>% cbind(fill_col="#623B17"),
            which(df == 2, arr.ind = TRUE) %>% cbind(fill_col="#13293D"),
            which(df == 3, arr.ind = TRUE) %>% cbind(fill_col="#ffff66"),
            which(df == 4, arr.ind = TRUE) %>% cbind(fill_col="#99ccff"),
            which(df == 5, arr.ind = TRUE) %>% cbind(fill_col="#1B998B")
            
        ) %>% 
        data.frame(stringsAsFactors = F) %>%
        transmute(y = as.numeric(row), x = as.numeric(col), fill_col=fill_col)
    return(
        df %>%
        ggplot(aes(x+0.5,y+0.5)) +
        geom_tile(width = 1, height = 1, fill = df$fill_col, col="black") +
        scale_y_reverse() +
        scale_x_continuous(breaks = seq(0, matrix_x_size, 1), limits = c(0+0.5, matrix_x_size+1.5), minor_breaks = NULL)+
        scale_y_continuous(breaks = seq(0, matrix_y_size, 1), limits = c(0+0.5, matrix_y_size+1.5), minor_breaks = NULL)+
        theme_linedraw()+
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.x=element_blank(),
              axis.ticks.y=element_blank())
        )
    
}
