
ColourBorders <- function(df, col_value){
    
    df[1,] <- col_value
    df[,1] <- col_value
    df[nrow(df),] <- col_value
    df[,ncol(df)] <- col_value

    return(df)
    
}
