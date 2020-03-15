
GetNeighbors <- function(matrix, x, y, wall_value = NULL){
  
  value <- c()
  neighbors <- data.frame(x=c(x+1,x-1,x,x),y=c(y,y,y-1,y+1))
  neighbors <- subset(neighbors, x!=0 & y!=0 & x<=nrow(M) & y<=ncol(M))
  rownames(neighbors)=NULL  
  
  for(i in 1:nrow(neighbors)){
    value <- c(value,matrix[neighbors[i,]$x,neighbors[i,]$y])
  }
  neighbors["value"] <- value
  
  if(!is.null(wall_value)){
    neighbors <- subset(neighbors, value!=wall_value)
  }
  

  return(as.data.frame(neighbors))
}
