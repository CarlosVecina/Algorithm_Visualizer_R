source("helpers/GetNeighbors.R")
library(plyr)
library(tidyverse)
# frontier = Queue()
# frontier.put(start )
# visited = {}
# visited[start] = True
# 
# while not frontier.empty():
#   current = frontier.get()
# for next in graph.neighbors(current):
#   if next not in visited:
#   frontier.put(next)
# visited[next] = True

# mylist<-list()
# # enqueue
# mylist <- c(mylist, list(1:5))
# #dequeue
# first <- mylist[[1]]
# mylist <- mylist[-1]
M <- matrix(ncol = 8, byrow = TRUE, c(
  0,0,0,0,4,0,1,0,
  0,0,1,2,4,1,1,0,
  1,0,0,0,4,0,1,0,
  0,0,1,1,0,1,1,0,
  1,0,1,0,0,0,0,0,
  0,0,0,0,4,1,0,0
))
start <- c(x=2,y=3)
frontier = list()

frontier = c(frontier, list(start))
visited = data.frame(x=NA, y=NA, value=NA, round = NA)
# visited["start"] = T
round <- 0
while(length(frontier) != 0){
  current = frontier[[1]]
  frontier <- frontier[-1]
  neighbors <- GetNeighbors(M,current[["x"]],current[["y"]],wall_value = 1)
  round = round + 1
  for(row_next_name in rownames(neighbors)){
    if(nrow(match_df(neighbors[row_next_name,], visited[,c("x","y","value")]))==0){
      # browser()
      frontier = c(frontier, list(neighbors[row_next_name,]))
      visited = rbind(visited, cbind(neighbors[row_next_name,],round=round))
    }
  }
}

visited <- na.omit(visited)
visited["xx"] <- visited["x"] 
visited["x"] <- visited["y"] 
visited["y"] <- visited["xx"]
ggplot(visited, aes(x, y)) +
  # ggplot maze part
  geom_tile(width = 1, height = 1, fill = "#623B17", data = walls) +
  geom_point(aes(group = round)) +
  scale_y_reverse(breaks = seq(6, 1, -1), limits = c(6, 0), minor_breaks = NULL) +
  scale_x_continuous(breaks = seq(1, 100, 1), limits = c(0, ncol(M)), minor_breaks = NULL) +
  coord_fixed() +
  transition_time(visited$round)
  




