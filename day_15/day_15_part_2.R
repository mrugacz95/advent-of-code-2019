source("integer_computer.R")
source("bfs.R")

lastMove <- NULL
lastPosition <- NULL
oxygenLoc <- NULL
input <- function(){
  lastPosition <<- findNewPosition()
  moveAndDirection <- makeMove(lastPosition)
  lastMove <<- moveAndDirection[["direction"]]
  return(lastMove)
}
output <- function(value){
  directions <- data.frame(x=c(0,0,1,-1), y=c(1,-1,0,0))
  triedPosition = currentPos + directions[lastMove, ]
  wall = value == 0
  if(!any(coords$x == triedPosition[["x"]] & coords$y == triedPosition[["y"]])){
    coords[nrow(coords) + 1, ] <<- c(triedPosition[["x"]], triedPosition[["y"]], wall, FALSE, 1, 0)
  }
  if(value != 0){ # update position
    delta <- switch(lastMove, c(x=0,y=1), c(x=0,y=-1), c(x=1,y=0),c(x=-1,y=0))
    currentPos <<- currentPos + delta
  }
  if(value == 2){
    print("Found oxygen location")
    oxygenLoc <<- currentPos
  }
}
run(input, output)
# test data
# coords <- data.frame(x=integer(),
#                      y=integer(),
#                      status=logical(),
#                      visited=logical(),
#                      parent=integer(),
#                      dist=integer())
# coords[1, ] <- c(2,0, TRUE, FALSE, 1, 0)
# coords[2, ] <- c(3,0, TRUE, FALSE, 1, 0)
# 
# coords[3, ] <- c(1,-1, TRUE, FALSE, 1, 0)
# coords[4, ] <- c(2,-1, FALSE, FALSE, 1, 0)
# coords[5, ] <- c(3,-1, FALSE, FALSE, 1, 0)
# coords[6, ] <- c(4,-1, TRUE, FALSE, 1, 0)
# coords[7, ] <- c(5,-1, TRUE, FALSE, 1, 0)
# 
# coords[8, ] <- c(1,-2, TRUE, FALSE, 1, 0)
# coords[9, ] <- c(2,-2, FALSE, FALSE, 1, 0)
# coords[10, ] <- c(3,-2, TRUE, FALSE, 1, 0)
# coords[11, ] <- c(4,-2, FALSE, FALSE, 1, 0)
# coords[12, ] <- c(5,-2, FALSE, FALSE, 1, 0)
# coords[13, ] <- c(6,-2, TRUE, FALSE, 1, 0)
# 
# coords[14, ] <- c(1,-3, TRUE, FALSE, 1, 0)
# coords[15, ] <- c(2,-3, FALSE, FALSE, 1, 0)
# coords[16, ] <- c(3,-3, FALSE, FALSE, 1, 0)
# coords[17, ] <- c(4,-3, FALSE, FALSE, 1, 0)
# coords[18, ] <- c(5,-3, TRUE, FALSE, 1, 0)
# 
# coords[19, ] <- c(2,-4, TRUE, FALSE, 1, 0)
# coords[20, ] <- c(3,-4, TRUE, FALSE, 1, 0)
# coords[21, ] <- c(4,-4, TRUE, FALSE, 1, 0)

printGrid(c(x=0,y=0))
currentPos = oxygenLoc
makeMove(c(x=.Machine$integer.max, y=.Machine$integer.max))
minutes <- max(coords$dist)
cat("Minutes needed", minutes)

