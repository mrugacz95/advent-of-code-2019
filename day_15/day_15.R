source("integer_computer.R")
source("bfs.R")

lastMove <- NULL
lastPosition <- NULL
input <- function(){
  lastPosition <<- findNewPosition()
  # cat("new target", lastPosition$x, lastPosition$y, "\n")
  moveAndDirection <- makeMove(lastPosition)
  lastMove <<- moveAndDirection[["direction"]]
  stepsNeeded <- moveAndDirection[["steps"]]
  # cat("planned move", lastMove, "\n")
  # cat("steps needed", stepsNeeded, "\n")
  # printGrid(lastPosition)
  return(lastMove)
}
output <- function(value){
  # cat("output:", value, "\n")
  directions <- data.frame(x=c(0,0,1,-1), y=c(1,-1,0,0))
  triedPosition = currentPos + directions[lastMove, ]
  wall = value == 0
  if(!any(coords$x == triedPosition[["x"]] & coords$y == triedPosition[["y"]])){
    coords[nrow(coords) + 1, ] <<- c(triedPosition[["x"]], triedPosition[["y"]], wall, FALSE, 1, 0)
  }
  switch(value + 1, 
         {
           # print("didnt moved")
         },
         { 
           delta <- switch(lastMove, c(x=0,y=1), c(x=0,y=-1), c(x=1,y=0),c(x=-1,y=0))
           currentPos <<- currentPos + delta
         },
         { 
           # update position before counting steps
           delta <- switch(lastMove, c(x=0,y=1), c(x=0,y=-1), c(x=1,y=0),c(x=-1,y=0))
           currentPos <<- currentPos + delta
           startingPos = c(x=0, y=0)
           printGrid(startingPos)
           moveAndDirection = makeMove(startingPos)
           cat("Steps needed", moveAndDirection[["steps"]], "\n")
           running <<- FALSE
          })
}
run(input, output)

