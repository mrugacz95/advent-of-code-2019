file <- "day_15.in"
# read memory
memory <- read.table(file,
                     header = FALSE,
                     sep = ",",
                     dec = ".")
memory <- as.numeric(unlist(memory))
# init varibales
pointer = 0
relativeBase = 0
running = TRUE

readFromMemory <- function(position) {
  if(position < 0){
    stop("Position can't be lower than zero.")
  }
  if(position > length(memory)){
    memory[[position + 1]] = 0
    return(0)
  }
  return(memory[[position + 1]])
}
getMode <- function(offset) {
  mode <- readFromMemory(pointer)
  for (variable in 0:(offset)) {
    mode <- mode %/% 10
  }
  mode <- mode %% 10
  return(mode)
}
getParam <- function(offset) {
  value <- switch(getMode(offset) + 1,
                  readFromMemory(readFromMemory(pointer + offset)),
                  readFromMemory(pointer + offset),
                  readFromMemory(relativeBase + readFromMemory(pointer + offset)))
  if(is.null(value)){
    stop(cat("Wrong param mode p:", pointer + 1, "o: ", offset, "m+1:", getMode(offset) + 1))
  }
  return(value)
}
saveValue <- function(offset, value){
  switch(getMode(offset) + 1,
         { 
           memory[[readFromMemory(pointer + offset) + 1]] <<- value
           return(TRUE)
         },
         { memory[[pointer + offset + 1]] <<- value
         return(TRUE) },
         { 
           memory[[relativeBase + readFromMemory(pointer + offset) + 1]] <<- value 
           return(TRUE)
         }
  )
  stop(cat("Wrong save mode p:", pointer + 1, "o: ", offset, "m+1:", getMode(offset) + 1))
  
}

run <- function(input, output){
  while(running){
    opcode <- readFromMemory(pointer) %% 100
    if(opcode == 99){
      running = FALSE
      break
    }
    # print(cat("opcode: ",opcode, " p:", pointer, " :D"))
    switch(opcode,
           { #1
             a <- getParam(1)
             b <- getParam(2)
             saveValue(3, a+b)
             pointer <<- pointer + 4
           },
           { #2
             a <- getParam(1)
             b <- getParam(2)
             saveValue(3, a*b)
             pointer <<- pointer + 4
           },
           { #3
             value <- input()
             saveValue(1, value)
             pointer <<- pointer + 2
           },
           { #4
             a <- getParam(1)
             output(a)
             pointer <<- pointer + 2
           },
           { #5
             a <- getParam(1)
             a <- getParam(1)
             b <- getParam(2)
             if(a != 0){
               pointer <<- b
             }
             else {
               pointer <<- pointer + 3
             }
           },
           { #6
             a <- getParam(1)
             b <- getParam(2)
             if(a == 0){
               pointer <<- b
             }
             else {
               pointer <<- pointer + 3
             }
           },
           { #7
             a <- getParam(1)
             b <- getParam(2)
             value <- if(a < b) 1 else 0
             saveValue(3, value)
             pointer <<- pointer + 4
           },
           { #8
             a <- getParam(1)
             b <- getParam(2)
             value <- if(a == b) 1 else 0
             saveValue(3, value)
             pointer <<- pointer + 4
           },
           { #9
             a <- getParam(1)
             relativeBase <<- relativeBase + a
             pointer <<- pointer + 2
           })
  }
}
# Task starts here
print("hello")
coords <- data.frame(x=integer(),
                     y=integer(),
                     status=logical(),
                     visited=logical(),
                     parent=numeric())
coords[1, ] <- c(0,0, FALSE, FALSE, 1)
#coords[2, ] <- c(1,0, TRUE, FALSE, -1)
#coords[3, ] <- c(1,1, TRUE, FALSE, -1)
#coords[4, ] <- c(0,1, TRUE, FALSE, -1)
#coords[5, ] <- c(-1,1, TRUE, FALSE, -1)
#coords[6, ] <- c(-1,0, TRUE, FALSE, -1)
#coords[7, ] <- c(-1,-1, TRUE, FALSE, -1)
#coords[8, ] <- c(1,-1, TRUE, FALSE, -1)
#coords[9, ] <- c(0,-1, FALSE, FALSE, -1)

currentPos <- c(x=0,y=0)
lastMove <- -1
findNewPosition <- function(){
  for(row in 1:nrow(coords)){
    if(coords[row, ]$status){ # skip searching neighbours of walls
      next
    }
    directions <- data.frame(x=c(1,0,0,-1), y=c(0,1,-1,0))
    for(d in 1:nrow(directions)){
      neighbour <- directions[d, ] + coords[row,c("x","y")]
      if(nrow(coords[which(coords$x==neighbour[[1]] & # if neigbour is not known
                           coords$y==neighbour[[2]]), ]) == 0){
        return(neighbour) # go there
      }
    }
  }
  stop("Lack of next position")
}
makeMove <- function(target){
  q = list()
  coords$visited <<- FALSE
  coords$parent <<- -1
  # push currentPos to queue
  start <- which(coords$x==currentPos[["x"]] & coords$y==currentPos[["y"]])
  coords[start, "parent"] <<- -1
  q[[1]] <- start
  lastVisited <- NULL
  lastDirection <- -1
  while(length(q) != 0){
    current = q[[1]]
    coords[current, "visited"] <<- TRUE
    q <- q[-1] # pop first element
    directions <- data.frame(x=c(0,0,1,-1), y=c(1,-1,0,0))
    for(d in 1:nrow(directions)){
      neighbour <- directions[d, ] + coords[current, c("x", "y")]
      if(all(neighbour == target)){
        lastVisited <- current
        lastDirection <- d
        break
      }
      mask = coords$x==neighbour[[1]] & coords$y==neighbour[[2]]
      if(all(!mask)){ # neighbour unknown
        next
      }
      nId <- which(mask)
      if(coords[nId, "status"] == TRUE){ # ommit walls
        next
      }
      if(coords[nId, "visited"] == FALSE){
        coords[nId, "parent"] <<- current
        q[[length(q) + 1]] <- nId
      }
    }
    if(!is.null(lastVisited)){
      break
    }
  }
  if(coords[lastVisited, "parent"] == -1){
    return(lastDirection)
  }
  while(coords[lastVisited, "parent"] != start){
    lastVisited = coords[lastVisited, "parent"]
  }
  if(coords[lastVisited, "x"] > currentPos[1]){
    return(3)
  }
  if(coords[lastVisited, "x"] < currentPos[1]){
    return(4)
  }
  if(coords[lastVisited, "y"] > currentPos[2]){
    return(1)
  }
  if(coords[lastVisited, "y"] < currentPos[2]){
    return(2)
  }
  stop("Couldnt find direciton")
}

printGrid <- function(){
  for(y in max(coords$y):min(coords$y)){
    for(x in min(coords$x):max(coords$x)){
      if(x==currentPos[["x"]] && y==currentPos[["y"]]){
        cat('D')
        next
      }
      mask <- coords$x==x & coords$y==y
      if(!any(mask)){
        cat(' ')
      }
      else{
        row = which(mask)
        if(coords[row, ]$status == TRUE){
          cat("#")
        }
        else {
          cat(".")
        }
      }
    }
    cat("\n")
  }
}

lastMove <- NULL
lastPosition <- NULL
input <- function(){
  lastPosition <<- findNewPosition()
  cat("new target", lastPosition$x, lastPosition$y, "\n")
  lastMove <<- makeMove(lastPosition)
  cat("planned move", lastMove, "\n")
  return(lastMove)
}
output <- function(value){
  cat("output:", value, "\n")
  printGrid()
  if(value + 1 == 1 || value + 1 == 2) {
    directions <- data.frame(x=c(0,0,1,-1), y=c(1,-1,0,0))
    triedPosition = currentPos + directions[lastMove, ]
    wall = value == 0
    if(!any(coords$x == triedPosition[["x"]] & coords$y == triedPosition[["y"]])){
      coords[nrow(coords) + 1, ] <<- c(triedPosition[["x"]], triedPosition[["y"]], wall, FALSE, 1)
    }
  }
  switch(value + 1, 
         {
           print("didnt moved")
         },
         { 
           delta <- switch(lastMove, c(x=0,y=1), c(x=0,y=-1), c(x=1,y=0),c(x=-1,y=0))
           currentPos <<- currentPos + delta
         },
         { stop("Target found")  })
}
run(input, output)

