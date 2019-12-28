coords <- data.frame(x=integer(),
                     y=integer(),
                     status=logical(),
                     visited=logical(),
                     parent=integer(),
                     dist=integer())

coords[1, ] <- c(0,0, FALSE, FALSE, 1, 0)
# test data
#coords[2, ] <- c(1,0, TRUE, FALSE, -1, 0)
#coords[3, ] <- c(1,1, TRUE, FALSE, -1, 0)
#coords[4, ] <- c(0,1, TRUE, FALSE, -1, 0)
#coords[5, ] <- c(-1,1, TRUE, FALSE, -1, 0)
#coords[6, ] <- c(-1,0, TRUE, FALSE, -1, 0)
#coords[7, ] <- c(-1,-1, TRUE, FALSE, -1, 0)
#coords[8, ] <- c(1,-1, TRUE, FALSE, -1, 0)
#coords[9, ] <- c(0,-1, FALSE, FALSE, -1, 0)
#coords[10, ] <- c(0,-2, FALSE, FALSE, -1, 0)
#coords[11, ] <- c(0,-3, FALSE, FALSE, -1, 0)
#coords[12, ] <- c(1,-3, FALSE, FALSE, -1, 0)
#coords[13, ] <- c(2,-3, FALSE, FALSE, -1, 0)

currentPos <- c(x=0,y=0)
lastMove <- -1
distance <- function(data){
  return(abs(currentPos[["x"]] - data[["x"]]) + abs(currentPos[["y"]] - data[["y"]]))
}
findNewPosition <- function(){
  coords <<- coords[order(apply(coords, 1,  distance)), ] # sort by distance
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
  running <<- FALSE
  print("No more tiles to discover")
  return(c(x=0,y=0)) # return default
}
makeMove <- function(target){ # bfs with checking first move direction
  q = list()
  coords$visited <<- FALSE
  coords$parent <<- -1
  # push currentPos to queue
  start <- which(coords$x==currentPos[["x"]] & coords$y==currentPos[["y"]])
  coords[start, "parent"] <<- -1
  coords[start, "dist"] <<- 0
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
        coords[nId, "dist"] <<- coords[current, "dist"] + 1
        q[[length(q) + 1]] <- nId
      }
    }
    if(!is.null(lastVisited)){
      break
    }
  }
  if(is.null(lastVisited)){
    print("Target not reached")
    return(-1)
  }
  if(coords[lastVisited, "parent"] == -1){
    return(list(direction=lastDirection, steps=1))
  }
  steps <- 2 # 1 because we start from target neighbour + 1 because we look for start's neighbour 
  while(coords[lastVisited, "parent"] != start){
    lastVisited = coords[lastVisited, "parent"]
    steps <- steps + 1
  }
  plannedDirection <- NULL
  if(coords[lastVisited, "x"] > currentPos[1]){
    plannedDirection <- 3
  }
  if(coords[lastVisited, "x"] < currentPos[1]){
    plannedDirection <- 4
  }
  if(coords[lastVisited, "y"] > currentPos[2]){
    plannedDirection <- 1
  }
  if(coords[lastVisited, "y"] < currentPos[2]){
    plannedDirection <- 2
  }
  if(is.null(plannedDirection)){
    stop("Couldnt find direciton")
  }
  return(list(direction=plannedDirection, steps=steps))
}

printGrid <- function(target){
  for(y in max(coords$y):min(coords$y)){
    for(x in min(coords$x):max(coords$x)){
      if(x==currentPos[["x"]] && y==currentPos[["y"]]){
        cat('D')
        next
      }
      if(target[["x"]]==x && target[["y"]]==y){
        cat("?")
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
          # cat(coords[row, "dist"] %% 10)
          cat(".")
          # cat(coords[row, "dist"])
        }
      }
    }
    cat("\n")
  }
  #Sys.sleep(0.3)
}