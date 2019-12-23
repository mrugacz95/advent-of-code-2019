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
input <- function(){
  return(as.integer(readline(prompt="Provide input: ")))
}
output <- function(value){
  cat("Output: ", value, "\n")
}
run(input, 
    output)
getMode(2)