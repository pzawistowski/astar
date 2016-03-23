# astar 
[![Travis-CI Build Status](https://travis-ci.org/pzawistowski/astar.svg?branch=master)](https://travis-ci.org/pzawistowski/astar)

A* algorithm implementation.


# Installing the package

The easiest way is to use devtools like this:

```R
install.packages("devtools")
devtools::install_github("pzawistowski/astar")
```

# Sample usage

## Solving the [15 puzzle](https://en.wikipedia.org/wiki/15_puzzle)

```R
params <- list(
  # A very simple heuristic function - solving complicated cases may take a long time
  heuristic = function(node, goal){
    cost <- sapply(1:15, function(v){
      expected <- which(goal==v, arr.ind=T)
      actual <- which(node==v, arr.ind=T)
      sum(abs(expected-actual))
    })

    sum(cost)
  },

  # The distance function just counts the number of moves already made
  distance = function(node, parent, parentDistance) parentDistance+1,

  # Neighbourhood function generates the moves feasible from a given board configuration
  neighbours = function(node){
    rc <- which(is.na(node), arr.ind = T)
    row <- rc[[1]]
    col <- rc[[2]]

    res <- list()
    addMove <- function(otherRow, otherCol){
      n <- node
      n[[row,col]] <- n[[otherRow,otherCol]]
      n[[otherRow,otherCol]] <- NA
      res <<- append(res, list(n))
    }

    if(row > 1){ addMove(row-1,col) }
    if(row < 4){ addMove(row+1,col) }
    if(col > 1){ addMove(row,col-1) }
    if(col < 4){ addMove(row,col+1) }

    res
  }
)

# Initial game state is depicted by the following matrix
start <- matrix(c( 1, 2, 3, 4,
                   5, 6, NA, 7,
                   9,10,11,8,
                   13,14,15,12 ), nrow=4, byrow=T)

# Our goal - the board state after solving the puzzle
goal <- matrix(c( 1, 2, 3, 4,
                  5, 6, 7, 8,
                  9,10,11,12,
                 13,14,15,NA ),nrow=4,byrow=T)

res <- astar::astar(start, goal, params)

# The solution is a list of moves to make
res$solution
```

## Sorting numbers
```R
params <- list(
  # A simple bubble-sort-like heuristic
  heuristic = function(node, goal){
    len <- length(node)
    sum((node[1:(len-1)]-node[2:len])>0)
  },

  # We just count the number of "moves" made
  distance = function(node, parent, parentDistance) parentDistance + 1,

  # The possible "moves" - we substitute only two numbers
  neighbours = function(node){
    moves <- combn(length(node),2)
    lapply(1:ncol(moves), function(i){
      neighbour <- node
      neighbour[[moves[[1,i]] ]] <- node[[moves[[2,i]] ]]
      neighbour[[moves[[2,i]] ]] <- node[[moves[[1,i]] ]]
      neighbour
    })
  }
)

start <- c(2,1,3,5,4) # Our initial state
# Our "goal" is defined using a function
isSorted <- function(node){ if(any(is.na(node))) FALSE else params$heuristic(node,NULL) == 0}
res <- astar::astar(start, isSorted, params)
res$solution
```
