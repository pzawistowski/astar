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
  },
  
  is_feasible = function(node){ if(any(is.na(node))) FALSE else params$heuristic(node,NULL) == 0}
)

start <- c(2,1,3,5,4) # Our initial state
res <- astar::astar(start, params)
res$solution
```

## Solving the [15 puzzle](https://en.wikipedia.org/wiki/15_puzzle)

Below is a simple solution to the `15 puzzle` which uses an additional (read-only)
parameter which the algorithm passes to each of the functions defined in `params`.
```R
params <- list(
  # A very simple heuristic function - solving complicated cases may take a long time
  heuristic = function(node, goal){ # Note that we're passing an additional parameter `goal` here
    cost <- sapply(1:16, function(v){
      expected <- which(goal==v, arr.ind=T)
      actual <- which(node==v, arr.ind=T)
      sum(abs(expected-actual))
    })

    sum(cost)
  },

  # The distance function just counts the number of moves already made
  distance = function(node, parent, parentDistance, ...) parentDistance+1, # `...` basically says that we're ignoring the additional `goal` parameter

  # Neighbourhood function generates the moves feasible from a given board configuration
  neighbours = function(node, ...){ # `...` - same as above
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
  },
  is_feasible = function(node, goal) identical(node, goal) # we're using `goal` again
)

# Initial game state is depicted by the following matrix
start <- matrix(c( 1, 2, 3, 4,
                   5, 6, NA, 7,
                   9,10,11,8,
                   13,14,15,12 ), nrow=4, byrow=T)

# Our goal - the board state after solving the puzzle
goalNode <- matrix(c( 1, 2, 3, 4,
                  5, 6, 7, 8,
                  9,10,11,12,
                 13,14,15,NA ),nrow=4,byrow=T)

res <- astar::astar(start, params, goal = goalNode) # here we're setting the value for `goal` 

# The solution is a list of moves to make
res$solution
```


## Sorting numbers and counting heuristic function calls

If we want to pass an additional parameter, that we can modify - we can utilise a [Reference Class](http://adv-r.had.co.nz/R5.html)
object.
```R
Context <- setRefClass("Context", fields = c("count"))
    
params <- list(
  # We count the number of heuristic function calls
  heuristic = function(node, ctx){
    ctx$count <- ctx$count + 1
    len <- length(node)
    sum((node[1:(len-1)]-node[2:len])>0)
  },

  # Same as in the first example
  distance = function(node, parent, parentDistance, ...) parentDistance + 1,

  # Same as in the first example
  neighbours = function(node, ...){
    moves <- combn(length(node),2)
    lapply(1:ncol(moves), function(i){
      neighbour <- node
      neighbour[[moves[[1,i]] ]] <- node[[moves[[2,i]] ]]
      neighbour[[moves[[2,i]] ]] <- node[[moves[[1,i]] ]]
      neighbour
    })
  },
  
  # Same as in the first example
  is_feasible = function(node, ...){ if(any(is.na(node))) FALSE else params$heuristic(node,NULL) == 0}
)

start <- c(2,1,3,5,4) # Our initial state
context = Context$new(count = 0)

astar::astar(start, params, ctx = context)
context$count

```