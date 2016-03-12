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

```R
dirs <- list(c(1,0),c(0,1),c(0,-1),c(-1,0))

params <- list(
  heuristic = function(el, goal) sum((goal-el)^2),
  distance = function(el, partialSolution) length(partialSolution),
  neighbours = function(el) setdiff(lapply(dirs, function(d) el+d), list(c(0,2), c(1,1), c(-1,1)))
)

res <- astar(c(0,0), c(0,3), params)
```
