
#' Runs the A* algorithm.
#'
#'
#' The search space is defined by a list passed thorugh params.
#'
#' @param start initial node in the search space
#' @param params a list containing functions necessary to define the search problem: \code{heuristic}, \code{distance}, \code{neighbours} and \code{is_feasible} (see example below)
#' @param max.iters maximum number of iterations to run - for debuggin purposes only, defaults to \code{Inf}
#' @param ... additional parameters passed to each of the functions defined in \code{params}
#' @return A list containing two components \code{solution} and \code{history}.
#' @examples
#'
#' # Searching for a path from (0,0), to (0,3) with three points blocked along the way
#'
#' dirs <- list(c(1,0),c(0,1),c(0,-1),c(-1,0))
#' params <- list(
#'  heuristic = function(el, goal) sum((goal-el)^2),
#'  distance = function(el, parent, parentDistance, ...) parentDistance + 1,
#'  neighbours = function(el, ...) setdiff( lapply(dirs, function(d) el+d), 
#'                                          list(c(0,2), c(1,1), c(-1,1))),
#'  is_feasible = function(node, goal){ identical(node, goal) }
#'  )
#'
#'  res <- astar(c(0,0), params, goal = c(0,3))
#'
#' @export
astar <- function(start, params, max.iters = Inf, ...){
  
  visitedNodes <- VisitedNodesSet$new(50)
  history <- list()
  it <- 0

  backtrack <- function(el) {
    if(is.na(el$parent)){
      list()
    }else{
      parent <- history[[el$parent]]
      append(backtrack(parent),list(parent$node))
    }
  }

  updateHistory <- function(el){
    visitedNodes$add(el$node)
    history <<- append(history, list(el))
  }

  stopCriterion <- function(A){
    it <<- it + 1
    A$length() == 0 || (visitedNodes$count() > 0 && params$is_feasible(visitedNodes$last(),...)) || it > max.iters
  }
  
  with(params,
   {
     A <- NaivePriorityQueue$new()
     h <- heuristic(start, ...)
     A$push(list(node=start, parent=NA, distance=0, heuristic=h),h)

     while(!stopCriterion(A)){
        curr <- A$pop()
        updateHistory(curr)

        for(n in visitedNodes$filterNeighbours(neighbours(curr$node, ...))){
          d <- distance(n,curr$node, curr$distance, ...)
          h <- heuristic(n, ...)
          filter <- function(el) identical(el$node, n)
          prev <- A$find(filter)

          if(is.na(prev)){
            A$push(list(node=n,parent=it, distance=d, heuristic=h), h+d)
          }else{
             other <- A$get(prev)
             if(other$distance>d){
               A$remove(prev)
               A$push(list(node=n,parent=it, distance=d, heuristic=h), h+d)
             }
          }
        }
     }
   })

  last <- history[[it-1]]
  list(
    solution=append(backtrack(last),list(last$node)),
    solution.cost = last$distance,
    history=history
  )
}

