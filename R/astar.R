
#' Runs the A* algorithm.
#'
#'
#' The search space is defined by a list passed thorugh params.
#'
#' @param start initial node in the search space
#' @param goal final node in the search space
#' @param params a list containing functions necessary to define the search space
#' @param max.iters maximum number of iterations to run - for debuggin purposes only, defaults to \code{Inf}
#' @return A list containing two components \code{solution} and \code{history}.
#' @examples
#'
#' # Searching for a path from (0,0), to (0,3) with three points blocked along the way
#'
#' dirs <- list(c(1,0),c(0,1),c(0,-1),c(-1,0))
#' params <- list(
#'  heuristic = function(el, goal) sum((goal-el)^2),
#'  distance = function(el, partialSolution) length(partialSolution),
#'  neighbours = function(el) setdiff(lapply(dirs, function(d) el+d), list(c(0,2), c(1,1), c(-1,1)))
#'  )
#'
#'  res <- astar(c(0,0), c(0,3), params)
#'
#' @export
astar <- function(start, goal, params, max.iters = Inf){
  history <- list()
  parents <- list()
  it <- 0

  backtrack <- function(el) {
    if(is.na(el$parent)){
      list()
    }else{
      parent <- parents[[el$parent]]
      append(backtrack(parent),list(parent$el))
    }
  }

  stopCriterion <- function(A, history){
    it <<- it + 1
    length(A) == 0 || list(goal) %in% tail(history,1) || it > max.iters
  }

  with(params,
   {

    evaluate <- function(el,sol) heuristic(el,goal) + distance(el,sol)

     A <- NaivePriorityQueue$new()
     A$push(list(el=start, parent=NA), evaluate(start, list()) )

     while(!stopCriterion(A, history)){
        curr <- A$pop()
        history <<- append(history, list(curr$el))
        parents <<- append(parents, list(curr))

        sol <- c(backtrack(curr), curr)
        for(n in setdiff(neighbours(curr$el), history))  A$push(list(el=n,parent=it), evaluate(n, sol))
     }
   })

  last <- tail(parents,1)[[1]]
  list(
    solution=append(backtrack(last),list(last$el)),
    history=history
  )
}

