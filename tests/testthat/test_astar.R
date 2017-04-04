context("astar")

test_that("astar is able to find an integer route from 0 to 10", {
  params <- list(
    heuristic = function(el, goal) (goal - el)^2,
    distance = function(el, parent, parentDistance, goal) el^2,
    neighbours = function(el, goal) list(el + 1, el - 1),
    is_feasible = function(node, goal){identical(node, goal) }
  )

  res <- astar(0, params, goal = 10)

  expect_equal(res$solution, as.list(0:10))
})



test_that("astar is able to find an integer route from (0,0) to (0,3) despite a speed bump", {
  dirs <- list(c(1,0),c(0,1),c(0,-1),c(-1,0))

  params <- list(
    heuristic = function(el, goal) sum((goal-el)^2),
    distance = function(el, parent, parentDistance, goal) parentDistance + 1,
    neighbours = function(el, goal) setdiff(lapply(dirs, function(d) el+d), list(c(0,2))),
    is_feasible = function(node, goal){ identical(node, goal) }
  )

  res <- astar(c(0,0), params, goal=c(0,3))
  expect_equal(tail(res$solution,1), list(c(0,3)))
  expect_equal(length(res$solution), 6)
})



test_that("astar is able to backtrack", {
  dirs <- list(c(1,0),c(0,1),c(0,-1),c(-1,0))

  params <- list(
    heuristic = function(el, goal) sum((goal-el)^2),
    distance = function(el, parent, parentDistance, goal) parentDistance + 1,
    neighbours = function(el, goal) setdiff(lapply(dirs, function(d) el+d), list(c(0,2), c(1,1), c(-1,1))),
    is_feasible = function(node, goal){ identical(node, goal) }
  )

  res <- astar(c(0,0), params, goal=c(0,3))
  with(res,{
    expect_equal(tail(solution,1), list(c(0,3)))
    expect_equal(length(solution), 8)

    expect_equal(history[[1]]$node, c(0,0))
    expect_equal(tail(history,1)[[1]]$node, c(0,3))
    expect_equal(history[[2]]$node, c(0,1))
  })
})

test_that("astar is able to solve a 100D problem", {
  n <- 100
  params <- list(
    heuristic = function(el, goal) sum((goal - el)^2),
    distance = function(el, parent, parentDistance, goal) parentDistance + 1,
    neighbours = function(el, goal) {
      v <- function(i) {zeros <- rep(0, n); zeros[[i]] <- 1}
      dims <- as.list(1:n)
      c(lapply(dims, function(i){ el + v(i)}), lapply(dims, function(i){el - v(i)}))
    },
    is_feasible = function(node, goal){ identical(node, goal) }
  )

  res <- astar(rep(0,n), params, goal = rep(10,n))

  expect_equal(tail(res$solution,1), list(rep(10,n)))
})

test_that("astar handles multiple paths to the same node", {
  dist <- matrix(nrow=4, ncol=4, byrow=TRUE,
                 data = c(0, 2, 10, Inf,
                          2, 0, 3, Inf,
                          10,3, 0, 4,
                          Inf, Inf, 4, 0))
  params <- list(
    heuristic = function(el, goal) 5*(goal - el),
    distance = function(el, parent, parentDist, goal) parentDist + dist[[parent, el]],
    neighbours = function(el, goal) {
      row <- dist[el,]
      as.list(which(row > 0 & row < Inf))
    },
    is_feasible = function(node, goal){ identical(node,goal)}
  )

  res <- astar(1, params, goal = 4)

  expect_equal(res$solution, as.list(1:4))
  expect_equal(res$solution.cost, 9)
})

test_that("is't possible to pass arbitrary parameters to user defined functions", {
  got_foo <- NA
  got_bar <- NA
  got_both <- NA
  
  params <- list(
    heuristic = function(el, foo, ...) {got_foo <<- foo; (10 - el)^2},
    distance = function(el, parent, parentDistance, bar, ...) {got_bar <<- bar; el^2},
    neighbours = function(el, foo, bar) {got_both <<- foo + bar; list(el + 1, el - 1)},
    is_feasible = function(node, foo, bar){ identical(node, 10) }
  )
  
  astar(0, params, foo = 1, bar = 2)

  expect_equal(got_foo, 1)
  expect_equal(got_bar, 2)
  expect_equal(got_both, 3)
})



test_that("there is no NA call to feasibility function during the first iteration", {
  
  params <- list(
    heuristic = function(node, ctx){
      len <- length(node)
      sum((node[1:(len-1)]-node[2:len])>0)
    },
    distance = function(node, parent, parentDistance, ...) parentDistance + 1,
    
    neighbours = function(node, ...){
      moves <- combn(length(node),2)
      lapply(1:ncol(moves), function(i){
        neighbour <- node
        neighbour[[moves[[1,i]] ]] <- node[[moves[[2,i]] ]]
        neighbour[[moves[[2,i]] ]] <- node[[moves[[1,i]] ]]
        neighbour
      })
    },
    is_feasible = function(node, ...) params$heuristic(node,NULL) == 0
  )
  
  start <- c(2,1,3,5,4) 
  
  res = astar::astar(start, params)
  expect_equal(tail(res$solution,1)[[1]], 1:5)
})



