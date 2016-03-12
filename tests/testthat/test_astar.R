context("astar")

test_that("astar is able to find an integer route from 0 to 10", {
  params <- list(
    heuristic = function(el, goal) (goal - el)^2,
    distance = function(el, partialSolution) el^2,
    neighbours = function(el) list(el + 1, el - 1)
  )

  res <- astar(0, 10, params)

  expect_equal(res$solution, as.list(0:10))
})



test_that("astar is able to find an integer route from (0,0) to (0,3) despite a speed bump", {
  dirs <- list(c(1,0),c(0,1),c(0,-1),c(-1,0))

  params <- list(
    heuristic = function(el, goal) sum((goal-el)^2),
    distance = function(el, partialSolution) length(partialSolution),
    neighbours = function(el) setdiff(lapply(dirs, function(d) el+d), list(c(0,2)))
  )

  res <- astar(c(0,0), c(0,3), params)
  expect_equal(tail(res$solution,1), list(c(0,3)))
  expect_equal(length(res$solution), 6)
})



test_that("astar is able to backtrack", {
  dirs <- list(c(1,0),c(0,1),c(0,-1),c(-1,0))

  params <- list(
    heuristic = function(el, goal) sum((goal-el)^2),
    distance = function(el, partialSolution) length(partialSolution),
    neighbours = function(el) setdiff(lapply(dirs, function(d) el+d), list(c(0,2), c(1,1), c(-1,1)))
  )

  res <- astar(c(0,0), c(0,3), params)
  with(res,{
    expect_equal(tail(solution,1), list(c(0,3)))
    expect_equal(length(solution), 8)

    expect_equal(head(history,1), list(c(0,0)))
    expect_equal(tail(history,1), list(c(0,3)))
    expect_equal(history[[2]], c(0,1))
  })
})

test_that("astar is able to solve a 100D problem", {
  n <- 100
  params <- list(
    heuristic = function(el, goal) sum((goal - el)^2),
    distance = function(el, partialSolution) length(partialSolution),
    neighbours = function(el) {
      v <- function(i) {zeros <- rep(0, n); zeros[[i]] <- 1}
      dims <- as.list(1:n)
      c(lapply(dims, function(i){ el + v(i)}), lapply(dims, function(i){el - v(i)}))
    }
  )

  res <- astar(rep(0,n), rep(10,n), params)

  expect_equal(tail(res$solution,1), list(rep(10,n)))
})


