context("examples")

test_that("15 puzzle heuristic counts only misplaced tiles", {
  heuristic <- function(node, goal){ 
    mask <- (goal != 0)
    sum((node != goal) & mask)
  }
  
  goal <- c(1,2,3,0)
  
  expect_equal(heuristic(goal, goal), 0)
  expect_equal(heuristic(c(1,2,0,3), goal), 1)
  expect_equal(heuristic(c(1,3,2,0), goal), 2)
  expect_equal(heuristic(c(2,3,1,0), goal), 3)
})
