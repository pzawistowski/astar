
NaivePriorityQueue <- R6::R6Class(
  "NaivePriorityQueue",
  public = list(
    initialize = function() {
    },
    push = function(element, value) {
      idx <- private$findIdx(value)

      private$elements <- append(private$elements, list(element), after = idx)
      private$values <- append(private$values, value, after = idx)
    },

    pop = function() {
      if (length(private$elements) > 0) {
        element <- private$elements[[1]]
        private$elements <- private$elements[-1]
        private$values <- private$values[-1]

        element
      }else
        NA
    },
    length = function() length(private$elements),
    find = function(filter) {
      found <- sapply(private$elements, filter)
      res <- if(length(found) > 0) which(found) else c()
      if(length(res) > 0) res[[1]] else NA
    },

    get = function(idx) private$elements[[idx]],
    getValue = function(idx) private$values[[idx]],
    remove = function(idx) {
      private$elements[[idx]] <- NULL
      private$values[[idx]] <- NULL
    }
  ),
  private = list(
    elements = list(),
    values  = list(),

    findIdx = function(value) {
      idx <- 1
      count <- length(private$elements)
      while (idx <= count &&
             private$values[[idx]] <= value) {
        idx <- idx + 1
      }

      idx - 1
    }
  )

)
