
VisitedNodesSet <- R6::R6Class(
  "VisitedNodesSet",
  public = list(
    initialize = function(shardSize) {
      private$shardSize <- shardSize
    },
    add = function(node) {
      if(private$counter > private$shardSize){
        private$current <- private$current + 1
        private$shards[[private$current]] <- list()
        private$counter <- 0
      }

      private$lastNode <- node
      private$shards[[private$current]] <- append(private$shards[[private$current]], list(node))
      private$counter <- private$counter + 1
    },

    filterNeighbours = function(neighbours){
      idx <- private$current
      while(idx >= 1 && length(neighbours) > 0){
        mask <- if(length(neighbours) > 0) fastmatch::fmatch(neighbours, private$shards[[idx]], 0L) == 0L else c()
        neighbours <- neighbours[mask]
        idx <- idx - 1
      }

      neighbours
    },

    last = function(){
      private$lastNode
    }
  ),
  private = list(
    shards = list(list()),
    lastNode = NA,
    current = 1,
    counter = 0,
    shardSize = 10
  )
)
