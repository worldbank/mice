# 'Parallel wrapper for mice
#
#'@export
mice_par <- function(data, m = 5, 
                 method = NULL,
                 predictorMatrix,
                 where = NULL,
                 blocks,
                 visitSequence = NULL,
                 formulas,
                 blots = NULL,
                 post = NULL,
                 defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                 maxit = 5, printFlag = TRUE, seed = NA,
                 data.init = NULL,
                 ...) {

  retVal <- mice (data, m = 5, 
                       method = NULL,
                       predictorMatrix,
                       where = NULL,
                       blocks,
                       visitSequence = NULL,
                       formulas,
                       blots = NULL,
                       post = NULL,
                       defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                       maxit = 5, printFlag = TRUE, seed = NA,
                       data.init = NULL,
                       ...)
    
  return (retVal)
  }