#' Row to Column
#'
#' This function transforms a single column Matrix into a single column
#' @param m The name of the matrix that will be processed
#' @return the transformed matrix
#' RowToCol()

RowToCol <- function(m) {
  if(is.matrix(m)) {
    numCol <- ncol(m)
    numRow <- nrow(m)
    newMatrix <- matrix(nrow=numCol*numRow, ncol=1)
    idxCol <- 1
    idxRow <- 1
    idxAll <- 1
    while (idxRow <= numRow) {
      while (idxCol <= numCol) {
        if(is.na(m[idxRow,idxCol])){
          idxCol = idxCol + 1
        } else {
          newMatrix[idxAll,1] <- m[idxRow,idxCol]
          idxCol = idxCol + 1
          idxAll = idxAll + 1
        }
      }
      idxRow = idxRow + 1
      idxCol = 1
    }
    newMatrix <- na.omit(newMatrix)
  return(newMatrix)
  }
  else
  {return("This function only works on a Matrix")}
}
