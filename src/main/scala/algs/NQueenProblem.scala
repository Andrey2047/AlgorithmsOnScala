package algs

object NQueenProblem {

  type Board = Array[Array[Int]]

  def locateQueens(boardSize: Int): (Boolean, Board) = {

    def findValidArrangement(rowNumber: Int, columnNumber: Int, matrixState: Board, queens: Int): (Boolean, Board) = {
      if(queens == boardSize) {
        (true, matrixState)
      }else {
        val newState = matrixState.map(_.clone())
        val validRow = findValidRow(columnNumber, rowNumber, newState)
        validRow match {
          case vr if vr == -1 => (false, matrixState)
          case _ =>
            newState(validRow)(columnNumber) = 1
            val (correctPath, rState) = findValidArrangement(0, columnNumber + 1, newState, queens + 1)
            if (correctPath) {
              (correctPath, rState)
            } else if (validRow < boardSize - 1) {
              findValidArrangement(validRow + 1, columnNumber, matrixState, queens)
            } else {
              (false, matrixState)
            }
        }
    }
    }

    def findValidRow(columnNumber: Int, row: Int, state: Board): Int = {
      val newState = state.map(_.clone())
      newState(row)(columnNumber) = 1
      if (isArrangementValid(newState)) {
        row
      } else if (row < boardSize - 1) {
        findValidRow(columnNumber, row + 1, state)
      } else {
        -1
      }
    }

    def isArrangementValid(state: Board): Boolean = {
      var tt = true
      for (i <- 0 until boardSize) {
        val s = (1 until boardSize).map(k => state(k)(i)).sum
        if (s > 1) {
          tt = false
        }
      }

      var tt2 = true
      for (i <- 0 until boardSize) {
        for (j <- 0 until boardSize) {
          if (state(i)(j) == 1) {
            if (!diagonalSumValid(i, j, state)) {
              tt2 = false
            }
          }
        }
      }
      !state.exists(_.sum > 1) && tt && tt2
    }

    def diagonalSumValid(row: Int, column: Int, state: Board): Boolean = {
      val upOffset = Math.min(row, column)
      val downOffset = Math.min(boardSize - row, boardSize - column)
      val s1 = (0 to upOffset).map(off => state(row - off)(column - off)).sum
      val s2 = (1 until downOffset).map(off => state(row + off)(column + off)).sum

      val upOffset2 = Math.min(row, boardSize - column - 1)
      val downOffset2 = Math.min(boardSize - row, column)
      val s3 = (0 to upOffset2).map(off => state(row - off)(column + off)).sum
      val s4 = (1 until downOffset2).map(off => state(row + off)(column - off)).sum
      (s1 + s2) < 2 && (s3 + s4) < 2
    }

    val initialMatrix = Array.ofDim[Int](boardSize, boardSize)

    for (i <- 0 until boardSize) {
      for (j <- 0 until boardSize) {
        initialMatrix(i)(j) = 0
      }
    }

    findValidArrangement(0, 0, initialMatrix, 0)
  }

  def main(args: Array[String]): Unit = {
    printBoard(locateQueens(16)._2)
  }

  def printBoard(board: Board) = {
    for (i <- board.indices) {
      println(board(i).mkString(" "))
    }
  }

}
