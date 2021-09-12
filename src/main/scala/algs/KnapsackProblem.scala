package algs

object KnapsackProblem {

  //brute force knap sack algorithm
  def bfKnapSack(capacity: Int, weights: Vector[Int], profit: Vector[Int], number: Int): Int = {
    if (capacity == 0 | number == 0) {
      0
    } else if (weights(number) > capacity) {
      bfKnapSack(capacity, weights, profit, number - 1)
    } else {
      val profitWithNElem = profit(number) + bfKnapSack(capacity - weights(number), weights, profit, number - 1)
      val profitWithoutNElem = bfKnapSack(capacity, weights, profit, number - 1)
      Math.max(profitWithNElem, profitWithoutNElem)
    }
  }

  //dynamic programming knap sack algorithm
  def dpKnapSack(capacity: Int, weights: Vector[Int], profit: Vector[Int]): Int = {
    val d = Array.ofDim[Int](weights.size + 1, capacity + 1)

    for (i <- 0 to capacity) {
      d(0)(i) = 0
    }

    for (j <- 0 to weights.size) {
      d(j)(0) = 0
    }

    for (i <- 1 to weights.size) {
      for (j <- 1 to capacity) {
        if (weights(i-1) > j) {
          d(i)(j) = d(i)(j - 1)
        } else {
          d(i)(j) = Math.max(profit(i - 1) + d(i - 1)(j - weights(i-1)), d(i)(j - 1))
        }
      }
    }

    d(weights.size)(capacity)

  }

  def printMatrix(w: Int, d: Array[Array[Int]]) = {
    for (i <- 0 to w) {
      println(d(i).mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    val weights = Vector(10, 20, 30)
    val profit = Vector(60,100,120)
    val capacity = 50
    println(dpKnapSack(capacity, weights, profit))
  }

}
