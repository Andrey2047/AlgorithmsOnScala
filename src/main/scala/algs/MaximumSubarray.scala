package algs

object MaximumSubarray {

  def maxSubArray(inputArray: Vector[Int]): Unit = {

    var max_sum_ending_here = inputArray(0)
    var max_sum_so_far = inputArray(0)

    for (i <- 1 until inputArray.size) {
      max_sum_ending_here = Math.max(inputArray(i), max_sum_ending_here + inputArray(i))
      max_sum_so_far = Math.max(max_sum_ending_here, max_sum_so_far)
    }

    println(max_sum_so_far)
  }

  def main(args: Array[String]): Unit = {
    maxSubArray(Vector(-1, 2, 3, -4, 5, 10))
  }


}
