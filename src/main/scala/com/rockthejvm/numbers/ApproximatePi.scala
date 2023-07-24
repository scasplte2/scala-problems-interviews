package com.rockthejvm.numbers

object ApproximatePi extends App {

  // approximate pi using Monte-Carlo algorithm
  def approximatePi(n: Int): Double = {
    val random = new scala.util.Random(System.currentTimeMillis())
    val radii = (0 to n)
      .map(_ => (random.between(-1: Double, 1: Double), random.between(-1: Double, 1: Double)))
      .map { case (x,y) => Math.pow(x, 2) + Math.pow(y, 2) }

    val insideNumber: Double = radii.count(_ <= (1: Double))
    val totalNumber: Double = radii.length

    (4: Double) * insideNumber / totalNumber
  }

  println(approximatePi(5))
  println(approximatePi(50))
  println(approximatePi(500))
  println(approximatePi(5000))
  println(approximatePi(50000))
  println(approximatePi(500000))
  println(approximatePi(5000000))
}
