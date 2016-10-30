def sumSegment(a: Array[Int], p: Double, s: Int, t: Int): Int = {

  var total = 0
  for (i <- s to t - 1) {
    total += power(a(i), p)
  }
  total
}

def power(x: Int, p: Double): Int =
  math.exp(p * math.log(math.abs(x))).toInt

def pNorm(a: Array[Int], p: Double): Int =
  power(sumSegment(a, p, 0, a.length), 1/p)

def pNormTwoPart(a: Array[Int], p: Double): Int = {
  val m = a.length / 2
  val (sum1, sum2) = parallel(sumSegment(a, p, 0, m),
    sumSegment(a, p, m, a.length))
  power(sum1 + sum2, p)
}

def segmentRec(a: Array[Int], p: Double, s: Int, t: Int): Int = {
  if(t - s < threshold)
    sumSegment(a, p, s, t)
  else {
    val m = s + (t - s) / 2
    val (sum1, sum2) = parallel(segmentRec(a, p, s, m),
      segmentRec(a, p, m, t))
    sum1 + sum2
  }
}

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {

}

//Monte Carlo Method

import scala.util.Random

def mcCount(iter: Int): Int = {
  val randomX = new Random
  val randomY = new Random
  var hits = 0
  for(i <- 0 until iter){
    val x = randomX.nextDouble
    val y = randomY.nextDouble
    if(x * x + y * y < 1) hits = hits + 1
  }
  hits
}

def monteCarloPiSeq(iter: Int): Double = 4.0 * mcCount(iter) /iter

def monteCarloPiPar(iter: Int): Double = {
  val ((pi1, pi2), (pi3, pi4)) = parallel(
    parallel(mcCount(iter/4), mcCount(iter/4)),
    parallel(mcCount(iter/4), mcCount(iter/4))
  )
}

def task(c: => A): Task[A]

trait Task[A]{
  def join: A
}

def parallel[A, B](cA: => A, cB => B): (A, B) = {
  val tB: Task[B] = task { cB }
  val tA: A = cA
  (tA, tB.join)
}