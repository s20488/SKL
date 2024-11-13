object Functions {

  def compose[A, B, C](f: A => B)(g: B => C): A => C = {
    (x: A) => g(f(x))
  }

  def prod[A, B, C, D](f: A => C, g: B => D): (A, B) => (C, D) = {
    (x: A, y: B) => (f(x), g(y))
  }

  def lift[A, B, T](op: (T, T) => T)(f: A => T, g: B => T): (A, B) => T = {
    (x: A, y: B) => op(f(x), g(y))
  }

  type MSet[A] = A => Int

  def sum[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {
    (x: A) => s1(x) + s2(x)
  }

  def diff[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {
    (x: A) => (s1(x) - s2(x)).max(0)
  }

  def mult[A](s1: MSet[A], s2: MSet[A]): MSet[A] = {
    (x: A) => math.min(s1(x), s2(x))
  }
}

@main
def mainFunctions(): Unit = {
  val f1: Int => Int = _ + 1
  val f2: Int => Int = _ * 2
  val op: (Int, Int) => Int = _ + _

  val composed = Functions.compose(f1)(f2)
  println(composed(3))

  val prodFunc = Functions.prod(f1, f2)
  println(prodFunc(3, 4))

  val lifted = Functions.lift(op)(f1, f2)
  println(lifted(3, 4))

  val s1: Functions.MSet[Int] = (n: Int) => n match {
    case 1 => 2
    case 3 => 1
    case _ => 0
  }

  val s2: Functions.MSet[Int] = (n: Int) => n match {
    case 1 => 1
    case 2 => 3
    case _ => 0
  }

  val summed = Functions.sum(s1, s2)
  println(summed(1))

  val diffed = Functions.diff(s1, s2)
  println(diffed(1))

  val multiplied = Functions.mult(s1, s2)
  println(multiplied(1))
}
