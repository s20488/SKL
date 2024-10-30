class C(val re: Double, val im: Double) extends Ordered[C] {

  def this(re: Double) = this(re, 0.0)

  override def toString: String = {
    val reFormatted = f"$re%.2f"
    val imFormatted = f"$im%.2f"
    if (im > 0) s"$reFormatted + ${imFormatted}i"
    else if (im < 0) s"$reFormatted - ${-im}i"
    else reFormatted
  }

  def +(that: C): C = new C(this.re + that.re, this.im + that.im)
  def -(that: C): C = new C(this.re - that.re, this.im - that.im)
  def *(that: C): C = new C(this.re * that.re - this.im * that.im,
    this.re * that.im + this.im * that.re)
  def /(that: C): C = {
    if (that.re == 0 && that.im == 0) {
      throw new IllegalArgumentException("Dzielenie przez zero")
    }
    val denominator = that.re * that.re + that.im * that.im
    new C((this.re * that.re + this.im * that.im) / denominator,
      (this.im * that.re - this.re * that.im) / denominator)
  }

  def +(that: Double): C = new C(this.re + that, this.im)
  def -(that: Double): C = new C(this.re - that, this.im)
  def *(that: Double): C = new C(this.re * that, this.im)
  def /(that: Double): C = {
    if (that == 0) {
      throw new IllegalArgumentException("Dzielenie przez zero")
    }
    new C(this.re / that, this.im / that)
  }

  override def compare(that: C): Int = {
    val thisDistance = math.sqrt(this.re * this.re + this.im * this.im)
    val thatDistance = math.sqrt(that.re * that.re + that.im * that.im)
    thisDistance.compare(thatDistance)
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: C => this.re == that.re && this.im == that.im
    case _ => false
  }

  override def hashCode(): Int = {
    (re, im).hashCode()
  }

  override def <(that: C): Boolean = this.compare(that) < 0
  override def >(that: C): Boolean = this.compare(that) > 0
  override def <=(that: C): Boolean = this.compare(that) <= 0
  override def >=(that: C): Boolean = this.compare(that) >= 0
  def !=(that: C): Boolean = !this.equals(that)
}

object zadanie_2 extends App {
  val c1 = new C(2.1, 3.5)
  val c2 = new C(2.2, 3.4)

  println(s"c1: $c1")
  println(s"c2: $c2")
  println(s"c1 + c2 = ${c1 + c2}")
  println(s"c1 - c2 = ${c1 - c2}")
  println(s"c1 * c2 = ${c1 * c2}")

  try {
    println(s"c1 / c2 = ${c1 / c2}")
  } catch {
    case e: IllegalArgumentException => println(e.getMessage)
  }

  println(s"c1 + 5.3 = ${c1 + 5.3}")
  println(s"c2 * 2.5 = ${c2 * 2.5}")

  println(s"c1 < c2: ${c1 < c2}")
  println(s"c1 > c2: ${c1 > c2}")
}
