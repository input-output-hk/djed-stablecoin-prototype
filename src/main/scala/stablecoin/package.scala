package object stablecoin {

  type Address = Int
  type N = BigDecimal

  def roundAt(d: BigDecimal, at: Int) = d.setScale(at, BigDecimal.RoundingMode.DOWN)
  def min(x: N, y: N) = if (x < y) x else y
  def max(x: N, y: N) = if (x > y) x else y
  def abs(x: N) = if (x >= 0) x else -x

  object Currency extends Enumeration {
    type Currency = Value

    val PegCurrency = Value("PegCurrency")
    val BaseCoin = Value("BaseCoin")
    val StableCoin = Value("StableCoin")
    val ReserveCoin = Value("ReserveCoin")
  }
}
