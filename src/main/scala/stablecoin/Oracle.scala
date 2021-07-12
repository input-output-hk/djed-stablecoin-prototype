package stablecoin

import stablecoin.Currency.Currency

import scala.collection.mutable.{Map => MMap}

class Oracle {
  private val m = MMap[(Currency,Currency), N]()

  def conversionRate(from: Currency, to: Currency): N = m((from, to))

  def updateConversionRate(from: Currency, to: Currency, rate: N) = { m((from, to)) = rate }
}