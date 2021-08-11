package simulator.scenarios

import com.typesafe.scalalogging.LazyLogging
import simulator.{BullMarketEnvironment, Environment, Player, Simulator, StablecoinBuyer, StablecoinSeller}
import stablecoin.Currency.{BaseCoin, PegCurrency}
import stablecoin.{Address, BuyStablecoinTransaction, Ledger, N, Oracle, SellReservecoinTransaction, SellStablecoinTransaction, MinimalDjedStablecoin, Transaction}

object MarketCrashGame {
  val initStablecoinAccounts: Map[Address, N] = (for (i <- 0 until 1000) yield i -> BigDecimal(1000)).toMap
  val initReservecoinsAccounts: Map[Address, N] = (for (i <- 0 until 100) yield i -> BigDecimal(100)).toMap

  val bankAddress = 0x999;
  val bankFee = 0.01
  val minReserveRatio = 1.5
  val maxReserveRatio = 4
  val reservecoinDefaultPrice = 0.2

  val oracle = new Oracle
  oracle.updateConversionRate(PegCurrency, BaseCoin, 0.2) // 1 BaseCoin = 5 USD (PegCurrency)

  val contract = new MinimalDjedStablecoin(
    oracle,
    bankFee,
    minReserveRatio,
    maxReserveRatio,
    reservecoinDefaultPrice,
    400000.0,
    1000000.0,
    10000.0)

  def main(args: Array[String]): Unit = {
    val ledger = new Ledger(Map(), initStablecoinAccounts, initReservecoinsAccounts, contract)
    val scPanicSellers = for (i <- 0 until 100) yield (new StablecoinPanicSeller(i)) // 10% of stablecoin holders are panicsellers
    val rcPanicSellers = for (i <- 0 until 20) yield (new ReserveCoinsPanicSeller(i)) // 20% of reservecoin holders are panicsellers
    val players = scPanicSellers ++ rcPanicSellers

    new Simulator(ledger, new MarketCrashEnvironment, players, 26).run()
  }
}

class MarketCrashEnvironment extends Environment with LazyLogging {

  override def newRoundCallback(ledger: Ledger, round: Address): Ledger = {
    val oldRate = ledger.stablecoinContract.oracle.conversionRate(PegCurrency, BaseCoin)
    val newRate = round match {
      case r if r > 10 && r < 20 => oldRate * 1.01 // from round 10 bear market started with price depreciation 1% each round
      case r if r == 20 => oldRate * 1.5  // market crash 50% in one round
      case r if r > 20 & r < 25 => oldRate * 0.95  // price recovery in the following 5 days
      case _ => oldRate
    }

    val contract = ledger.stablecoinContract
    contract.oracle.updateConversionRate(PegCurrency, BaseCoin, newRate)
    logger.trace("Updated conversion rate: " + PegCurrency + " -> " + BaseCoin + ": " + oldRate + "->" + newRate)

    val ratio = contract.getReservesAmount / (contract.getStablecoinsAmount * contract.oracle.conversionRate(PegCurrency, BaseCoin))
    if (ratio < 1.5) logger.warn("Resrves ratio is below minimal limit!")
    if (ratio < 1.0) logger.warn("CRITICAL: Resrves are below liabilities!")
    ledger
  }
}


class StablecoinPanicSeller(val address: Address) extends Player with LazyLogging {
  private var prevPrice: N = 0

  override def newRoundCallback(ledger: Ledger, round: Int): Iterable[Transaction] = {
    val myStablecoins = ledger.getStablecoinAccounts(address)
    val currentPrice = ledger.stablecoinContract.oracle.conversionRate(PegCurrency,BaseCoin)

    val txs = if (prevPrice != 0 && (prevPrice / currentPrice) < 0.9) {
      logger.info("Price is dropping 10%! Stablecoin holder panic, selling all stablecoins: " + myStablecoins + " SC")
      List(SellStablecoinTransaction(address, myStablecoins))
    } else List()

    prevPrice = currentPrice
    txs
  }
}

class ReserveCoinsPanicSeller(val address: Address) extends Player with LazyLogging {
  private var prevPrice: N = 0

  override def newRoundCallback(ledger: Ledger, round: Int): Iterable[Transaction] = {
    val myReservecoins = ledger.getReservecoinAccounts(address)
    val currentPrice = ledger.stablecoinContract.reservecoinNominalPrice()

    val txs = if (prevPrice != 0 && (currentPrice / prevPrice) < 0.9) {
      logger.info("Reservecoin price is dropping 10%! Selling all reservecoins: " + myReservecoins + " RC")
      List(SellReservecoinTransaction(address, myReservecoins))
    } else List()

    prevPrice = currentPrice
    txs
  }
}