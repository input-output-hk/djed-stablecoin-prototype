package simulator.scenarios

import simulator.{BullMarketEnvironment, Simulator, StablecoinBuyer, StablecoinSeller}
import stablecoin.Currency.{BaseCoin, PegCurrency}
import stablecoin._

object SimpleGame {

  val initBasecoinAccounts = Map[Address, N](0x1 -> 10, 0x2 -> 10)
  val initStablecoinAccounts = Map[Address, N](0x2 -> 20)

  val bankAddress = 0x999;
  val bankFee = 0.01
  val minReserveRatio = 1.5
  val maxReserveRatio = 4
  val reservecoinDefaultPrice = 0.5

  val oracle = new Oracle
  oracle.updateConversionRate(PegCurrency, BaseCoin, 0.2) // 1 BaseCoin = 5 USD (PegCurrency)

  val contract = new MinimalDjedStablecoin(
    bankAddress,
    oracle,
    bankFee,
    minReserveRatio,
    maxReserveRatio,
    reservecoinDefaultPrice,
    20,
    60,
    5)

  def main(args: Array[String]): Unit = {
    val ledger = new Ledger(initBasecoinAccounts, initStablecoinAccounts, Map(), contract)
    val players = List(
      new StablecoinBuyer(0x1),
      new StablecoinSeller(0x2))
    val env = new BullMarketEnvironment

    new Simulator(ledger, env, players, 10).run()
  }
}
