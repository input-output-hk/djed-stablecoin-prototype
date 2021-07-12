package simulator

import stablecoin.Currency.{BaseCoin, PegCurrency}
import stablecoin.{Address, BuyStablecoinTransaction, Ledger, SellStablecoinTransaction, Transaction}

trait Player {

  def address: Address

  /**
   * At the beginning of each round, each player is provided with an updated ledger and is asked to provide
   * a set of transactions he wants to submit
   * @param ledger current state of the system
   * @return set of transactions a player wants to execute
   */
  def newRoundCallback(ledger: Ledger, round: Int): Iterable[Transaction]
}

class StablecoinBuyer(val address: Address) extends Player {
  override def newRoundCallback(ledger: Ledger, round: Int): Iterable[Transaction] = {
    val myBaseCoins = ledger.getBasecoinAccounts(address)
    // buy stablecoins for 5% of available basecoins
    val amountSC = myBaseCoins * 0.05 / ledger.stablecoinContract.oracle.conversionRate(PegCurrency, BaseCoin)
    List(BuyStablecoinTransaction(address, amountSC))
  }
}

class StablecoinSeller(val address: Address) extends Player {
  override def newRoundCallback(ledger: Ledger, round: Int): Iterable[Transaction] = {
    val myStableCoins = ledger.getBasecoinAccounts(address)
    // sell 5% of stablecoins
    val amountSC = myStableCoins * 0.05
    List(SellStablecoinTransaction(address, amountSC))
  }
}
