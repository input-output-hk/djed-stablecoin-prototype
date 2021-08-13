package simulator

import com.typesafe.scalalogging.LazyLogging
import ledger.Ledger
import stablecoin.Currency.{BaseCoin, PegCurrency}
import stablecoin.Address

/**
 * An environment can arbitrarily change the ledger state at the beginning of each round
 * (e.g., update Oracle conversion rates or add new accounts or change amounts for existing accounts, etc.)
 */
trait Environment {
  /**
   * newRoundCallback is called by simulator at the beginning of each new round
   * @param ledger old ledger
   * @param round
   * @return new updated ledger
   */
  def newRoundCallback(ledger: Ledger, round: Int): Ledger
}

class ConstantEnvironment extends Environment {
  // just return ledger without any changes
  override def newRoundCallback(ledger: Ledger, round: Address): Ledger = ledger
}

class BullMarketEnvironment extends Environment with LazyLogging {

  override def newRoundCallback(ledger: Ledger, round: Address): Ledger = {
    val oldRate = ledger.stablecoinContract.oracle.conversionRate(PegCurrency, BaseCoin)
    val newRate = oldRate * 0.99 // make BaseCoin 1% more expensive
    ledger.stablecoinContract.oracle.updateConversionRate(PegCurrency, BaseCoin, newRate)
    logger.trace("Updated conversion rate: " + PegCurrency + " -> " + BaseCoin + ": " + oldRate + "->" + newRate)
    ledger
  }
}
