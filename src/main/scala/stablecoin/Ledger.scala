package stablecoin

import stablecoin.Ledger.CoinType.{BaseCoin, ReserveCoin, StableCoin}

import scala.collection.mutable.{Map => MMap}
import scala.util.Try

class Ledger(initBaseCoinAccounts: Map[Address, N],
             initStableCoinAccounts: Map[Address, N],
             initReserveCoinAccounts: Map[Address, N],
             val stablecoinContract: MinimalDjedStablecoin) {

  private val baseCoinAccounts = MMap[Address, N]().addAll(initBaseCoinAccounts)
  private val stableCoinAccounts = MMap[Address, N]().addAll(initStableCoinAccounts)
  private val reserveCoinAccounts = MMap[Address, N]().addAll(initReserveCoinAccounts)
  private var transactionsHistory = List[Transaction]()

  def getBasecoinAccounts = baseCoinAccounts.toMap
  def getStablecoinAccounts = stableCoinAccounts.toMap
  def getReservecoinAccounts = reserveCoinAccounts.toMap
  def getTransactionsHistory = transactionsHistory

  // execute tx and update the ledger state if execution succeeded
  def addTransaction(tx: Transaction): Try[Unit] = Try {
    tx match {
      case TransferTransaction(from, to, amount, coin) =>
        val accounts = coin match {
          case BaseCoin => baseCoinAccounts
          case StableCoin => stableCoinAccounts
          case ReserveCoin => reserveCoinAccounts
        }
        require(amount > 0 && accounts(from) >= amount)
        accounts(from) = (accounts(from) - amount)
        accounts(to) = amount + accounts.getOrElse(to,0.0)

      case BuyStablecoinTransaction(from, amountSC) =>
        val amountBaseToPay = stablecoinContract.calculateAmountBaseToPayForStablecoins(amountSC)
        require(amountSC > 0 && baseCoinAccounts(from) >= amountBaseToPay)
        if (stablecoinContract.buyStablecoin(amountSC).get != amountBaseToPay)
          ??? // If this happens something is wrong with the code
        baseCoinAccounts(from) -= amountBaseToPay
        stableCoinAccounts(from) = amountSC + stableCoinAccounts.getOrElse(from, 0.0)

      case SellStablecoinTransaction(from,amountSC) =>
        require(amountSC > 0 && stableCoinAccounts(from) >= amountSC)
        val amountBaseReturned = stablecoinContract.sellStablecoin(amountSC).get
        stableCoinAccounts(from) -= amountSC
        baseCoinAccounts(from) = amountBaseReturned + baseCoinAccounts.getOrElse(from,0.0)

      case BuyReservecoinTransaction(from,amountRC) =>
        val amountBaseToPay = stablecoinContract.calculateAmountBaseToPayForReservecoins(amountRC)
        require(amountRC > 0 && baseCoinAccounts(from) >= amountBaseToPay)
        if (stablecoinContract.buyReservecoin(amountRC).get != amountBaseToPay)
          assert(false, "Expected amount is not equal to the actual. Something is wrong with the code!!!") // Should never happen
        baseCoinAccounts(from) -= amountBaseToPay
        reserveCoinAccounts(from) = amountRC + reserveCoinAccounts.getOrElse(from,0.0)

      case SellReservecoinTransaction(from,amountRC) =>
        require(amountRC > 0 && reserveCoinAccounts(from) >= amountRC)
        val amountBaseReturned = stablecoinContract.sellReservecoin(amountRC).get
        reserveCoinAccounts(from) -= amountRC
        baseCoinAccounts(from) = amountBaseReturned + baseCoinAccounts.getOrElse(from,0.0)
    }

    // if we reached this point tx had been successfully applied
    transactionsHistory = transactionsHistory :+ tx
  }
}

object Ledger {
  object CoinType extends Enumeration {
    type CoinType = Value

    val BaseCoin = Value("BaseCoin")
    val StableCoin = Value("StableCoin")
    val ReserveCoin = Value("ReserveCoin")
  }
}